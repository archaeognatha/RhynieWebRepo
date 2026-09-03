# load necessary Julia libraries
# these must be installed via the Julia repl or terminal environment. Do so with the following commands
# using Pkg
# Pkg.add("CSV")
# can use saved environment in terminal using julia --project=. 
using CSV, DataFrames, FilePathsBase, Tables

# metrics computed on the unlumped web, so meaningless after lumping. These only
# appear in legacy speciesinfo files (written by the old WebMetrics
# notebook); speciesinfo files regenerated through the current pipeline won't have them.
const DERIVED_COLS = ["sp_no_prey", "sp_no_preds", "sp_ntp", "sp_long_chain",
                      "sp_out_closeness", "sp_in_closeness",
                      "guild_ntp", "guild_out_closeness", "guild_in_closeness"]

# Identifiers: always joined to a string, so the column has one type
# whether or not a given trophospecies lumped anything.
const ID_COLS = ["sp_id", "original_sp_id", "prev_id"]

# Helper function for dealing with values in lumped trophospecies
# All values identical -> that value, type preserved.
# Otherwise -> semicolon-joined string.
function collapse_values(x)
    vals = collect(skipmissing(x))
    isempty(vals) && return missing
    u = unique(vals)
    length(u) == 1 ? u[1] : join(string.(u), ";")
end

join_values(x) = join(string.(collect(skipmissing(x))), ";")

function lump_trophospecies(input_dir::String, output_dir::String)
    # Create output directory if needed
    isdir(output_dir) || mkpath(output_dir)

    # Find input files
    matrix_files = sort(filter(f -> occursin(r"matrix_.*\.csv", f), readdir(input_dir; join=true)))
    info_files = sort(filter(f -> occursin(r"speciesinfo_.*\.csv", f), readdir(input_dir; join=true)))

    # create dataframe to store how many species were lumped in each web and whether any are from different guilds
    lumping_summary = DataFrame(web_id = String[], n_lumped = Int[], n_cross_guild = Int[])

    n_SLNs = length(matrix_files)

    #### the main loop begins below ####
    for index in 1:n_SLNs
        ### File input ###
        matrix_path = matrix_files[index]
        # Extract the unique web ID (e.g., "messel", "103") from the matrix filename
        ## Assumes filename format: "matrix_NAME.csv"
        name = match(r"matrix_(.*)\.csv", basename(matrix_path)).captures[1]
        # Find the corresponding species info file
        ## We look for "speciesinfo_WEBID.csv" in the info_files list
        expected_info_name = "speciesinfo_$(name).csv"
        matching_info = filter(f -> basename(f) == expected_info_name, info_files)
        if isempty(matching_info)
            println("SKIPPING $name: Could not find $expected_info_name")
            continue
        end
        info_path = matching_info[1]
        println("Processing $name...")
        println("   Matrix: ", basename(matrix_path))
        println("   Info:   ", basename(info_path))

        ### 1. Load data ###
        A_df = CSV.read(matrix_path, DataFrame; header=false)
        A = Matrix(A_df)
        species_df = CSV.read(info_path, DataFrame; missingstring="NA")
        n = size(A, 1)

        ### 2. Get predator and prey sets for each species ###
        preds = [findall(A[:, i] .== 1) for i in 1:n]  # incoming links
        prey  = [findall(A[i, :] .== 1) for i in 1:n]  # outgoing links

        ### 3. Group species by identical (preds, prey) sets ###
        pair_hashes = Dict{Tuple{Vector{Int}, Vector{Int}}, Vector{Int}}()
        for i in 1:n
            key = (preds[i], prey[i])
            haskey(pair_hashes, key) ? push!(pair_hashes[key], i) : (pair_hashes[key] = [i])
        end

        groups = collect(values(pair_hashes))  # each group is a list of sp_ids with identical feeding links

        ### 4. Assign trophospecies IDs ###
        lumped_ids = [minimum(g) for g in groups]  # representative ID for each group (the smallest species ID)
        id_to_tropho = Dict{Int, Int}()
        for (ts_id, group) in zip(lumped_ids, groups)
            for sp_id in group
                id_to_tropho[sp_id] = ts_id
            end
        end

        species_df.trophospecies_id = [id_to_tropho[i] for i in 1:n]

        ### 5. Build reduced matrix ###
        unique_ts = sort(unique(values(id_to_tropho)))
        ts_index = Dict(id => i for (i, id) in enumerate(unique_ts))
        m = length(unique_ts)
        A_reduced = zeros(Int, m, m)

        for i in 1:n, j in 1:n
            if A[i, j] == 1
                from = ts_index[id_to_tropho[i]]
                to   = ts_index[id_to_tropho[j]]
                A_reduced[from, to] = 1
            end
        end

        ### 6. Aggregate species info by trophospecies ###
        keep = setdiff(names(species_df), vcat(DERIVED_COLS, ["trophospecies_id"]))

        transforms = Any[nrow => :n_lumped]
        for col in keep
            f = col in ID_COLS ? join_values : collapse_values
            push!(transforms, col => f => col)
        end

        species_grouped = combine(groupby(species_df, :trophospecies_id), transforms...)

        # Count number of lumped taxa (i.e., groups with more than one original species)
        n_lumped_taxa = sum(species_grouped.n_lumped .> 1)
        
        # Count + flag trophospecies that span more than one guild. Not an error:
        # it means two species from different guilds ended up with identical
        # links in this web. 
        n_cross_guild = 0
        if "guild" in names(species_df)
            cross = combine(groupby(species_df, :trophospecies_id),
                            :guild => (x -> length(unique(skipmissing(x))) > 1) => :multi)
            n_cross_guild = sum(cross.multi)
            n_cross_guild > 0 && @info "Trophospecies spanning multiple guilds" web=name n=n_cross_guild
        end

        # Add to lumping summary table
        push!(lumping_summary, (web_id = name, n_lumped = n_lumped_taxa,
                        n_cross_guild = n_cross_guild))

        ### 7. Write output ###
        CSV.write(joinpath(output_dir, "matrix_$name.csv"), Tables.table(A_reduced); writeheader=false)
        CSV.write(joinpath(output_dir, "speciesinfo_$name.csv"), species_grouped)
        println("Wrote lumped matrix to: ", joinpath(output_dir, "matrix_$name.csv"))
    end
    CSV.write(joinpath(output_dir, "lumping_summary.csv"), lumping_summary)
end

function opt_val(flag::String, default = nothing)
    i = findfirst(==(flag), ARGS)
    (i === nothing || i == length(ARGS)) ? default : ARGS[i + 1]
end

if abspath(PROGRAM_FILE) == @__FILE__
    in_dir  = opt_val("--in-dir")
    out_dir = opt_val("--out-dir")

    in_dir  === nothing && error("--in-dir is required")
    out_dir === nothing && error("--out-dir is required")
    isdir(in_dir)       || error("--in-dir is not a directory: $in_dir")

    println("troph_sp_lumper.jl")
    println("  in-dir:  ", in_dir)
    println("  out-dir: ", out_dir)
    lump_trophospecies(in_dir, out_dir)
end