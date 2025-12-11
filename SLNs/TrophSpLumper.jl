# load necessary Julia libraries
# these must be installed via the Julia repl or terminal environment. Do so with the following commands
# using Pkg
# Pkg.add("CSV")
using CSV, DataFrames, FilePathsBase

function lump_trophospecies(input_dir::String, output_dir::String)
    # Create output directory if needed
    isdir(output_dir) || mkpath(output_dir)

    # Find input files
    matrix_files = filter(f -> occursin(r"matrix_.*\.csv", f), readdir(input_dir; join=true))
    info_files = filter(f -> occursin(r"speciesinfo_.*\.csv", f), readdir(input_dir; join=true))

    # create dataframe to store how many species were lumped in each web
    lumping_summary = DataFrame(web_id = String[], n_lumped = Int[])

    for (matrix_file, info_file) in zip(matrix_files, info_files)
        name = match(r"matrix_(.*)\.csv", matrix_file).captures[1]

        ### 1. Load data ###
        A_df = CSV.read(matrix_file, DataFrame; header=false)
        A = Matrix(A_df)
        species_df = CSV.read(info_file, DataFrame; missingstring="NA")
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

        # Determine if optional sp_id column exists
        has_sp_id = :sp_id in names(species_df)

        # Build the list of transformations for grouping
        transforms = [
            :sp_name => (x -> join(skipmissing(x), ";")) => :sp_name,
            :guild => (x -> first(skipmissing(x))) => :guild,
            :sp_name => length => :n_lumped
        ]
        if has_sp_id
            push!(transforms, :sp_id => (x -> join(string.(x), ";")) => :sp_ids)
        end

        species_grouped = combine(groupby(species_df, :trophospecies_id), transforms...)

        # Count number of lumped taxa (i.e., groups with more than one original species)
        n_lumped_taxa = sum(species_grouped.n_lumped .> 1)
        # Add this to the lumping summary table (assuming web_id is defined for this web)
        push!(lumping_summary, (web_id = name, n_lumped = n_lumped_taxa))

        ### 7. Write output ###
        CSV.write(joinpath(output_dir, "matrix_$name.csv"), Tables.table(A_reduced); writeheader=false)
        CSV.write(joinpath(output_dir, "speciesinfo_$name.csv"), species_grouped)
        println("Wrote lumped matrix to: ", joinpath(output_dir, "matrix_$name.csv"))
    end
    CSV.write(joinpath(output_dir, "lumping_summary.csv"), lumping_summary)
end

lump_trophospecies("SLNs/DeRuiterSoil", "SLNs/DeRuiterSoil_TS")
