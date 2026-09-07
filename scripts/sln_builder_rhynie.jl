# ============================================================
# sln_builder_rhynie.jl
#
# Adapted from scripts by P. Roopnarine
#
# Usage:
#   julia --project=. scripts/sln_builder_rhynie.jl \
#       --in-dir  data/rhynie/rhynie_unlumped_complete \
#       --out-dir SLNs/rhynie_unlumped_complete \
#       --n-reps 1000 --gamma 3 --seed 20260906 --create
#
# Reads  <in-dir>/guilds.csv and <in-dir>/guild_matrix.csv
#        (one variant folder written by metaweb_builder_rhynie.R)
# Writes <out-dir>/matrix_<rep>.csv
#        <out-dir>/speciesinfo_<rep>.csv
#        <out-dir>/sln_params.csv     one row recording this run
#
# ============================================================

using CSV, DelimitedFiles, DataFrames, Random

include(joinpath(@__DIR__, "SLN_maker.jl"))
include(joinpath(@__DIR__, "r_no_prey.jl"))

# Peter's max-entropy draw is optional: only needed for --k-model max_ent.
const MAX_ENT_FILE = joinpath(@__DIR__, "max_ent_no_prey.jl")
isfile(MAX_ENT_FILE) && include(MAX_ENT_FILE)


# ------------------------------------------------------------
# Argument parsing - same helpers as web_metrics.jl
# ------------------------------------------------------------

function opt_val(flag::String, default = nothing)
    i = findfirst(==(flag), ARGS)
    (i === nothing || i == length(ARGS)) ? default : ARGS[i + 1]
end

opt_flag(flag::String) = flag in ARGS

const K_MODELS = ("prk", "max_ent", "pfim", "uniform")

function parse_args()
    in_dir  = opt_val("--in-dir")
    in_dir  === nothing && error("--in-dir is required")
    isdir(in_dir) || error("--in-dir is not a directory: $in_dir")

    guilds_file = joinpath(in_dir, "guilds.csv")
    matrix_file = joinpath(in_dir, "guild_matrix.csv")
    isfile(guilds_file) || error("Missing: $guilds_file")
    isfile(matrix_file) || error("Missing: $matrix_file")

    k_model = opt_val("--k-model", "prk")
    k_model in K_MODELS || error("--k-model must be one of $(K_MODELS); entered $k_model")
    k_model == "max_ent" && !isfile(MAX_ENT_FILE) &&
        error("--k-model max_ent needs $(MAX_ENT_FILE)")
    
    gamma_str = opt_val("--gamma", "3")
    if k_model != "prk" && "--gamma" in ARGS
        @warn "--gamma is ignored when --k-model is $k_model"
    end
    γ = parse(Float64, gamma_str)

    out_dir = opt_val("--out-dir")
    defaulted = out_dir === nothing
    if defaulted
        stem = basename(in_dir)
        k_model == "prk"   || (stem *= "_$(k_model)")
        γ == 3             || (stem *= "_g$(gamma_str)")
        out_dir = joinpath("SLNs", stem)
    end

    if !isdir(out_dir)
        (defaulted || opt_flag("--create")) || error("Output folder does not exist: $out_dir\n" *
                                                     " Pass --create to make it.")
        mkpath(out_dir)
        println("Created output folder: ", out_dir)
    end

    seed_str = opt_val("--seed")
    seed = seed_str === nothing ? nothing : parse(Int, seed_str)

    return (
        in_dir      = in_dir,
        out_dir     = out_dir,
        guilds_file = guilds_file,
        matrix_file = matrix_file,
        n_reps      = parse(Int, opt_val("--n-reps", "1000")),
        k_model     = k_model,
        γ           = γ,
        seed        = seed,
    )
end

# ------------------------------------------------------------
# In-degree draw: one function, four models
#
#   prk      mixed power law-exponential, parameter γ  (primary method)
#   max_ent  maximum-entropy draw, no parameter
#   pfim     every potential prey is eaten (in-degree = M)
#   uniform  in-degree drawn uniformly from 1:M
#
# M = the species' number of potential prey (guild_no_prey).
# ------------------------------------------------------------

function draw_no_prey(M::Integer, k_model::String, γ::Real)
    M == 0 && return 0
    if k_model == "prk"
        return Int(r_no_prey(M, γ))
    elseif k_model == "max_ent"
        return Int(max_ent_no_prey(M))
    elseif k_model == "pfim"
        return Int(M)
    elseif k_model == "uniform"
        return rand(1:M)
    end
    error("unknown k_model: $k_model")
end


# ------------------------------------------------------------
# Input checks - fail before the loop, not inside it
# ------------------------------------------------------------

const REQUIRED_GUILD_COLS = [:guild_name, :G, :terr, :aqu]

function load_metaweb(guilds_file::String, matrix_file::String)
    P = CSV.read(guilds_file, DataFrame)
    A = readdlm(matrix_file, ',', Int8)

    missing_cols = setdiff(REQUIRED_GUILD_COLS, propertynames(P))
    isempty(missing_cols) || error("guilds.csv is missing columns: $missing_cols")

    ng = nrow(P)
    size(A) == (ng, ng) || error("guild_matrix.csv is $(size(A)) but guilds.csv has $ng rows")
    all(x -> x in (0, 1, 2), A) || error("guild_matrix.csv contains values other than 0/1/2")

    return P, A
end


# ------------------------------------------------------------
# main
# ------------------------------------------------------------

function main(a)
    a.seed === nothing || Random.seed!(a.seed)

    P, A = load_metaweb(a.guilds_file, a.matrix_file)

    no_guilds  = nrow(P)
    no_species = sum(P[:, :G])
    println("Metaweb: $no_guilds guilds, $no_species species")
    println("In-degree model: $(a.k_model)" * (a.k_model == "prk" ? ", gamma = $(a.γ)" : ""))

    # construct guild x species array
    meta_SLN = SLN_maker(A, P, no_guilds, no_species)

    # calculate no. of prey species per guild
    P[:, :no_prey]  .= 0.0
    P[:, :no_preds] .= 0.0
    for i = 1:no_guilds
        for j = 1:no_guilds
            if A[i, j] != 0
                P[i, :no_prey] = P[i, :no_prey] + P[j, :G]
            end
            if A[j, i] != 0
                P[i, :no_preds] = P[i, :no_preds] + P[j, :G]
            end
        end
    end

    # ========================================================
    # assign resources (prey) and fill species-level matrix
    # ========================================================
    
    # Make empty dataframe for species data
    template = DataFrame(sp_name = Int64[], guild = String[], guild_no = Int64[], guild_richness = Int64[], guild_no_prey = Int64[], guild_no_preds = Int64[], terr = Int64[], aqu = Int64[], sp_no_prey = Int64[], sp_no_preds = Int64[])

    # Push guild data
    begin
        tally1 = [1]
        for i = 1:no_guilds
            guild_richness = P[i,:G]
            for j = 1:guild_richness
                push!(template, [tally1[1], P[i,:guild_name], i, P[i,:G], P[i,:no_prey], P[i,:no_preds], P[i,:terr], P[i,:aqu],0,0])
                tally1[1] = tally1[1] + 1
            end
        end
    end
    
    for rep in 1:a.n_reps                                          
        species = copy(template)

        # Initial species no. of prey; uses in-degree distribution
        for i = 1:no_species
            species[i,:sp_no_prey] = draw_no_prey(species[i,:guild_no_prey], a.k_model, a.γ)
        end

        # Select species-specific prey and generate species A matrix
        sp_A = zeros(Int64,no_species,no_species)
        for i = 1:no_species
            current_species_guild = species[i, :guild_no] # track guild ID of current sp.
            N = species[i, :sp_no_prey] # current sp. dietary breadth

            # Identify potential prey species, categorized by priority
            potential_prey_all = Int64[] # all potential prey (1's + 2's)
            priority_prey = Int64[]  # priority 2 prey species
            priority_guilds = Int64[] # unique priority guilds
            species_to_guild_map = species[!, :guild_no] # map species index to guild index

            for j = 1:no_species
                interaction_type = meta_SLN[current_species_guild, j]
                if interaction_type > 0
                    push!(potential_prey_all, j)
                end

                if interaction_type == 2
                    push!(priority_prey, j)

                    # Add the guild of this prey species to our priority guild list if unique
                    prey_guild = species_to_guild_map[j]
                    if !(prey_guild in priority_guilds)
                        push!(priority_guilds, prey_guild)
                    end
                end
            end

            # Initialize final prey list and slot counter
            final_prey = Int64[]
            prey_slots_filled = 0

            # Fill with priority prey -- one from each priority guild
            if N > 0 && length(priority_guilds) > 0
                shuffle!(priority_guilds) # randomize order of priority guilds to draw from

                for p_guild in priority_guilds
                    if prey_slots_filled >= N
                        break # stop if prey slots are filled
                    end
                    # Find all species in this priority guild
                    available_species_in_guild = Int64[]
                    for sp_index in priority_prey
                        if species_to_guild_map[sp_index] == p_guild
                            push!(available_species_in_guild, sp_index)
                        end
                    end
                    # If there are available species, pick one at random
                    if length(available_species_in_guild) > 0
                        chosen_prey = rand(available_species_in_guild)
                        push!(final_prey, chosen_prey)
                        prey_slots_filled += 1
                    end
                end
            end

            # Fill remaining prey slots with random potential prey
            slots_to_fill = N - prey_slots_filled
            if slots_to_fill > 0 && length(potential_prey_all) > 0
                # Create a list of available prey
                available_prey = Int64[]
                for sp_index in potential_prey_all
                    if !(sp_index in final_prey)
                        push!(available_prey, sp_index)
                    end
                end
                shuffle!(available_prey) # shuffle the remaining available prey

                num_to_add = min(slots_to_fill, length(available_prey))

                if num_to_add > 0
                    for k = 1:num_to_add
                        push!(final_prey, available_prey[k])
                    end
                end
            end

            # Update the adjacency matrix
            for prey_species_index in final_prey
                sp_A[i, prey_species_index] = 1
            end
        end

        #calculate no. preds, or out-degree
        for i = 1:no_species
            out_degree = 0
            for j = 1:no_species
                if sp_A[j,i] == 1
                    out_degree += 1
                end
            end
            species[i,:sp_no_preds] = out_degree
        end

        writedlm(joinpath(a.out_dir, "matrix_$rep.csv"), sp_A, ',')          
        CSV.write(joinpath(a.out_dir, "speciesinfo_$rep.csv"), species)     

        rep % 100 == 0 && println("  wrote $rep / $(a.n_reps)")             
    end
    # ========================================================
    
    # -- record run parameters alongside the output
    CSV.write(joinpath(a.out_dir, "sln_params.csv"), DataFrame(
        in_dir = a.in_dir, k_model = a.k_model, gamma = a.γ,
        n_reps = a.n_reps, seed = string(a.seed),
        no_guilds = no_guilds, no_species = no_species,
    ))
    
    println("Done: $(a.n_reps) SLNs written to $(a.out_dir)")
    return nothing
end


# ------------------------------------------------------------
# Entry point - runs on `julia script.jl`, not on include()
# ------------------------------------------------------------

if abspath(PROGRAM_FILE) == @__FILE__
    a = parse_args()
    println("sln_builder_rhynie.jl")
    println("  in-dir:  ", a.in_dir)
    println("  out-dir: ", a.out_dir)
    main(a)
end
