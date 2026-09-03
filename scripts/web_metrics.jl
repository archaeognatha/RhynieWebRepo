#!/usr/bin/env julia
# ============================================================
# web_metrics.jl
# Adapted from "Analytical approaches to networks, trophic structure, 
#    and ancient food webs" NAPC 2024 food web workshop
# Computes network-level metrics for
#    every SLN in a folder, plus per-node metrics written alongside.
#
# Usage example:
#   julia --project=. scripts/web_metrics.jl --in-dir SLNs/Messel --name Messel
#
# Options:
#   --in-dir DIR    folder holding matrix_*.csv and speciesinfo_*.csv   (required)
#   --name STR      label used in the output filename                  (required)
#   --slns-root     DIR output root  (default: SLNs)
#   --out FILE      metrics table path   (default: SLNs/<name>/WebMetrics_<name>.csv)
#   --node-dir DIR  per-node output dir  (default: SLNs/<name>)
#   --create        create the output folder if it doesn't exist
#   --maxtime N     chain-search time limit per species, seconds (default: 10)
# ============================================================

using CSV, DelimitedFiles, DataFrames, Random, Distributions, StatsPlots,
      LinearAlgebra, PoissonRandom, Graphs, Colors, FilePathsBase

# @__DIR__ is the folder holding THIS script, so the includes resolve
# no matter which directory you launch julia from.
include(joinpath(@__DIR__, "loop_finder.jl"))
include(joinpath(@__DIR__, "max_chain_calcs.jl"))

# ------------------------------------------------------------
# Argument parsing
# ------------------------------------------------------------

function opt_val(flag::String, default = nothing)
    i = findfirst(==(flag), ARGS)
    (i === nothing || i == length(ARGS)) ? default : ARGS[i + 1]
end

opt_flag(flag::String) = flag in ARGS

function parse_args()
    in_dir = opt_val("--in-dir")
    name   = opt_val("--name")

    in_dir === nothing && error("--in-dir is required")
    name   === nothing && error("--name is required")
    isdir(in_dir)      || error("--in-dir is not a directory: $in_dir")

    slns_root = opt_val("--slns-root", "SLNs")
    out       = opt_val("--out",      joinpath(slns_root, name, "WebMetrics_$(name).csv"))
    node_dir  = opt_val("--node-dir", joinpath(slns_root, name))
    create    = opt_flag("--create")

    # Both outputs need their folder to exist. Missing folder is an error
    # by default, because the usual cause is a typo in --name: without this
    # guard a misspelling silently writes to a brand new directory.
    for d in unique([node_dir, dirname(out)])
        isempty(d) && continue          # bare filename, means current directory
        if !isdir(d)
            create || error("Output folder does not exist: $d\n" *
                            "  Check --name matches the folder under $slns_root,\n" *
                            "  or pass --create to make it.")
            mkpath(d)
            println("Created output folder: ", d)
        end
    end
    
  return (
        in_dir   = in_dir,
        name     = name,
        out      = out,
        node_dir = node_dir,
        maxtime  = parse(Float64, opt_val("--maxtime", "10")),
    )
end


# ------------------------------------------------------------
# main
#
# Everything from notebook cell 4 goes in here, with these changes:
#   dir_path       -> in_dir
#   analysis_name  -> name
#   line 390       -> CSV.write(joinpath(node_dir, "nodemetrics_$(web_id).csv"), sp_P)
#   line 403       -> CSV.write(out, SLN_stats_out)
#   chain_depths   -> pass maxtime through:
#                     chain_depths(sp_A, no_species, web_id, maxtime = maxtime)

# ------------------------------------------------------------

function main(in_dir::String, name::String, out::String,
              node_dir::String, maxtime::Real)


    # create an empty dataframe to populate with metric values for each web
    SLN_stats_out = DataFrame(SLN_ID = String[], Detritus = Int64[], S = Float64[], interactions = Float64[], L_D = Float64[], C = Float64[],
        Basal = Float64[], Top = Float64[], Herbiv_true = Float64[], Herbiv = Float64[], Carniv = Float64[],
        meanInDegree = Float64[], stdInDegree = Float64[], 
        mean_NTP = Float64[], max_NTP = Float64[], mean_NTP_norm = Float64[], 
        TrOmniv = Float64[], q_inCoherence = Float64[], 
        diameter = Float64[], max_chain_len = Float64[],
        mean_path_len = Float64[], std_path_len = Float64[],
        loop = Float64[], Modularity = Float64[]
    )
   
    # create an empty dataframe to populate with node-level metric values for each web
    node_out = DataFrame()

    # store the filenames of the matrix files from the specified directory 
    matrix_files = sort(filter(f -> occursin(r"matrix_.*\.csv", f), readdir(in_dir; join=true)))
    info_files = sort(filter(f -> occursin(r"speciesinfo_.*\.csv", f), readdir(in_dir; join=true)))

    n_SLNs = length(matrix_files)

    #### the main loop begins below ####
    for index in 1:n_SLNs
        ### File input ###
        matrix_path = matrix_files[index]
        # Extract the unique web ID (e.g., "messel", "103") from the matrix filename
        ## Assumes filename format: "matrix_WEBID.csv"
        web_id = match(r"matrix_(.*)\.csv", basename(matrix_path)).captures[1]
        # Find the corresponding species info file
        ## We look for "speciesinfo_WEBID.csv" in the info_files list
        expected_info_name = "speciesinfo_$(web_id).csv"
        matching_info = filter(f -> basename(f) == expected_info_name, info_files)
        if isempty(matching_info)
            println("SKIPPING $web_id: Could not find $expected_info_name")
            continue
        end
        info_path = matching_info[1]
        println("Processing $web_id...")
        println("   Matrix: ", basename(matrix_path))
        println("   Info:   ", basename(info_path))

        ## Read the species info and adjacency matrix files.
        sp_P = CSV.read(info_path, DataFrame; 
            pool=false, 
            normalizenames=true, 
            ntasks=1,
            stringtype=String
            # quotechar='^' # Add these back if you haven't fixed the quotes in the files yet
        )
        sp_A_df = CSV.read(matrix_path, DataFrame; header = false)
        sp_A = Matrix(sp_A_df)

        if nrow(sp_P) != size(sp_A, 1)
            error("DIMENSION MISMATCH for $web_id: Matrix has $(size(sp_A, 1)) rows, but Species Info has $(nrow(sp_P)) rows.")
        end

        # FORCE CONVERT guild column to strings
        sp_P.guild = string.(sp_P.guild)
        
        # add web_id to species info for differentiating networks in node-level datasheet
        sp_P[!, :web_id] .= web_id
        
        #------------------------------------#
        ### Basic stats ###

        ## no. of interactions 
        interactions = sum(sp_A)
        no_species = size(sp_A)[1]
        ## link density
        L_D = interactions/no_species
        ## connectance
        C = interactions/(no_species*(no_species-1))

        #------------------------------------#
        ### Check if no_preds and no_prey columns are missing/empty and fill in if so

        # Make sure sp_no_prey and sp_no_preds columns are mutable and allow missing type
        for colname in [:sp_no_prey, :sp_no_preds]
            if !( colname in names(sp_P) )
                sp_P[!, colname] = Vector{Union{Missing, Int}}(missing, nrow(sp_P))
            elseif !(eltype(sp_P[!, colname]) <: Union{Missing, Int})
                T = nonmissingtype(eltype(sp_P[!, colname]))
                sp_P[!, colname] = Vector{Union{Missing, T}}(sp_P[!, colname])
            end
        end

        # Fill in missing predator/prey counts
        for i in 1:no_species
            if ismissing(sp_P.sp_no_prey[i])
                sp_P.sp_no_prey[i] = sum(sp_A[i, :])  # Row = outgoing links (prey)
            end
            if ismissing(sp_P.sp_no_preds[i])
                sp_P.sp_no_preds[i] = sum(sp_A[:, i])  # Col = incoming links (predators)
            end
        end

        #------------------------------------#
        ### Trophic Composition ###

        ## Basal: fraction of total species (minus detritus) that eat only basal species

        # Identify basal species (no incoming links)
        is_basal = [sum(sp_A[i, j] for j in 1:no_species) == 0 for i in 1:no_species]
        # Identify how many of the taxa are detritus and reports how many detrital nodes are reported in the web
        detritalnodes = findall(occursin.("detritus", sp_P.guild))
        Detritus = length(detritalnodes)

        Basal = (sum(is_basal)-length(detritalnodes))/(no_species-length(detritalnodes))

        ## Top: fraction of total species (minus detritus) with no predators
        is_top = [sum(sp_A'[i, j] for j in 1:no_species) == 0 for i in 1:no_species]

        Top = sum(is_top)/no_species

        ## Herbivores, Carnivores: fraction of consumer species that eat only basal species, only non-basal species

        herbivores = 0
        carnivores = 0
        consumers = 0

        ## herbivores_true: consumers that eat only basal species that are not detritus
        herbivores_true = 0

        for i in 1:no_species
            prey = findall(sp_A[i, :] .== 1)
            if !isempty(prey)
                consumers += 1
                if all(is_basal[j] for j in prey)
                    herbivores += 1
                    # "For all items 'p' in 'prey', check that 'p' is NOT in 'detritalnodes'"
                    if all(p -> !(p in detritalnodes), prey) 
                        herbivores_true +=1
                    end
                end
                if all(!is_basal[j] for j in prey)
                    carnivores += 1
                end
            end
        end

        Herbiv = herbivores/consumers
        Carniv = carnivores/consumers
        
        Herbiv_true = herbivores_true/consumers
        #------------------------------------#
        ## Mean and st. dev. in-degree (generality), mean # of prey species

        meanInDegree = sum(sp_P.sp_no_prey)/consumers
        stdInDegree = std(sp_P.sp_no_prey[sp_P.sp_no_prey .!= 0])

        #------------------------------------#
        ### Chain length/NTP analyses ###

        # store number of guilds for later use
        # no_guilds = maximum(sp_P.guild_no)
        # add columns to the species dataframe to store the longest chain and ntp
        sp_P[!, :sp_ntp] = fill(0.0, nrow(sp_P))
        sp_P[!, :sp_long_chain] = fill(0.0, nrow(sp_P))

        #initialize pathways matrix
        paths = Array{Int64}(undef,no_species,no_species)
        paths = deepcopy(sp_A)
        #longest possible pathway. for rhynie this is the number of guilds, but here is generalized for networks not drawn from a guild metaweb
        P_max = min(no_species - 1, 30) # max of 30 to avoid computational overload in large food webs
        #set initial longest path for each species
        for i = 1:no_species
            if sp_P[i,:sp_no_prey] > 0
                sp_P[i,:sp_long_chain] = 1
            end
        end
            
        #calculate pathways by raising binary adjacency matrix to pathway lengths
        for i = 1:P_max
            A2 = sp_A^i
            for j = 1:no_species
                for k = 1:no_species
                    #if path now exists between species
                    if paths[j,k]==0 && A2[j,k]>0
                        #update the pathways matrix
                        paths[j,k] = i
                    end
                end
                #list as longest chain if one exists
                if sum(paths[j,:]) != 0
                    sp_P[j,:sp_long_chain] = maximum(paths[j,:])
                end
            end       
            if sum(A2)==0
                break
            end
        end
        
        ## Closeness Centrality    
        ### OUTBOUND closeness (how accessible resources are to consumers) -- probably more useful
        ## new column in sp_P to record Outbound closeness centrality
        sp_P[!, :sp_out_closeness] = fill(0.0, nrow(sp_P))

        for i in 1:no_species
            # Extract distances from species 'i' to everyone else
            dists = paths[:, i]
            
            # filter valid interactions
            # only sum paths that are > 0 (reachable) AND not to itself (index != i)
            valid_paths = [dists[k] for k in 1:no_species if dists[k] > 0 && k != i]
            
            # Sum the shortest pathways
            sum_paths = sum(valid_paths)
            
            # Apply the formula: (N-1) / Sum
            # We check if sum_paths > 0 to avoid dividing by zero (for species with no prey)
            if sum_paths > 0
                sp_P.sp_out_closeness[i] = (no_species - 1) / sum_paths
            else
                sp_P.sp_out_closeness[i] = 0.0
            end
        end
        ### INBOUND closeness (how connected consumers are to resources)
        ## new column in sp_P to record Inbound closeness centrality
        sp_P[!, :sp_in_closeness] = fill(0.0, nrow(sp_P))

        for i in 1:no_species
            # Extract distances to species 'i' from everyone else
            dists = paths[i, :]
            
            # filter valid interactions
            # only sum paths that are > 0 (reachable) AND not to itself (index != i)
            valid_paths = [dists[k] for k in 1:no_species if dists[k] > 0 && k != i]
            
            # Sum the shortest pathways
            sum_paths = sum(valid_paths)
            
            # Apply the formula: (N-1) / Sum
            # We check if sum_paths > 0 to avoid dividing by zero (for species with no prey)
            if sum_paths > 0
                sp_P.sp_in_closeness[i] = (no_species - 1) / sum_paths
            else
                sp_P.sp_in_closeness[i] = 0.0
            end
        end

        #calculate ntps
        #build vector of primary producers
        prods = Int64[]
        for i = 1:no_species
            if sp_P[i,:sp_no_prey] == 0
                push!(prods,i)
            end
        end

        #calculate path length of prey to producers
        for i = 1:no_species
            if sp_P[i,:sp_no_prey]==0 #if producer
                sp_P[i,:sp_ntp] = 1
                elseif sp_P[i,:sp_no_prey]>0 #else if consumer
                    #list prey
                    its_prey = Int64[]
                    path_length = 0
                    no_paths = 0
                    for j = 1:no_species
                        #if species is producer prey of i
                        if paths[i,j] == 1 && sp_P[j,:sp_no_prey] == 0
                            no_paths+=1
                        end
                        #if species is consumer prey of i
                        if paths[i,j] == 1 && sp_P[j,:sp_no_prey] > 0
                            #record path lengths to producers
                            for k = 1:no_species
                                if paths[j,k]!=0 && sp_P[k,:sp_no_prey]==0
                                    path_length = path_length + paths[j,k]
                                    no_paths+=1
                                end
                            end
                        end 
                    end
                    #if herbivore
                    if path_length==0
                        sp_P[i,:sp_ntp] = 2.0
                    elseif path_length > 0
                        #if not herbivore
                        sp_P[i,:sp_ntp] = 2.0 + (Float64(path_length)/Float64(no_paths))
                        #println(species[i,6])
                    end
                end
            end

        ## mean net trophic position (ntp)
        mean_NTP = mean(sp_P.sp_ntp)

        ## max and normalized mean ntp (divide by max value of ntp)
        max_NTP = maximum(sp_P.sp_ntp)
        mean_NTP_norm = mean_NTP / max_NTP

        ## Trophic Omnivory: fraction of consumer species that eat across trophic levels
        num_integers = count(x -> isfinite(x) && x % 1 == 0, sp_P.sp_ntp)
        trophic_omnivores = no_species - num_integers
        TrOmniv = trophic_omnivores/consumers

        #------------------------------------#
        ## Trophic Coherence ###

        ## incoherence (q) is a metric from Johnson et al. 2014 and is related to TrophOmniv
        ## it is the standard deviation of differences between ntps of predator and prey across for all links in the web

        # make a list of all edges (trophic links) in the SLN
        graph = DiGraph(sp_A)
        links = collect(edges(graph))

        # create an empty vector to store ntp differences for all edges 
        troph_distances = Float64[]

        # Loop through all edges and calculate ntp differences
        # note that the direction of source and destination seems backwards from an energy POV-->in graphs the standard direction is from the predator to the prey
        for e in edges(graph)
            i = src(e) # consumer sp ID
            j = dst(e) # resource sp ID
            ntp_i = sp_P[i, :sp_ntp]
            ntp_j = sp_P[j, :sp_ntp]
            push!(troph_distances, ntp_i - ntp_j)
        end

        # compute standard deviation
        q_inCoherence = std(troph_distances)

        #------------------------------------#
        ### Diameter ####

        # Compute all-pairs shortest paths
        all_paths = floyd_warshall_shortest_paths(SimpleDiGraph(sp_A))

        # Extract all finite path lengths
        lengths = Float64[]
        for i in 1:no_species
            for j in 1:no_species
                d = all_paths.dists[i, j]
                if i != j && (d) < 100000
                    push!(lengths, d)
                end
            end
        end
        
        diameter = maximum(lengths)
        mean_path_len = mean(lengths)
        std_path_len = std(lengths)

        #------------------------------------#
        ### Max chain length
        
        trophic_height = chain_depths(sp_A, no_species, web_id, maxtime = maxtime)
        max_chain_len = maximum(trophic_height)

        #------------------------------------#
        ### Loop ###
        
        # use custom function to return boolean list of species involved in trophic cycles
        spp_in_loops = nodes_in_loops(sp_A)

        # loop is the fraction of all species involved in non-cannibalistic cycles (e.g., Dunne et al. 2008)
        loop = sum(spp_in_loops)/no_species
        #------------------------------------#
        ### Guild-level metrics (ntp and closeness centralities) ###
        
        # create new columns filled with 0.0 (Float64)
        sp_P[!, :guild_ntp] = fill(0.0, nrow(sp_P))
        sp_P[!, :guild_out_closeness] = fill(0.0, nrow(sp_P))
        sp_P[!, :guild_in_closeness] = fill(0.0, nrow(sp_P))

        # get list of unique guilds
        guild_names = unique(sp_P.guild)

        for g in guild_names
            logical_mask = sp_P.guild .== g

            ntp_mean = mean(sp_P.sp_ntp[logical_mask])
            sp_P.guild_ntp[logical_mask] .= ntp_mean

            out_closeness_mean = mean(sp_P.sp_out_closeness[logical_mask])
            sp_P.guild_out_closeness[logical_mask] .= out_closeness_mean
            
            in_closeness_mean = mean(sp_P.sp_in_closeness[logical_mask])
            sp_P.guild_in_closeness[logical_mask] .= in_closeness_mean

        end
        
        #------------------------------------#
        ### Modularity ###


        
        #------------------------------------#
        #### Metrics Output ####

        # accumulate this web's node rows for the stacked node-level output
        append!(node_out, sp_P; cols = :union, promote = true)

        # push metrics to SLN_stats_out
        push!(SLN_stats_out, (web_id, Detritus, no_species, interactions, L_D, C, 
            Basal, Top, Herbiv_true, Herbiv, Carniv,
            meanInDegree, stdInDegree, mean_NTP, max_NTP, mean_NTP_norm,
            TrOmniv, q_inCoherence, diameter, max_chain_len,
            mean_path_len, std_path_len, loop, 0    
        ))  # Order must match column order
        println("Updated species info file #$(web_id) and pushed metrics to dataframe.")
    end

    # save SLN_stats_out as a csv
    CSV.write(out, SLN_stats_out)
    println("Successfully completed metrics calculations for $name and outputted table as .csv.")

    # save node_out as a csv
    CSV.write(joinpath(node_dir, "NodeMetrics_$(name).csv"), node_out)

    return nothing
end


# ------------------------------------------------------------
# Entry point
#
# PROGRAM_FILE is the script julia was launched with. This guard means
# main() runs when the file is executed, but NOT when it is include()d
# from the REPL - so you can load the file interactively to poke at the
# functions without kicking off a full run.
# ------------------------------------------------------------

if abspath(PROGRAM_FILE) == @__FILE__
    a = parse_args()
    println("web_metrics.jl")
    println("  in-dir:   ", a.in_dir)
    println("  name:     ", a.name)
    println("  out:      ", a.out)
    println("  node-dir: ", a.node_dir)
    println("  maxtime:  ", a.maxtime)
    main(a.in_dir, a.name, a.out, a.node_dir, a.maxtime)
end