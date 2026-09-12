#!/usr/bin/env julia
# ============================================================
# sln_builder_Digel.jl
# Converts Digel occurrence data, species info, and links into 
#   SLN data: adjacency matrix and speciesinfo table
# Usage example:
#   julia --project=. scripts/sln_builder_Digel.jl
#
# Options:
#   --in-dir DIR    folder holding digel SpeciesInfo, 
#                   SpeciesOccurrencesData, MetawebInteractions  
#                   (default: data/digel)
#   --out-dir DIR      output file path (default: SLNs/DigelSoil)
# ============================================================

# load necessary Julia libraries
# install new ones using Pkg
# Pkg.add("CSV")
# can use saved environment in terminal using julia --project=. 
using CSV,DelimitedFiles,DataFrames,Random,Distributions,LinearAlgebra,Graphs,FilePathsBase,Tables

function opt_val(flag::String, default = nothing)
    i = findfirst(==(flag), ARGS)
    (i === nothing || i == length(ARGS)) ? default : ARGS[i + 1]
end

opt_flag(flag::String) = flag in ARGS

function parse_args()
    in_dir  = opt_val("--in-dir",  "data/digel_soil")
    out_dir = opt_val("--out-dir", "SLNs/digel_soil/raw")
    create  = opt_flag("--create")

    isdir(in_dir) || error("--in-dir is not a directory: $in_dir")

    if !isdir(out_dir)
        create || error("Output folder does not exist: $out_dir\n" *
                        "  Pass --create to make it.")
        mkpath(out_dir)
        println("Created output folder: ", out_dir)
    end

    if !isfile(joinpath(in_dir, "SpeciesInfo.csv"))
        error("SpeciesInfo.csv not present in input directory.")
    end 

    if !isfile(joinpath(in_dir, "SpeciesOccurrencesData.csv"))
        error("SpeciesOccurrencesData.csv not present in input directory.")
    end 

    if !isfile(joinpath(in_dir, "MetawebInteractions.csv"))
        error("MetawebInteractions.csv not present in input directory.")
    end 

    return (in_dir = in_dir, out_dir = out_dir)
end

# get the species present from a given plot as a vector of Ints
function get_present_species(spp_list_plots_df, plot_id)
    row_index = findfirst(==(plot_id), spp_list_plots_df.plotid)
    row_data = spp_list_plots_df[row_index, Not(:plotid)]
    return [parse(Int, String(name)) for (name, val) in pairs(row_data) if val == 1]
end

function main(in_dir::String, out_dir::String)

    # read in raw species occurrence data and filter out rows without any observed occurrences
    spp_occ_raw = CSV.read(joinpath(in_dir, "SpeciesOccurrencesData.csv"),DataFrame)
    spp_occ_filtered = filter(:foundOnPlot => n -> n == 1, spp_occ_raw)

    # store the names of the soil plots (sites)
    plots = unique(spp_occ_filtered.plotid)
    # store the species IDs in ascending order
    species_ids = sort(unique(spp_occ_filtered.species_id))

    # make an empty dataframe to store the species list for each plot
    spp_list_plots_matrix = fill(0, length(plots), maximum(species_ids))
    spp_list_plots = DataFrame(spp_list_plots_matrix, Symbol.(1:size(spp_list_plots_matrix,2))) # convert to DataFrame
    spp_list_plots = hcat(DataFrame(plotid = plots), spp_list_plots) # add plots and rowlabels

    # create lookup Dicts for fast indexing
    plot_lookup = Dict(p => i for (i, p) in enumerate(plots))
    species_lookup = Dict(string(s) => Symbol(string(s)) for s in species_ids)

    # loop through spp_occ_filtered and update matrix
    for row in eachrow(spp_occ_filtered)
        r = plot_lookup[row.plotid]
        c = species_lookup[string(row.species_id)]
        spp_list_plots[r, c] = 1
    end

    # import global list of feeding interactions and filter observed interactions
    # in this dataset, for each interaction species 1 is the prey/resource and species 2 is the predator/consumer
    metaweb_raw = CSV.read(joinpath(in_dir, "MetawebInteractions.csv"), DataFrame)
    metaweb_filtered = filter(:feeding_interaction => n -> n == 1, metaweb_raw)

    # make a Dict to store the list of interactions for each plot
    plot_links = Dict{String, DataFrame}()
    # and to store dropped species for reference
    plot_present = Dict{String, Vector{Int}}()
    drop_summary = DataFrame(plotid = String[], n_present = Int[], n_linked = Int[],
                             n_dropped = Int[], dropped_guilds = String[])

    # loop through all the plots and create the list of interactions for species present at each, and store them in plot_links
    for plot in eachrow(spp_list_plots)
        # Create an empty DataFrame for this plot
        df = DataFrame(prey = Int[], predator = Int[])

        # # Find species present on this plot
        present_species = get_present_species(spp_list_plots, plot.plotid)
        println("Species count in $(plot.plotid): ", length(present_species)) 
        plot_present[plot.plotid] = present_species # keep the Vector for the summary
        present_set = Set(present_species)    # hash lookup for the inner loop

        # Loop through all potential interactions
        for row in eachrow(metaweb_filtered)
            sp1 = row.spec1
            sp2 = row.spec2

            # If both species are present 
            if sp1 in present_set && sp2 in present_set
                push!(df, (prey = sp1, predator = sp2))
            end
        end

        # Store the resulting link table using plot name as key
        plot_links[plot.plotid] = df
    end

    # read in the taxa info document and rename columns to match my speciesinfo format
    species_master = CSV.read(joinpath(in_dir, "SpeciesInfo.csv"), DataFrame; missingstring="NA")
    rename!(species_master, Dict(
        :number => :sp_id,
        :species => :sp_name,
        :group => :guild
    ))

    # big loop to create matrix and speciesinfo for each plot
    for plot in sort(collect(keys(plot_links)))
        links_df = plot_links[plot]        
        # get local species list and diversity
        local_species = sort(unique(vcat(links_df.prey, links_df.predator)))
        local_index = Dict(id => i for (i, id) in enumerate(local_species))
        S = length(local_species)
        # Species observed on this plot but with no feeding link to any other
        # species present. Excluded from the web, so S is "linked species",
        # not "observed species".
        dropped = setdiff(plot_present[plot], local_species)
        if !isempty(dropped)
            dropped_info = semijoin(species_master, DataFrame(sp_id = dropped); on = :sp_id)
            push!(drop_summary, (plotid = plot,
                                n_present = length(plot_present[plot]),
                                n_linked  = S,
                                n_dropped = length(dropped),
                                dropped_guilds = join(sort(unique(string.(dropped_info.guild))), ";")))
        end
        
        # Initialize empty matrix: rows = consumers, cols = resources
        A = zeros(Int, S, S)

        for row in eachrow(links_df)
            predator = row.predator
            prey = row.prey

            r = local_index[predator]  # row = consumer
            c = local_index[prey]      # col = resource

            A[r, c] = 1
        end

        # Save matrix to CSV (no headers or row names)
        CSV.write(joinpath(out_dir, "matrix_$plot.csv"), Tables.table(A); writeheader=false)
        
        # filter species info by plot
        local_info = semijoin(species_master, DataFrame(sp_id = local_species); on=:sp_id)
        sort!(local_info, :sp_id)

        ### check for NA/invalid taxa in key fields
        key_cols = [:sp_id, :sp_name, :guild, :animal]
        missing_rows = filter(row -> any(ismissing, row[key_cols]), local_info)
        if nrow(missing_rows) > 0
            @warn "$plot includes $(nrow(missing_rows)) taxa with missing info: $(missing_rows.sp_id)"
        end

        # output species info file
        select!(local_info, [:sp_name, :guild, :family, :sp_id])
        CSV.write(joinpath(out_dir, "speciesinfo_$plot.csv"), local_info)
        
    end

    CSV.write(joinpath(out_dir, "dropped_species_summary.csv"), drop_summary)
    return nothing
end 

if abspath(PROGRAM_FILE) == @__FILE__
    a = parse_args()
    println("sln_builder_Digel.jl")
    println("  in-dir:  ", a.in_dir)
    println("  out-dir: ", a.out_dir)
    main(a.in_dir, a.out_dir)
end