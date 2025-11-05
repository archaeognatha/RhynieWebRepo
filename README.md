Rhynie Chert paleo-food web project 
===================================
This project, part of Tanner Frank's PhD dissertation, aims to use a comprehensive list of described fossil taxa from the Early Devonian
Rhynie and Windyfield Cherts in Scotland to reconstruct an ancient terrestrial trophic network and compare it to available webs from
modern and fossil ecosystems.

Last updated: 10-04-2025

Repo Contents:
--------------
- **RhynieGuildStructure.xlsx**: spreadsheet defining metaweb guild structure, contains two sheets:
    - Guilds: lists trophic guilds along with basic info for each (similarly structured to guilds.csv).
    - Guild composition: lists species within each guild --> this is the only place where specific taxon names are referred to in the context of the analysis structure, as all members of a guild are treated as functional equivalents.
  
- **TaxaList.xlsx**: list of published taxa from Rhynie, Windyfield and a few other Devonian sites, with relevant references used for each. Only the Rhynie and Windyfield taxa are used in this project.
  
- **RhynieTaxaNotes.docx**: Word document containing notes about the trophic habits of some of the taxa in the rhynie web.

- **guilds.csv**: lists all guilds and information about each relevant to analyses. This file is an input into RhynieWebCode.R and SLNcode.ipynb scripts. All consumer guilds have "general" resources representing the guilds their members can potentially consume. Some also have "priority" resources; for each of these guilds, one interaction is preferentially assigned during the SLN generation process. The priority interactions represent those with greater certainty about their occurrence, and they often represent known species-specific interactions (e.g., a mycorrhizal association with a particular plant species) or cases where an important resource is represented by a single guild (e.g., detritivory)

- **links.csv**: list of all the guild-level consumer-resource links. This info is based on guilds.csv, and the file is an input into adj_matrix_builder_rhynie.R for generating guild_matrix.csv. Priority level 1 links are general resources, while priority level 2 links are equivalent to the priority resources in guilds.csv

- **adj_matrix_builder_rhynie.R**: R script used to generate the guild_matrix.csv adjacency matrix

- **guild_matrix.csv**: adjacency matrix containing feeding relationships of all the guilds in the metaweb. Rows represent consumer ID, columns represent resource ID. There is also **guild_matrix_transposed.csv**, which is the matrix but the rows and columns are switched. This file is an input into the SLNcode.ipynb and RhynieWebCode.R scripts

- **SLNcode.ipynb**: this is a julia script in a Jupyter notebook adapted from Peter Roopnarine's 2024 NAPC fossil food web workshop code. It takes in a guild-level metaweb (guild_matrix.csv) and species richness information (from guilds.csv) and generates a desired number of species-level networks (SLNs) that are randomized within the constraints of the metaweb structure. The script outputs two kinds of .csv files into a new directory within the directory /SLNs: " species-level adjacency matrices (matrix_X.csv), and corresponding species lists with guild assignments and basic info (speciesinfo_X.csv)
    - **SLN_maker.jl**: julia function used by SLNcode.ipynb, which constructs an empty array of appropriate size given the species richness of each guild
    - **r_no_prey.jl**: julia function used by SLNcode.ipynb, which determines the number of prey items for each species by drawing from a power law distribution

- **WebMetrics.ipynb**: another julia script in a Jupyter notebook adapted from Dr. Peter Roopnarine's code from the 2024 NAPC fossil food web workshop. This one takes folders containing adjacency matrix and taxa ID files (e.g., the output from SLNcode.ipynb) and calculates summary network metrics. It outputs a summary table where each row is a different SLN replicate and each column is a different metric. The metrics it calculates are:
    - Connectance
    - mean In-degree
    - mean Net Trophic Position (NTP)
    - max NTP
    - NTP per guild (optional for input webs with defined guild structure)
    - Trophic Coherence (Johnson et al. 2014)
    - Trophic Omnivory
    - mean/characteristic Path Length
    - Modularity 
    - Loops
    - mean Centrality

- **RhynieWebCode.R**: this is an R script that takes in a food web adjacency matrix and generates relevant statistics. It is adapted from Dr. Carrie Tyler's code used in the 2024 NAPC fossil food web workshop

-> **SLNs**: directory that stores species-level-networks for Rhynie and comparative datasets. Each SLN is stored as two .csv files: a matrix file containing the adjacency matrix of interactions, and a speciesinfo file containing taxa names and species-level metrics. For each folder, there is also a WebMetrics file with summary statistics calculated for each SLN, which is the output of **WebMetrics.ipynb**
    - **TrophSpLumper.jl**: julia script which defines and runs a function that collapses species with identical consumers and resources into trophospecies; the script inputs an SLN folder and outputs a new folder containing the new "lumped" networks
    - **NicheModelGenerator.R** R script that inputs the WebMetrics file for a folder of SLNs and uses to generate a desired number of networks with a comparable distribution of connectance and species richness values using the niche model

-> **Rhynie_lumped_data**: directory with alternative guild and guild-level matrix file for a taxonomically-lumped version of the Rhynie network

-> **DigelSoilWebs_rawdata**: directory with the original metaweb dataset for the Digel 2014 German soil study; unlike the guild-level Rhynie metaweb, this is a regional metaweb from which SLNs representing individual localities can be drawn. The jupyter notebook **SLNconvertDigelSoil.ipynb** performs this operation, outputting the resulting files into the >SLNs directory

-> **MetricsComparisons**: directory with files and scripts related to comparative statistics of the SLN metrics. Most notably, **CompleteMetrics.csv** is a manually-created combination of the WebMetrics results from the various SLNs calculated for different datasets. This datasheet is an input into the R scripts: **StatsComparisons.R** performs various exploratory statistics, **PlotMetrics.R** creates cleaner plots for comparing the data