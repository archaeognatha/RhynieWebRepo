Paleo-food web project for Rhynie Chert
=======================================
This project, part of Tanner Frank's PhD dissertation, aims to use a comprehensive list of described fossil taxa from the Early Devonian
Rhynie and Windyfield Cherts in Scotland to reconstruct an ancient terrestrial trophic network and compare it to available webs from
modern ecosystems.

Last updated: 06-16-2025

Repo Contents:
--------------
- **RhynieGuildStructure.xlsx**: spreadsheet defining metaweb guild structure, contains two sheets:
    - Guilds: lists trophic guilds along with basic info for each (similarly structured to guilds.csv)
    - Guild composition: lists species within each guild --> this is the only place where specific taxon names are referred to in the context of the analysis structure, as all members of a guild are functionally equivalent in the code
  
- **TaxaList.xlsx**: list of published taxa from Rhynie, Windyfield and a few other Devonian sites, with relevant references used for each. Only the Rhynie and Windyfield taxa are used in the other files in this project.
  
- **RhynieTaxaNotes.docx**: Word document containing notes about the trophic habits of some of the taxa in the rhynie web

- **guilds.csv**: Lists all guilds and relevant information for analyses. this file is an input into RhynieWebCode.R and SLNcode.ipynb scripts

- **guild_matrix.csv**: adjacency matrix containing feeding relationships of all the guilds in the metaweb. this file is an input into RhynieWebCode.R and SLNcode.ipynb scripts

- **SLNcode.ipynb**: this is a julia script in a Jupyter notebook adapted from Peter Roopnarine's code from the 2024 NAPC food web workshop. It takes in a guild-level metaweb (guild_matrix.csv) and species richness information (from guilds.csv) and generates species-level networks (SLNs). It also calculates basic network metrics for the SLNs.

- **SLN_maker.jl**: julia function used by SLNcode.ipynb, which constructs an empty array of appropriate size given the species richness of each guild
- **r_no_prey.jl**: julia function used by SLNcode.ipynb, which determines the number of prey items for each species by drawing from a power law distribution

- **RhynieWebCode.R**: this is an R script that takes in a food web adjacency matrix and generates relevant statistics
