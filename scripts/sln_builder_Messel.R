#!/usr/bin/env Rscript
# ============================================================
# sln_builder_Messel.R
#
# Builds 6 versions of the Messel fossil food web from Dunne et
# al. 2014 using a complete species info table + list of pairwise
# links: the full web, aquatic + terrestrial subsets, and a high-
# certainty version of each that excludes low-certainty links
#
# 4 functions: 
#   purge_stranded: removes links and nodes that are unconnected or 
#   consumers without downstream path to a basal resource
#
#   hi_cert_web: creates a high-certainty subset web from a link list 
#   and species info table
#
#   create_subset_matrix: creates a habitat subset web from a link list 
#   and species info table
#
#   table_to_adjmatrix: makes a WebMetrics-readable adjacency matrix
#   from a link list and species info table
#
# Usage:
#   Rscript scripts/sln_builder_Messel.R \
#     --speciesinfo data/messel/speciesinfo_messel.csv \
#     --links data/messel/links_messel.csv \
#     --out SLNs/Messel/raw \
# ============================================================

library(igraph) 
library(tidyverse)

#### ---- argument parsing --------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
opt_val <- function(flag, default = NA_character_) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
opt_flag <- function(flag) flag %in% args

species_path <- opt_val("--speciesinfo", "data/messel/speciesinfo_messel.csv")
links_path   <- opt_val("--links",       "data/messel/links_messel.csv")
output_path  <- opt_val("--out",         "SLNs/messel/raw")

for (p in c(species_path, links_path)) {
  if (!file.exists(p)) stop("Input file not found: ", p, call. = FALSE)
}
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

species_info <- read.csv(species_path)
links        <- read.csv(links_path)

#### ---- add habitat columns ------------------------------------------###
# Binary habitat flags for web_metrics.jl (matches Rhynie convention); habitat: 1 = terr, 2 = aqu, 3 = both
species_info$terr <- as.integer(species_info$habitat %in% c(1, 3))
species_info$aqu  <- as.integer(species_info$habitat %in% c(2, 3))

#### ---- function to purge stranded consumer nodes --------------------------------------------------

# Keep only nodes with a resource path to a basal guild
# Nodes with no links in 'links' drop out
purge_stranded <- function(links, species_info, label = "") {
  # Build a directed graph to analyze connectivity
  # first format the df to match igraph expectations
  g <- graph_from_data_frame(
    data.frame(from = links$Resource, to = links$Consumer),  # edge: resource -> consumer
    directed = TRUE)
  
  # Identify "proper" basal nodes 
  basal_guilds <- c("autotroph", "myxotroph", "chemotroph", "epiphyte", "detritus")
  basal_ids <- intersect(V(g)$name,
                         as.character(species_info$sp_id[trimws(species_info$guild) %in% basal_guilds]))
  
  # Purge Dangling Nodes (Reachability Check)
  # We want to keep ONLY nodes that can reach a Basal node (directly or indirectly).
  # We calculate the shortest path from every node TO the set of basal nodes.
  d <- distances(g, v = V(g), to = basal_ids, mode = "in") # distances calculates path lengths. If no path exists, it returns Inf.
  keep <- rownames(d)[apply(d, 1, function(x) any(is.finite(x)))] # a node is valid if it has a finite distance to AT LEAST one basal node

  n_purged <- length(setdiff(V(g)$name, keep))
  n_unlinked <- sum(!as.character(species_info$sp_id) %in% V(g)$name)
  message(label, ": dropped ", n_unlinked, " unlinked node(s); purged ", n_purged, " stranded consumer node(s)")
  
  list(links = links[as.character(links$Consumer) %in% keep &
                       as.character(links$Resource) %in% keep, ],
       species_info = species_info[as.character(species_info$sp_id) %in% keep, ])
}

#### ---- function to filter out low-certainty links --------------------------------------------------
hi_cert_web <- function(links, species_info, max_uncertainty = 2, label = "") {

  # Filter out low-certainty links NOTE: lower "un"certainty score value corresponds to greater certainty
  links_hi_cert <- links %>% filter(Certainty <= max_uncertainty)
  
  return(purge_stranded(links_hi_cert, species_info, label = label))
  
}

#### ---- function to convert link table to adjacency matrix --------------------------------------------------

table_to_adjmatrix <- function(links, species_info) {

  ## Make copy of species_info df
  species_info_fun <- species_info
  
  ## Build Matrix
  # Find the highest species ID to define the size of the square matrix.
  num_species <- nrow(species_info)
  
  # Initialize a square matrix with rows & columns equal to the number of species, populated with zeros
  adj_matrix <- matrix(0, nrow = num_species, ncol = num_species)
  
  # --- If the web is not complete, rename original sp_id and create new consecutive sp_id ---
  if (num_species != max(species_info$sp_id)){
    
    species_info_fun$original_sp_id <- species_info$sp_id
    species_info_fun$sp_id <- 1:nrow(species_info)
    
    # --- Create a map from original IDs to new consecutive IDs 
    # A named vector provides an efficient lookup map.
    id_map <- species_info_fun$sp_id
    names(id_map) <- as.character(species_info_fun$original_sp_id)
    
    # --- Remake links with new consecutive IDs ---
    links$Consumer <- id_map[as.character(links$Consumer)]
    links$Resource <- id_map[as.character(links$Resource)]
    
    # Remove any rows with NAs that might have resulted from mapping
    links <- na.omit(links)
    
    # Create an index matrix from the updated 'Consumer' and 'Resource' columns
    indices <- as.matrix(links[, c("Consumer", "Resource")])
    
    # Populate the matrix
    if (nrow(indices) > 0) {
      adj_matrix[indices] <- 1
    }
  }
  else{
    # if the web is complete, just do the last part (filling in the matrix)
    indices <- as.matrix(links[, c("Consumer", "Resource")])
    
    if (nrow(indices) > 0) {
      adj_matrix[indices] <- 1
    }
  }
  
return(list(adj_matrix = adj_matrix, species_info = species_info_fun))
}

#### Habitat subset function ----####
# --- Function to create and save a subset matrix based on habitat ---
create_subset_matrix <- function(links, species_info, habitat_codes, label = "") {
  
  # --- Subset species by habitat ---
  # Habitat codes: 1=terrestrial, 2=aquatic, 3=both
  species_info_sub <- species_info[species_info$habitat %in% habitat_codes, ]
  
  # --- Subset links where both species are in the habitat ---
  valid_ids <- species_info_sub$sp_id
  links_sub <- links[links$Consumer %in% valid_ids & links$Resource %in% valid_ids, ]
  
  # --- Purge consumers stranded by the habitat subset (e.g. amphibious taxa feeding only in the other realm) ---
  purged <- purge_stranded(links_sub, species_info_sub, label = label)
  links_sub <- purged$links
  species_info_sub <- purged$species_info
  
  # --- Rename original sp_id and create new consecutive sp_id if it doesn't already exist ---
  # --- Create a map from previous unsubsetted IDs to new consecutive IDs ---
  # A named vector provides an efficient lookup map.
  
  if("original_sp_id" %in% colnames(species_info_sub)){
    species_info_sub$prev_id <- species_info_sub$sp_id
    species_info_sub$sp_id <- 1:nrow(species_info_sub)
    
    id_map <- species_info_sub$sp_id
    names(id_map) <- as.character(species_info_sub$prev_id)
  }
  else{
    species_info_sub$original_sp_id <- species_info_sub$sp_id
    species_info_sub$sp_id <- 1:nrow(species_info_sub)
    
    id_map <- species_info_sub$sp_id
    names(id_map) <- as.character(species_info_sub$original_sp_id)
  }
  
  # --- Remake links with new consecutive IDs ---
  links_sub$Consumer <- id_map[as.character(links_sub$Consumer)]
  links_sub$Resource <- id_map[as.character(links_sub$Resource)]
  
  # Remove any rows with NAs that might have resulted from mapping
  links_sub <- na.omit(links_sub)
  
  # --- Make the matrix as before ---
  num_species_sub <- nrow(species_info_sub)
  adj_matrix_sub <- matrix(0, nrow = num_species_sub, ncol = num_species_sub)
  
  # Create an index matrix from the updated 'Consumer' and 'Resource' columns
  indices <- as.matrix(links_sub[, c("Consumer", "Resource")])
  
  # Populate the matrix
  if (nrow(indices) > 0) {
    adj_matrix_sub[indices] <- 1
  }
  
  return(list(matrix = adj_matrix_sub, species_info = species_info_sub))
}  

#### build matrices ----
#### Make complete matrix (all links)
matrix_all <- table_to_adjmatrix(links, species_info)

# Write the result to a new CSV file.
write.table(matrix_all$adj_matrix, file.path(output_path, "matrix_messel.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(species_info, file.path(output_path, "speciesinfo_messel.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### generate terrestrial subset (all links)
terr_subset <- create_subset_matrix(links, species_info, habitat_codes = c(1, 3), label = "messel_terr")
write.table(terr_subset$matrix, file.path(output_path, "matrix_messel_terr.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(terr_subset$species_info, file.path(output_path, "speciesinfo_messel_terr.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### generate aquatic subset (all links)
aqu_subset <- create_subset_matrix(links, species_info, habitat_codes = c(2, 3), label = "messel_aqu")
write.table(aqu_subset$matrix, file.path(output_path, "matrix_messel_aqu.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(aqu_subset$species_info, file.path(output_path, "speciesinfo_messel_aqu.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### Make high-certainty matrix (links with certainty 2 or better)
hi_cert_data <- hi_cert_web(links, species_info, 2, label = "messel_hi_cert")
hi_cert_final <- table_to_adjmatrix(hi_cert_data$links, hi_cert_data$species_info)
# Write the resulting matrix and sp info files to new CSV files
write.table(hi_cert_final$adj_matrix, file.path(output_path, "matrix_messel_hi_cert.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(hi_cert_final$species_info, file.path(output_path, "speciesinfo_messel_hi_cert.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### --- Generate high-certainty terrestrial subset
terr_hi_cert <- create_subset_matrix(hi_cert_data$links, hi_cert_data$species_info, habitat_codes = c(1, 3), label = "messel_terr_hi_cert")
write.table(terr_hi_cert$matrix, file.path(output_path, "matrix_messel_terr_hi_cert.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(terr_hi_cert$species_info, file.path(output_path, "speciesinfo_messel_terr_hi_cert.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### --- Generate high-certainty aquatic subset
aqu_hi_cert <- create_subset_matrix(hi_cert_data$links, hi_cert_data$species_info, habitat_codes = c(2, 3), label = "messel_aqu_hi_cert")
write.table(aqu_hi_cert$matrix, file.path(output_path, "matrix_messel_aqu_hi_cert.csv"), 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(aqu_hi_cert$species_info, file.path(output_path, "speciesinfo_messel_aqu_hi_cert.csv"), quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
