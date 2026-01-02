#setwd("~/Dropbox/Work/Devonian terr ecosystems/EcolNetworks/RhynieWebRepo/SLNs/Messel")
library(igraph) 
library(tidyverse)

#### Function to filter out low-certainty links and purge dangling nodes ####
hi_cert_web <- function(links, species_info, min_certainty = 2) {

  # 1. Filter out low-certainty links
  links_hi_cert <- links %>% filter(Certainty >= min_certainty)
  
  # 2. Build a directed graph to analyze connectivity
  # first format the df to match igraph expectations
  graph_hi_cert <- data.frame(from = links_hi_cert$Resource, to = links_hi_cert$Consumer, 
                              Certainty = links_hi_cert$Certainty)
  g <- graph_from_data_frame(graph_hi_cert, directed = TRUE)
  
  # 3. Identify Basal Resources (In-degree = 0)
  basal_nodes <- V(g)[degree(g, mode = "in") == 0]

  # 4. Purge Dangling Nodes (Reachability Check)
  # We want to keep ONLY nodes that can reach a Basal node (directly or indirectly).
  # We calculate the shortest path from every node TO the set of basal nodes.
  if(length(basal_nodes) > 0) {
    # 'distances' calculates path lengths. If no path exists, it returns Inf.
    dists <- distances(g, v = V(g), to = basal_nodes, mode = "in")
    
    # A node is valid if it has a finite distance to AT LEAST one basal node
    has_path_to_basal <- apply(dists, 1, function(x) any(is.finite(x)))
    valid_species_ids <- names(has_path_to_basal)[has_path_to_basal]
    
  } else {
    warning("No basal species found! Returning empty web.")
    valid_species_ids <- character(0)
  }  
  
  # 5. Filter the df based on the valid IDs
  # We convert to character to ensure safe matching with igraph names
  links_hi_cert_clean <- links_hi_cert[as.character(links_hi_cert$Consumer) %in% valid_species_ids & 
                                   as.character(links_hi_cert$Resource) %in% valid_species_ids, ]
  
  species_info_hi_cert_clean <- species_info[as.character(species_info$sp_id) %in% valid_species_ids, ]
  
  return(list(species_info = species_info_hi_cert_clean, links = links_hi_cert_clean))
}

#### Function to convert link table to adjacency matrix ####

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
  
return(list(adj_matrix = adj_matrix, species_info_final = species_info_fun))
}

#### Subset function ----
# --- Function to create and save a subset matrix based on habitat ---
create_subset_matrix <- function(links, species_info, habitat_codes) {
  
  # --- Subset species by habitat ---
  # Habitat codes: 1=terrestrial, 2=aquatic, 3=both
  species_info_sub <- species_info[species_info$habitat %in% habitat_codes, ]
  
  # --- Subset links where both species are in the habitat ---
  valid_ids <- species_info_sub$sp_id
  links_sub <- links[links$Consumer %in% valid_ids & links$Resource %in% valid_ids, ]
  
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

#### load data ----
links <- read.csv("links_messel.csv")

species_info <- read.csv("speciesinfo_messel.csv")

#### build matrices ----
#### Make complete matrix (all links)
matrix_all <- table_to_adjmatrix(links, species_info)

# Write the result to a new CSV file.
write.table(matrix_all$adj_matrix, "matrix_messel.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)

#### generate terrestrial subset (all links)
terr_subset <- create_subset_matrix(links, species_info, habitat_codes = c(1, 3))
write.table(terr_subset$matrix, "matrix_messel_terr.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(terr_subset$species_info, "speciesinfo_messel_terr.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### generate aquatic subset (all links)
aqu_subset <- create_subset_matrix(links, species_info, habitat_codes = c(2, 3))
write.table(aqu_subset$matrix, "matrix_messel_aqu.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(aqu_subset$species_info, "speciesinfo_messel_aqu.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### Make high-certainty matrix (links with certainty > 2)
hi_cert_data <- hi_cert_web(links, species_info, 2)
hi_cert_final <- table_to_adjmatrix(hi_cert_data$links, hi_cert_data$species_info)
# Write the resulting matrix and sp info files to new CSV files
write.table(hi_cert_final$adj_matrix, "matrix_messel_hi_cert.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(hi_cert_final$species_info, "speciesinfo_messel_hi_cert.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### --- Generate high-certainty terrestrial subset
terr_hi_cert <- create_subset_matrix(hi_cert_data$links, hi_cert_final$species_info, habitat_codes = c(1, 3))
write.table(terr_hi_cert$matrix, "matrix_messel_terr_hi_cert.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(terr_hi_cert$species_info, "speciesinfo_messel_terr_hi_cert.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

#### --- Generate high-certainty aquatic subset
aqu_hi_cert <- create_subset_matrix(hi_cert_data$links, hi_cert_data$species_info, habitat_codes = c(2, 3))
write.table(aqu_hi_cert$matrix, "matrix_messel_aqu_hi_cert.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(aqu_hi_cert$species_info, "speciesinfo_messel_aqu_hi_cert.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
