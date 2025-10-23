# setwd("~/Dropbox/Work/Devonian terr ecosystems/EcolNetworks/RhynieWebRepo/SLNs/Messel")

# load data ----
species_info <- read.csv("speciesinfo_messel.csv")
links <- read.csv("links_messel.csv")

# Build Matrix ----

# Find the highest species ID to define the size of the square matrix.
num_species <- max(c(links$Consumer, links$Resource, species_info$sp_id), na.rm = TRUE)

# Initialize a square matrix with rows & columns equal to the number of species, populated with zeros
adj_matrix <- matrix(0, nrow = num_species, ncol = num_species)

# 'links' data has 'Consumer' (row) and 'Resource' (column).
# So, we can create an index matrix from the 'Consumer' and 'Resource' columns.
indices <- as.matrix(links[, c("Consumer", "Resource")])

# Place a '1' at each position corresponding to a consumer-resource interaction.
adj_matrix[indices] <- 1


# Write the adjacency matrix to a new CSV file.
output_filename <- "matrix_messel.csv"
write.table(adj_matrix, output_filename, sep = ",", row.names = FALSE, col.names = FALSE)

# Subset function ----
# --- Function to create and save a subset matrix based on habitat ---
create_subset_matrix <- function(species_info, links, habitat_codes) {
  
  # --- Subset species by habitat ---
  # Habitat codes: 1=terrestrial, 2=aquatic, 3=both
  species_info_sub <- species_info[species_info$habitat %in% habitat_codes, ]
  
  # --- Subset links where both species are in the habitat ---
  valid_ids <- species_info_sub$sp_id
  links_sub <- links[links$Consumer %in% valid_ids & links$Resource %in% valid_ids, ]
  
  # --- Rename original sp_id and create new consecutive sp_id ---
  species_info_sub$original_sp_id <- species_info_sub$sp_id
  species_info_sub$sp_id <- 1:nrow(species_info_sub)
  
  # --- Create a map from original IDs to new consecutive IDs ---
  # A named vector provides an efficient lookup map.
  id_map <- species_info_sub$sp_id
  names(id_map) <- as.character(species_info_sub$original_sp_id)
  
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
  
return(adj_matrix_sub)
}  

# --- Generate Terrestrial Subset ---
terr_subset <- create_subset_matrix(species_info, links, habitat_codes = c(1, 3))
write.table(terr_subset, "matrix_messel_terr.csv", sep = ",", row.names = FALSE, col.names = FALSE)

# --- Generate Aquatic Subset ---
aqu_subset <- create_subset_matrix(species_info, links, habitat_codes = c(2, 3))
write.table(aqu_subset, "matrix_messel_aqu.csv", sep = ",", row.names = FALSE, col.names = FALSE)
