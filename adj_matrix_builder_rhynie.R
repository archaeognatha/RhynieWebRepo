# This script builds a guild-level adjacency matrix for the Rhynie Chert food web,
# using interaction priority levels (1 is standard, 2 is priority) from the links file.

# setwd("~/Dropbox/Work/Devonian terr ecosystems/EcolNetworks/RhynieWebRepo")

# load data ----
guild_info <- read.csv("guilds.csv")
links <- read.csv("links.csv")

# Build Matrix ----

# Find the highest guild ID to define the size of the square matrix.
num_guilds <- max(c(links$consumer, links$resource, guild_info$guild_no), na.rm = TRUE)

# Initialize a square matrix with rows & columns equal to the number of guilds, populated with zeros
adj_matrix <- matrix(0, nrow = num_guilds, ncol = num_guilds)

# The 'links.csv' file has a 'priority.level' column.
# We can loop through the links and assign the value from that column.

for (i in 1:nrow(links)) {
  consumer_id <- links$consumer[i]
  resource_id <- links$resource[i]
  priority_val <- links$priority.level[i]
  
  # Check if IDs are valid (not NA and within bounds)
  if (!is.na(consumer_id) && !is.na(resource_id) && 
      consumer_id > 0 && resource_id > 0 && 
      consumer_id <= num_guilds && resource_id <= num_guilds) {
    
    # Assign the priority value (1 or 2) to the matrix cell
    adj_matrix[consumer_id, resource_id] <- priority_val
  }
}

# Write the adjacency matrix to a new CSV file.
output_filename <- "guild_matrix.csv"
write.table(adj_matrix, output_filename, sep = ",", row.names = FALSE, col.names = FALSE)
