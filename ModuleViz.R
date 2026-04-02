library(igraph)
library(tidyverse)

# choose which SLN to analyze (1-1000):
SLNid <- 1

path <- "SLNs/Rhynie_TS"

matrix <- read.csv(paste0(path, "/matrix_", SLNid, ".csv"), header = F) 
sp_info <- read.csv(paste0(path, "/speciesinfo_", SLNid, ".csv"), header = T) 

# store rhynie matrix as igraph object. t() transposes matrix 
rhynie.graph <- graph_from_adjacency_matrix(t(matrix), mode = "directed", weighted = NULL)

# associate species info with igraph object 
V(rhynie.graph)$sp_ID    <- sp_info$trophospecies_id
V(rhynie.graph)$guild    <- sp_info$guild
V(rhynie.graph)$ntp      <- sp_info$sp_ntp

# Modularity ----

# Another important aspect of network structure is modularity. Modules are groups of nodes that are 
# interacting mostly with one another, that is densely connected groups with sparser connections 
# between groups. Different groups of nodes may perform different functions with some degree of independence, 
# and may increase the probability of network stability. These groups are often referred to as 'modules' or 
# 'compartments' and the strength of the division into groups is 'modularity'. Modularity varies from 0-1 
# with 1 indicating high modularity.

# There are a variety of algorithms to partition the network into modules and measure modularity. 
# The different methods for finding communities in igraph include: cluster_edge_betweenness, cluster_fast_greedy, 
# cluster_label_prop, cluster_leading_eigen, cluster_louvain, cluster_leiden, cluster_optimal, cluster_spinglass, 
# cluster_walktrap. We will start with the Louvain algorithm to identify 
# modules and then add them to the graph. All of the above functions return an object of the class 'communities'.

mods.rhynie <- cluster_louvain(as_undirected(rhynie.graph))

n_mods <- max(mods.rhynie$membership) # number of modules detected
print(paste0("Modules detected in SLN ", SLNid ,": ", n_mods))

library(RColorBrewer)
base_colors <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen", 'lightblue'), alpha=.6)
colrs <- adjustcolor(colorRampPalette(base_colors)(n_mods), alpha = 0.6)

# use Kamada-Kawaito algorithm to set spacing between nodes
lay <- layout_with_kk(rhynie.graph)
lay <- norm_coords(lay, ymin = -1.75, ymax = 1.75, xmin = -1.75, xmax = 1.75)

V(rhynie.graph)$module <- mods.rhynie$membership
par(mar = c(0, 0, 2, 0))  # reset bottom, left, top, right margins — leaves room @ top for title
plot(rhynie.graph,
     main = paste0("SLN ", SLNid), # plot title
     vertex.color = colrs[V(rhynie.graph)$module],
     vertex.size = 10,             # default is 15
     vertex.label.cex = 0.6,       # shrinks the numbers
     vertex.label.color = "black",
     edge.arrow.size = 0.2,        # default is 1
     layout = lay,                 # spread nodes out,
     rescale = FALSE
)

print(V(rhynie.graph)$guild)

