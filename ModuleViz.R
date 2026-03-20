
library(igraph)
library(tidyverse)



rhynie.graph <- graph_from_adjacency_matrix(as.matrix(matrix))

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

mods.rhynie <- cluster_louvain(as.undirected(rhynie.graph))
mods.rhynie

class(mods.rhynie) # check the class

plot(mods.rhynie,rhynie.graph)

# We can also make this look neater by changing the node colour instead of circling the four modules in 
# the modern reef, and removing the labels.

V(rhynie.graph)$community <- mods.rhynie$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen", 'lightblue'), alpha=.6)
plot(rhynie.graph, vertex.color=colrs[V(rhynie.graph)$community])