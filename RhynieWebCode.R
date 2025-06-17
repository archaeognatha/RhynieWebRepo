### Rhynie Chert Food web code
#
# Modified from NAPC 2024 Workshop, Ann Arbor, MI
# Analytical approaches to networks, trophic structure, and ancient food webs
# 
# Original code written by Carrie L. Tyler (carrie.tyler@unlv.edu), University of Nevada, Las Vegas
# on June 8th, 2024
# -- modified by Tanner Frank
###

# Load the necessary libraries ----

library(dplyr)
library(igraph)
library(NetIndices)

# METANETWORK CONSTRUCTION ----

# use the 'igraph' and 'NetIndices' packages to examine and plot the metanetwork graph. 

# Read in the data ----
rhynie.matrix <- as.matrix(read.csv('guild_matrixR.csv'))

# The adjacency matrix can be used to visualize and assess the metanetwork.
# Note that the matrix needs to be of the class 'matrix'.
rhynie.graph <- graph_from_adjacency_matrix(rhynie.matrix)
class(rhynie.graph) # Check the class

# We can use the 'igraph' package to visualize the network. 
# First let's examine the nodes (vertices) and edges (links) before plotting:
V(rhynie.graph) # Nodes (guilds), also known as vertices

# Each guild must be listed here. We have used the guild numbers here, but later, 
# you can assign the guild names to this vector.

E(rhynie.graph) # Edges (links or interactions)

# Edges (or vertices) are the interactions, in this case potential interactions between guilds, 
# and are arranged with consumers and resources, with each row as a unique interaction. 
# Every guild must be listed here, as a food web would not have a guild in the dataset that is not a 
# consumer or resource. That is there cannot be any isolated nodes. Note that a guild can consume resources from itself.

# The 'igraph' package offers customization through a wide range of plot parameters. 
# You can find all of the package options online by searching for igraph

# Graphing ----
# Now let's make our graph plot.
plot.igraph(rhynie.graph, vertex.shape="rectangle")

# The guild numbers are too big relative to the nodes, and the arrows are also too large. 
# Next, let's adjust those by setting the sizes of the vertices (nodes) and edges (links), 
# and changing the colour of the nodes and node label text.
V(rhynie.graph)$size <- 15
V(rhynie.graph)$color <- 'lightgreen'
V(rhynie.graph)$label.cex <- 0.5
V(rhynie.graph)$label.color <- "black"
E(rhynie.graph)$arrow.size <- 0.5

plot.igraph(rhynie.graph)

# When plotting, you can also choose from several styles of layout. 
# There are numerous options, but let's try plotting the graph with a circular layout.

plot.igraph(rhynie.graph,
            layout = layout_in_circle)

# This time, we will use the Guild List to create the network in R. 

# Read in the data
guilds<-read.csv("guild_matrix_v1.csv",header=T)
guilds

#make detritus a producer within network 
guilds[1,]$Prey <- ""
guilds[13,]$Prey <- ""

# We can also use the guild list to create an adjacency matrix which can be used in Julia

# Create adjacency matrix ----
# create empty matrix to input binary data representing guild interactions as adjacency matrix
adjacency.matrix <-  data.frame(matrix(ncol = nrow(guilds), nrow = nrow(guilds)))
rownames(adjacency.matrix) <- guilds[,1]
colnames(adjacency.matrix) <- guilds[,1]

# loop to take guild interactions and make adjacency matrix
for (i in 1:nrow(guilds)) { #for each row in guild interactions
  x <- as.list(strsplit(guilds$Prey[i], ",")) #separate Prey string
  y <- unlist(x) # unlist
  z <- as.numeric(y,'numeric') # convert to numeric
  
  for (j in z) { #for each entry in the Prey string,
    for (k in 1:ncol(adjacency.matrix)) { # for each column
      
      if (j == as.numeric(colnames(adjacency.matrix))[[k]]) { # if Prey matches column
        adjacency.matrix[k,i] <- 1 #then insert a 1 in that column in the ith row
      
      }
      
    }
  }
}

matrix <- replace(adjacency.matrix, is.na(adjacency.matrix), 0)
names(matrix) <- guilds[,1]
matrix

rhynie.graph <- graph_from_adjacency_matrix(as.matrix(matrix))
V(rhynie.graph)$size <- 20
V(rhynie.graph)$color <- 'lightblue'
V(rhynie.graph)$label.cex <- 0.7
V(rhynie.graph)$label.color <- "black"
E(rhynie.graph)$arrow.size <- 0.5

plot.igraph(rhynie.graph)

# Metrics

## Exercise 2

# We can describe the graph using a variety of metrics calculated in 'igraph'. 
# These metrics are intended for use on species level networks, but can still be useful in quantifying metanetwork structure.

# Vertices - The number of nodes (in this case, the number of guilds).
# Edges - The number of edges (the number of consumer-resource links beteen the guilds).
# Edge density - The proportion of edges relative to the maximum possible (0-1). The closer to 1 the more interconnected the network.
# Network diameter - A measure of how compact the network is, given its size and degree of interconnectedness. 
#               The function will output the longest of the shortest paths across all edges.
# Degree - The number of links associated with a node, this can be in, out, or total.
# Link density - The average number of links per node (in this case per guild).
# Diameter - The longest path of all the calculated shortest paths between pairs of species/guilds/nodes in a network.
# Connectance - The fraction of all possible links that are realized, and is a commonly used measure of food web complexity.
# Compartmentalizsation - The degree of connectedness of subsystems within a network, also known as modularity, ranges from 0-1.

# Let's start by checking how many nodes and edges our graphs have:

# Vertices (nodes) - in this case guilds
vcount(rhynie.graph) 

# Links (interactions or edges)
gsize(rhynie.graph)

# The modern network has 24 nodes (vertices) and 49 edges (links), 
# and the fossil network has 16 nodes and 22 edges. Now let's calculate a few other metrics.

# Degree ----
# Degree is another important quality for food webs, as it characterizes consumer-resource interactions. 
# It is typically more useful to examine in-degree and out-degree (as opposed to total degree).

# In-degree: the number of links going into a node; i.e., the number of prey/resources for that species/guild
# Out-degree: the number of links going out of a node; i.e., the number of predators/consumers for that species/guild

# The 'mode' in the degree function below can be set to 'in', 'out' or 'total' for in-degree, 
# out-degree, or the sum of in- and out-degree (total). 

in.degree.m <- degree(rhynie.graph, mode = 'in', loops = T) 
in.degree.m

# We can plot the distribution of degree using a simple histogram.

hist(in.degree.m, main = 'Rhynie In Degree', breaks = 7)

# Link Density ----

# Edge or link density can also be useful to assess. More guilds (or species) may lead to increase 
# in consumer-resource interactions (trophic links) between them, increasing the density of interactions 
# and energy flow within network (link density) 	Link density = L/S 
# where S is the number of guilds (nodes) and L is the number of links trophic interactions (links)

edge_density(rhynie.graph)

# Diameter ----

# Let's compare the diameter of the modern and fossil networks (Remember, network diameter 
# is the longest path of all the calculated shortest paths between pairs of guilds in a network).

diameter(rhynie.graph)

# Connectance ----

# Another useful package for estimating network structure is 'NetIndices' which has a general indices 
# function 'GenInd', that calculates some additional metrics such as connectance (C), and compartmentalization (Cbar). 
# Connectance is a measure of network complexity. It is the proportion of realized ecological interactions among 
# the potential ones C = L/S^2 
# where S is the number of guilds (nodes) and L is the number of links trophic interactions (links)

ind <- GenInd(rhynie.matrix)

ind

# When plotting the graph, we can also adjust the size of the nodes to reflect an additional variable, 
# such as degree, and now let's also make the edges curved.

#V(rhynie.graph)$size <- in.degree.m * 5 # Five here is just a scaling factor

#plot.igraph(rhynie.graph,
#            edge.curved=0.4)

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

###################### END
#print output SLNs from Julia code

setwd('/Users/tannerfrank/Dropbox/Work/Devonian terr ecosystems/EcolNetworks/RhynieWebCode2024')
rhynie.SLN <- as.matrix(read.csv('SLN/output/complete/SLN_matrix_4.csv'))
SLN.graph <- graph_from_adjacency_matrix(rhynie.SLN)
plot.igraph(SLN.graph)
V(SLN.graph)$size <- 9
V(SLN.graph)$color <- 'lightgreen'
V(SLN.graph)$label.cex <- 0.4
V(SLN.graph)$label.color <- "black"
E(SLN.graph)$arrow.size <- 0.1
plot.igraph(SLN.graph)

mods.SLN <- cluster_louvain(as.undirected(SLN.graph))
mods.SLN

V(SLN.graph)$community <- mods.SLN$membership
colrs <- adjustcolor( c("gray50", "yellowgreen", "gold", "tomato", 'lightblue'), alpha=.6)
plot(SLN.graph, vertex.color=colrs[V(SLN.graph)$community])
V(SLN.graph)$size <- 9

