# Load the necessary libraries
library(igraph)

# Import character and relation data from remote URLs
data <- read.csv("https://kushagrakarira.com/data/GA2G10.csv", row.names = 1)

# Convert the data to a graph object
graph_data <- graph_from_adjacency_matrix(as.matrix(data), mode = "max", weighted = TRUE)

# Calculate the degree centrality of each node
degree_centrality <- degree(graph_data, mode = "all")

# Print the degree centrality of each node
print(degree_centrality)
