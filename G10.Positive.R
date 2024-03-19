# Load the packages
library(dplyr)
library(igraph)

# Import from remote url
characters <- read.csv("https://kushagrakarira.com/data/characters.csv")
relations <- read.csv("https://kushagrakarira.com/data/relations.csv")

# Filter positive relationships
positive_edges <- relations %>%
  filter(type != "-")

# Create a graph object with positive relationships
positive_graph <- graph_from_data_frame(positive_edges, directed = TRUE)

# Set vertex names
V(positive_graph)$name <- characters$name

# Plot the graph without edge labels
plot(positive_graph, 
     edge.arrow.size = 0.5, 
     layout = layout_with_fr,
     edge.color = "blue",  # Set edge color for positive relationships
     vertex.label.cex = 0.8,
     edge.label = NULL)

