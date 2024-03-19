# Load the necessary libraries
library(dplyr)
library(igraph)

# Import character and relation data from remote URLs
characters <- read.csv("https://kushagrakarira.com/data/characters.csv")
relations <- read.csv("https://kushagrakarira.com/data/relations.csv")

# Merge relations data with character names to create edges
edges <- relations %>%
  mutate(
    type = ifelse(type == "-", "negative", "positive")
  ) %>%
  select(source, target, type)

# Create a graph object
graph <- graph_from_data_frame(edges, directed = TRUE)

# Set vertex names
V(graph)$name <- characters$name

# Plot the graph without edge labels using the force-directed layout
plot(graph, 
     edge.arrow.size = 0.5, 
     layout = layout_with_fr,
     edge.color = ifelse(edges$type == "negative", "red", "blue"),
     vertex.label.cex = 0.8,
     edge.label = NULL)

