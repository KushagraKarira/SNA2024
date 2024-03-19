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

# Calculate degree centrality
degree_centrality <- degree(graph)

# Calculate betweenness centrality
betweenness_centrality <- betweenness(graph)

# Calculate closeness centrality
closeness_centrality <- closeness(graph)

# Combine centrality measures into a data frame
centrality_df <- data.frame(
  character = V(graph)$name,
  degree = degree_centrality,
  betweenness = betweenness_centrality,
  closeness = closeness_centrality
)
# Plot centrality measures
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
barplot(centrality_df$degree, names.arg = centrality_df$character, main = "Degree Centrality")
barplot(centrality_df$betweenness, names.arg = centrality_df$character, main = "Betweenness Centrality")
barplot(centrality_df$closeness, names.arg = centrality_df$character, main = "Closeness Centrality")

