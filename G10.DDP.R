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

# Calculate the degree of each character (number of connections)
degrees <- degree(graph)

# Plot the degree distribution
hist(degrees, 
     breaks = 20,  # Adjust the number of bins as needed
     main = "Degree Distribution of Characters",
     xlab = "Degree",
     ylab = "Frequency")


