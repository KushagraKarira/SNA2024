#Loading Libraries
library(igraph)
library(dplyr)

#Installing data
characters <- read.csv("https://kushagrakarira.com/data/characters.csv")
relation <- read.csv("https://kushagrakarira.com/data/relations.csv")

#Network Visualisation
# Create the graph object
g <- graph_from_data_frame(relation[, c("source", "target")], directed = FALSE)

# Add node attributes (character names)
V(g)$name <- characters$name[match(V(g)$name, characters$id)]
  
  # Calculate degree centrality
degree <- degree(g, mode = "all")

# Join with character data
centrality_df <- data.frame(id = V(g)$name, 
                            degree_centrality = degree)

# View top 10 most central characters
top_central <- centrality_df %>%
  arrange(desc(degree_centrality)) %>%
  head(10)

top_central
