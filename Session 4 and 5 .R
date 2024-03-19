#install.packages("onadata")
#install.packages("igraph")
#install.packages()
library(igraph)
library(onadata)
## Basic Plotting

     
# Social Network Analysis

# Plot with 2 vertices
g <- graph(c(1,2))
plot(g)

#plot with 4 vertices
g <- graph(c(1,2,2,3,3,4,4,1))
plot(g)

# Making the graph undirected
g <- graph(c(1,2,2,3,3,4,4,1),
           directed = F)
plot(g)

plot(g,vertex.color="Green",vertex.size=40,edge.color="Red")

plot(g,vertex.color="Green",vertex.size=40,edge.color="Red",edge.curved=.4)

#plotting Example 2 

g1 <- graph(c("May", "Mar", "Mar", "Apr", "Apr", "May",
              "May", "Apr", "Jun", "Apr", "May","Jul","Jul","Jan","Jul","Jun","Jul","Apr","May","Jan"),
            directed=T)

plot(g1,vertex.color="Green",vertex.size=40,edge.color="Red")

plot(g1,vertex.color="Green",vertex.size=40,edge.color="Red",edge.curved=.5,edge.arrow.size=.6)


## Creating a graph from an adjacency matrix

# create 3x3 adjacency matrix
adj_flights <- matrix(c(0, 5, 2, 4, 0, 0, 4, 1, 0), nrow = 3, ncol = 3)
rownames(adj_flights) <- c("SFO", "PHL", "TUS")
colnames(adj_flights) <- rownames(adj_flights)

# create multigraph from adjacency matrix
(flightgraph <- igraph::graph_from_adjacency_matrix(
  adjmatrix = adj_flights,
  mode = "directed"
))

# create weighted graph 
(flightgraph_weighted <- igraph::graph_from_adjacency_matrix(
  adjmatrix = adj_flights,
  mode = "directed",
  weighted = TRUE
))
flightgraph_weighted

(flightgraph_simple <- igraph::simplify(
  flightgraph
))
## Creating a graph from a dataframe

# Using weights as edge width

# Set edge width based on weight:
E(flightgraph_weighted)$width <- E(flightgraph_weighted)$weight
#change arrow size and edge color:
E(flightgraph_weighted)$arrow.size <- .2
# We can even set the network layout:
graph_attr(flightgraph_weighted, "layout") <- layout_with_lgl
plot(flightgraph_weighted)






#We will work with a relatively famous graph known as Zachary’s Karate Club. This graph originates from a piece of research on a karate club by social anthropologist Wayne W. Zachary25, and is commonly used as an example of a social network in many teaching situations today. The graph contains 34 vertices representing different individuals or actors. The karate instructor is labelled as ‘Mr Hi’. The club administrator is labelled as ‘John A’. The other 32 actors are labelled as ‘Actor 2’ through ‘Actor 33’. Zachary studied the social interactions between the members outside the club meetings, and during his study a conflict arose in the club that eventually led to the group splitting into two: one group forming a new club around the instructor Mr Hi and the other group dispersing to find new clubs or to give up karate completely. In this graph, an edge between two vertices means that the two individuals interacted socially outside the club.

# Step 1- get karate edgelist data as dataframe
karate_edgelist <- read.csv("https://ona-book.org/data/karate.csv")
head(karate_edgelist)

# Step 2 - use our edgelist to create an undirected graph object in igraph

(karate <- igraph::graph_from_data_frame(karate_edgelist, 
                                         directed = FALSE))

# Step 3- Basic Plotting

# Step 3.1- set seed for reproducibility
set.seed(123)

# Step 3.2- create random layout
l <- layout_randomly(karate)



# Step 3.3- plot with random layout
plot(karate, layout = l)


# Step 3.4 - only store a label if Mr Hi or John A
V(karate)$label <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          V(karate)$name,
                          "")

# Step 3.5 change label font color, size and font family 

# (selected font family needs to be installed on system)
V(karate)$label.color <- "black"
V(karate)$label.cex <- 0.8
V(karate)$label.family <- "arial"
plot(karate, layout = l)


# Step 3.6 - different colors and shapes for Mr Hi and and John A

V(karate)$color <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "lightblue", 
                          "pink")

V(karate)$shape <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "square", 
                          "circle")

plot(karate, layout = l)

# Step 3.7 - change color and linetype of all edges
E(karate)$color <- "blue"
E(karate)$lty <- "dashed"
plot(karate, layout = l)


set.seed(123)
karate_grid <- igraph::add_layout_(karate, on_grid())

# check a few lines of the 'layout' property
head(karate_grid$layout)

# circle layout
set.seed(123)
circ <- layout_in_circle(karate)
plot(karate, layout = circ)

# sphere layout
set.seed(123)
sph <- layout_on_sphere(karate)
plot(karate, layout = sph)


# F-R algorithm- Fruchterman-Reingold algorithm
set.seed(123)
fr <- layout_with_fr(karate)
plot(karate, layout = fr)

# K-K algorithm- Kamada-Kawai algorithm
set.seed(123)
kk <- layout_with_kk(karate)
plot(karate, layout = kk)


# GEM algorithm
set.seed(123)
gem <- layout_with_gem(karate)
plot(karate, layout = gem)

#############################################################
#install.packages("ggraph")
library(ggraph)

# get karate edgelist
karate_edgelist <- read.csv("https://ona-book.org/data/karate.csv")

# create graph object
karate <- igraph::graph_from_data_frame(karate_edgelist, 
                                        directed = FALSE)

# set seed for reproducibility
set.seed(123)

# visualise using ggraph with fr layout
ggraph(karate, layout = "fr") +
  geom_edge_link() +
  geom_node_point() 
set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(color = "blue", size = 5) +
  theme_void() + 
  labs(title = "Zachary's Karate Club Network")
V(karate)$leader <- ifelse(
  V(karate)$name %in% c("Mr Hi", "John A"), 1, 0
)

set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(color = as.factor(leader)), size = 5, 
                  show.legend = FALSE) +
  theme_void() + 
  labs(title = "Zachary's Karate Club Network")
#**************************************************************************#
# get edgelist with mins property
workfrance_edgelist <- read.csv(
  "https://ona-book.org/data/workfrance_edgelist.csv"
)

# get vertex set with dept property
workfrance_vertices <- read.csv(
  "https://ona-book.org/data/workfrance_vertices.csv"
)

# create undirected graph object
workfrance <- igraph::graph_from_data_frame(
  d = workfrance_edgelist,
  vertices = workfrance_vertices,
  directed = FALSE
)

# basic visualization
set.seed(123)
ggraph(workfrance, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(color = "blue", size = 5) +
  theme_void()

set.seed(123)
ggraph(workfrance, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7, aes(width = mins), 
                 show.legend = FALSE) +
  geom_node_point(aes(color = dept), size = 5) +
  labs(color = "Department") +
  theme_void() +
  labs(title = "Spatial co-location of employees in a workplace")

#********************************************************************************#
londontube_vertices <- read.csv(
  "https://ona-book.org/data/londontube_vertices.csv"
)
head(londontube_vertices)
# download and view london tube edge data
londontube_edgelist <- read.csv(
  "https://ona-book.org/data/londontube_edgelist.csv"
)
head(londontube_edgelist)
lines <- londontube_edgelist |> 
  dplyr::distinct(line, linecolor)

# create graph object
tubegraph <- igraph::graph_from_data_frame(
  d = londontube_edgelist, 
  vertices = londontube_vertices,
  directed = FALSE
)

# visualize tube graph using linecolors for edge color
set.seed(123)
ggraph(tubegraph) +
  geom_node_point(color = "black", size = 1) +
  geom_edge_link(aes(color = line), width = 1) +
  scale_edge_color_manual(name = "Line",
                          values = lines$linecolor) +
  theme_void()
# reorganize to include longitude and latitude for start and end
new_edgelist <- londontube_edgelist |> 
  dplyr::inner_join(londontube_vertices |> 
                      dplyr::select(id, latitude, longitude), 
                    by = c("from" = "id")) |> 
  dplyr::rename(lat_from = latitude, lon_from = longitude) |> 
  dplyr::inner_join(londontube_vertices |> 
                      dplyr::select(id, latitude, longitude), 
                    by = c("to" = "id")) |> 
  dplyr::rename(lat_to = latitude, lon_to = longitude)

# view
head(new_edgelist)
# recreate graph object to capture additional edge data
tubegraph <- igraph::graph_from_data_frame(
  d = new_edgelist, 
  vertices = londontube_vertices,
  directed = FALSE
)
#*************************************#
#install.packages("visNetwork")
library(visNetwork)

nodes <- data.frame(
  id = 1:4,
  label = c("David", "Zubin", "Suraya", "Jane")
)

edges <- data.frame(
  from = c(1, 1, 1, 4, 4),
  to = c(2, 3, 4, 2, 3)
)

visNetwork(nodes, edges) |> 
  visLayout(randomSeed = 123)

library(igraph)
library(ggraph)

# get karate edgelist
karate_edgelist <- read.csv("https://ona-book.org/data/karate.csv")

# create graph object
karate <- igraph::graph_from_data_frame(karate_edgelist, 
                                        directed = FALSE)

# different colors and shapes for Mr Hi and and John A
V(karate)$color <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "lightblue", 
                          "pink")

V(karate)$shape <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "square", 
                          "circle")

# more visible edges 
E(karate)$color = "grey"
E(karate)$width <- 3

# visualize from igraph
visNetwork::visIgraph(karate, layout = "layout_with_fr", 
                      randomSeed = 123) 
########################################
#install.packages("networkD3")
library(networkD3)

# get karate edgelist
karate_edgelist <- read.csv("https://ona-book.org/data/karate.csv")

# visualize
networkD3::simpleNetwork(karate_edgelist)

#get karate edgelist
karate_edgelist <- read.csv("https://ona-book.org/data/karate.csv")

# create igraph object
karate <- igraph::graph_from_data_frame(karate_edgelist, 
                                        directed = FALSE)

# give Mr Hi and John A a different group
V(karate)$group <- ifelse(
  V(karate)$name %in% c("Mr Hi", "John A"), 1, 2
)

# translate to networkD3 - creates a list with links and nodes dfs
# links have a source and target column and group if requested 
netd3_list <- networkD3::igraph_to_networkD3(karate, 
                                             group = V(karate)$group)

# visualize
networkD3::forceNetwork(
  Links = netd3_list$links,
  Nodes = netd3_list$nodes,
  NodeID = "name",
  Source = "source",
  Target = "target",
  Group = "group"
)

