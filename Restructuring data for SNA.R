library(igraph)
library("onadata")
library(dplyr)
library(ggraph)


# data frame 1
df1 = data.frame(CustomerId = c(1:6), Product = c("Oven","Television","Mobile","WashingMachine","Lightings","Ipad"))
df1 

# data frame 2
df2 = data.frame(CustomerId = c(2, 4, 6, 7, 8), State = c("California","Newyork","Santiago","Texas","Indiana")) 
df2 

#### Left Join using merge function
df = merge(x=df1,y=df2,by="CustomerId")
df

#### Left Join using inner_join function 
df= df1 %>% inner_join(df2,by="CustomerId")
df
###### outer join in R using merge() function
df = merge(x=df1,y=df2,by="CustomerId",all=TRUE)
df

###### outer join in R using outer_join() function 
df= df1 %>% full_join(df2,by="CustomerId")
df
###### outer join in R using outer_join() function 

df= df1 %>% full_join(df2,by="CustomerId")
df

###### left join in R using left_join() function 

df= df1 %>% left_join(df2,by="CustomerId")
df
###### right join in R using merge() function 
df = merge(x=df1,y=df2,by="CustomerId",all.y=TRUE)
df


###### right join in R using merge() function 

library(dplyr)

df= df1 %>% right_join(df2,by="CustomerId")
df

##### cross join in R

df = merge(x = df1, y = df2, by = NULL)
df


#### Semi join in R

library(dplyr)

df= df1 %>% semi_join(df2,by="CustomerId")
df
#### anti join in R

library(dplyr)
df= df1 %>% anti_join(df2,by="CustomerId")

df

###Transforming data in rectangular tables for use in graphs


# download chinook database tables
chinook_employees <- read.csv("https://ona-book.org/data/chinook_employees.csv")
chinook_customers <- read.csv("https://ona-book.org/data/chinook_customers.csv")
chinook_invoices <- read.csv("https://ona-book.org/data/chinook_invoices.csv")
chinook_items <- read.csv("https://ona-book.org/data/chinook_items.csv")

# create edgelist
(orgchart_edgelist1 <- chinook_employees |> dplyr::inner_join(chinook_employees,by = c("EmployeeId" = "ReportsTo")))
(orgchart_edgelist2 <- orgchart_edgelist1 |> dplyr::select(from = FirstName.x, to = FirstName.y)) |> dplyr::filter(!is.na(from) & !is.na(to))
# create orgchart graph
(orgchart <- igraph::graph_from_data_frame(d = orgchart_edgelist2))

# create management structure as dendrogram (tree)
set.seed(1234)
ggraph(orgchart, layout = 'dendrogram') + geom_edge_elbow() +geom_node_label(aes(label = name), fill ="lightblue") + theme_void()


head(chinook_customers)
# create customer to support rep edgelist
cust_reps <- chinook_customers |> 
  dplyr::inner_join(chinook_employees, 
                    by = c("SupportRepId" = "EmployeeId")) |> 
  dplyr::mutate(
    CustomerName = paste(FirstName.x, LastName.x),
    RepName = paste(FirstName.y, LastName.y)
  ) |> 
  dplyr::select(RepName, CustomerName, SupportRepId)

# view head
head(cust_reps)
# create igraph
cust_rep_graph <- igraph::graph_from_data_frame(
  d = cust_reps
)

# create customer and rep property for vertices
V(cust_rep_graph)$Type <- ifelse(
  V(cust_rep_graph)$name %in% cust_reps$RepName,
  "Rep",
  "Customer"
)

# visualize with color and name aesthetic
set.seed(123)
ggraph(cust_rep_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_label(aes(color = Type, label = name), size = 2) +
  theme_void()
# connect customers via common support rep
cust_cust <- cust_reps |> 
  dplyr::inner_join(chinook_customers, by = "SupportRepId") |> 
  dplyr::mutate(Customer1 = CustomerName,
                Customer2 = paste(FirstName, LastName)) |> 
  dplyr::select(Customer1, Customer2, RepName)

# view head
head(cust_cust)
# remove loop edges
customer_network_edgelist <- cust_cust |> 
  dplyr::filter(
    Customer1 != Customer2
  ) 

# view head
head(customer_network_edgelist)
# create igraph object
customer_network <- igraph::graph_from_data_frame(
  d = customer_network_edgelist,
  directed = FALSE
)

# visualize
set.seed(123)
ggraph(customer_network) +
  geom_edge_link(aes(color = RepName), alpha = 0.3) +
  geom_node_point(color = "lightblue", size = 6) +
  theme_void()
# view some invoices
head(chinook_invoices, 3)
# generate distinct customer-item pairs
cust_item <- chinook_customers |> 
  dplyr::inner_join(chinook_invoices) |> 
  dplyr::inner_join(chinook_items) |> 
  dplyr::mutate(CustName = paste(FirstName, LastName)) |> 
  dplyr::select(CustName, TrackId) |> 
  dplyr::distinct()

# view head
head(cust_item, 3)
# initiate graph object
customer_item_network <- igraph::graph_from_data_frame(
  d = cust_item,
  directed = FALSE
)

# create vertex type
V(customer_item_network)$Type <- ifelse(
  V(customer_item_network)$name %in% cust_item$TrackId,
  "Item",
  "Customer"
)
set.seed(123)
ggraph(customer_item_network, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(color = Type), size = 2) +
  theme_void()
# join customers to customers via common items
cust_cust_itemjoin <- cust_item |> 
  dplyr::inner_join(cust_item, by = "TrackId") |> 
  dplyr::select(CustName1 = CustName.x, 
                CustName2 = CustName.y, TrackId) |> 
  dplyr::filter(CustName1 != CustName2)

# view head
head(cust_cust_itemjoin)
# avoid double counting
cust_item_network <- cust_cust_itemjoin |> 
  dplyr::group_by(Cust1 = pmin(CustName1, CustName2), 
                  Cust2 = pmax(CustName1, CustName2)) |> 
  dplyr::summarise(TrackId = unique(TrackId), .groups = 'drop')

# view head
head(cust_item_network)
# check double size
nrow(cust_cust_itemjoin)/nrow(cust_item_network)
# count common items
cust_item_network <- cust_item_network |> 
  dplyr::count(Cust1, Cust2, name = "Items") 

# view head
head(cust_item_network)
# create undirected graph
custtocust_network <- igraph::graph_from_data_frame(
  d = cust_item_network,
  directed = FALSE
)

# visualize with edges color coded by no of items
set.seed(123)
ggraph(custtocust_network) +
  geom_edge_link(aes(color = ordered(Items)), alpha = 0.5) +
  geom_node_point(color = "lightblue", size = 6) +
  labs(edge_color = "# of Common Items") +
  theme_void()
# select edges that have Item value of at least 2
edges <- E(custtocust_network)[E(custtocust_network)$Items >= 2]

# create subgraph using these edges
two_item_graph <- igraph::subgraph.edges(custtocust_network, 
                                         eids = edges)

# visualise
set.seed(123)
ggraph(two_item_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(color = "lightblue", size = 6) +
  theme_void()

