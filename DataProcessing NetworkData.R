################################################################################
##                                                                            ##
##                   Data Processing of Social Network Data                   ##
##                                                                            ##
################################################################################

# input:  point.shp for each behaviour category
# output: Solaredges.csv, Solarnodes.csv, Greeningedges.csv, Greeningnodes.csv
# Caredges.csv, Carnodes.csv, Resourceedges.csv, Resourcenodes.csv 

# note: run "function processinglocationdat.R" and " function location2network.R" 
# before running this script

# ---------------------------------------------------------------------------- #

# rm(list = ls())

library(dplyr)

# Solar Panels
setwd("~/Groningen/Revitalising Neighbourhoods/RevitalisingNeighbourhoods/Solar Panels/")

solardata <- processing_location_data("point.shp")

write.csv(solardata, "SolarNetworkWeights.csv")

nodesedges <- location2network(solardata)
edges <- nodesedges[[1]]
nodes <- nodesedges[[2]]

write.csv(edges, "Solaredges.csv")
write.csv(nodes, "Solarnodes.csv")

# ---------------------------------------------------------------------------- #

# Greening
setwd("~/Groningen/Revitalising Neighbourhoods/RevitalisingNeighbourhoods/Greening/")

greeningdata <- processing_location_data("point.shp")

weighted_contacts <- greeningdata %>% filter(!grepl("huis", buttonname)) %>% 
  
  group_by(respondent) %>% mutate(weighted_contacts = sum(weight)) %>% 
  
  filter(row_number() == 1) %>% dplyr::select(-c(coords.x1, coords.x2, weight))

sum_contacts <- greeningdata %>% filter(!grepl("huis", buttonname)) %>% 
  
  group_by(respondent) %>% transmute(sum_contacts = n())

greeningdata2 <- left_join(weighted_contacts, sum_contacts) %>% 
  
  filter(row_number() == 1)

write.csv(greeningdata2, "GreeningNetworkWeights.csv")

nodesedges <- location2network(greeningdata)
edges <- nodesedges[[1]]
nodes <- nodesedges[[2]]

all(c(edges[,1], edges[,2]) %in% nodes[,1]) 

write.csv(edges, "Greeningedges.csv")
write.csv(nodes, "Greeningnodes.csv")

# ---------------------------------------------------------------------------- #

# Car Use
setwd("~/Groningen/Revitalising Neighbourhoods/RevitalisingNeighbourhoods/Car Use/")

cardata <- processing_location_data("point.shp")

weighted_contacts <- cardata %>% filter(!grepl("huis", buttonname)) %>% 
  
  group_by(respondent) %>% mutate(weighted_contacts = sum(weight)) %>% 
  
  filter(row_number() == 1) %>% dplyr::select(-c(coords.x1, coords.x2, weight))

sum_contacts <- cardata %>% filter(!grepl("huis", buttonname)) %>% 
  
  group_by(respondent) %>% transmute(sum_contacts = n())

cardata2 <- left_join(weighted_contacts, sum_contacts) %>% 
  
  filter(row_number() == 1)

write.csv(cardata2, "CarNetworkWeights.csv")


nodesedges <- location2network(cardata)
edges <- nodesedges[[1]]
nodes <- nodesedges[[2]]

all(c(edges[,1], edges[,2]) %in% nodes[,1]) 

write.csv(edges, "Caredges.csv")
write.csv(nodes, "Carnodes.csv")

# ---------------------------------------------------------------------------- #

# Resource Conservation
setwd("~/Groningen/Revitalising Neighbourhoods/RevitalisingNeighbourhoods/Resource Conservation/")

resourcedata <- processing_location_data("point.shp")

weighted_contacts <- resourcedata %>% filter(!grepl("huis", buttonname)) %>% 
  
  group_by(respondent) %>% mutate(weighted_contacts = sum(weight)) %>% 
  
  filter(row_number() == 1) %>% dplyr::select(-c(coords.x1, coords.x2, weight))

sum_contacts <- resourcedata %>% filter(!grepl("huis", buttonname)) %>% 
  
  group_by(respondent) %>% transmute(sum_contacts = n())

resourcedata2 <- left_join(weighted_contacts, sum_contacts) %>% 
  
  filter(row_number() == 1)

write.csv(resourcedata2, "ResourceNetworkWeights.csv")

nodesedges <- location2network(resourcedata)
edges <- nodesedges[[1]]
nodes <- nodesedges[[2]]

all(c(edges[,1], edges[,2]) %in% nodes[,1]) 

write.csv(edges, "Resourceedges.csv")
write.csv(nodes, "Resourcenodes.csv")

## Analysis ##

# plot network

ledges <- data.frame(color = c("#1b9e77", "#d95f02", "7570b3"),
                     label = c("acquaintances", "friends", "family"))

visNetwork(nodes, edges) %>% 
  visEdges(arrows = 'to') %>% 
  visLegend(addEdges = ledges)


groningen_graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

ggraph(groningen_graph, layout = "auto") + 
  geom_node_point() +
  geom_edge_link(aes(colour = factor(weight)), arrow = arrow(type = "open", length = unit(1.5, 'mm'))) +
  theme_void()

# describe network
## density
edge_density(groningen_graph, loops = FALSE) 
## degree centrality: number of connections between a node and all other nodes
degree(groningen_graph, mode="all")
## betweeness centrality: capacity of standing on the paths that connect other nodes
betweenness(groningen_graph, directed=F, weights=NA, normalized = T)

