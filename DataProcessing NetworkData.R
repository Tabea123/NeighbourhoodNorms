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
setwd("~/Groningen/Project 1/NeighbourhoodNorms/Solar Panels/")

solardata <- processinglocationdat("point.shp")

write.csv(solardata, "SolarNetworkData.csv")

nodesedges <- location2network(solardata)
edges <- nodesedges[[1]]
nodes <- nodesedges[[2]]

all(edges[,1] %in% nodes[,1]) 
all(edges[,2] %in% nodes[,1])

write.csv(edges, "Solaredges.csv")
write.csv(nodes, "Solarnodes.csv")

# ---------------------------------------------------------------------------- #

# Greening
setwd("~/Groningen/Project 1/NeighbourhoodNorms/Greening/")

greeningdata <- processinglocationdat("point.shp")

write.csv(greeningdata, "GreeningNetworkData.csv")

nodesedges <- location2network(greeningdata)
edges <- nodesedges[[1]]
nodes <- nodesedges[[2]]

all(edges[,1] %in% nodes[,1]) 
all(edges[,2] %in% nodes[,1]) 

write.csv(edges, "Greeningedges.csv")
write.csv(nodes, "Greeningnodes.csv")

# ---------------------------------------------------------------------------- #

# Car Use
setwd("~/Groningen/Project 1/NeighbourhoodNorms/Car Use/")

cardata <- processinglocationdat("point.shp")

write.csv(cardata, "CarNetworkData.csv")

nodesedges <- location2network(cardata)
edges <- nodesedges[[1]]
nodes <- nodesedges[[2]]

all(edges[,1] %in% nodes[,1]) 
all(edges[,2] %in% nodes[,1])

write.csv(edges, "Caredges.csv")
write.csv(nodes, "Carnodes.csv")

# ---------------------------------------------------------------------------- #

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

