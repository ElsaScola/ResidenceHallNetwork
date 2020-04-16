
library("igraph")

dat=read.csv("./Residence-hall-matrix.csv",header=FALSE) # read .csv file
adjmatrix <- as.matrix(dat)

mode(adjmatrix) <- "numeric"
g <- graph.adjacency(adjmatrix, mode = "directed", weighted = TRUE, diag = FALSE)

get.adjacency(g)
get.adjlist(g)

## NETWORK FEATURES
vcount(g) # count number of vertices/nodes
ecount(g) # count number of edges/links
E(g) # list of edges
V(g) # list of vertices
is_directed(g) # check directed graph
is_weighted(g) # check weighted graph
cbind(get.edgelist(g), E(g)$weight) # display edge weights in a list
min(E(g)$weight) # the weights range from 1 to 5
max(E(g)$weight)


## DEGREE
summary(degree(g))
summary(log(degree(g)))

V(g)[degree(g)>30] # get most connected nodes
dg <- data.frame(name=V(g)$name,degree=degree(g)) # order nodes by their degree
head(dg[order(dg$degree,decreasing=T),])

plot(degree_distribution(g,cumulative=F),type="s") # degree distribution
plot(degree_distribution(g,cumulative=F),type="s",log='xy') # degree distribution

degree(g, nodes=c("V156"))

## ASSORTATIVITY COEFFICIENT (PEARSON)
assortativity_degree(g)

## CLUSTERING COEFFICIENT (TRANSITIVITY)
transitivity(g, type="global")

## AVERAGE SHORTEST PATH LENGTH AND GIANT CONNECTED COMPONENT
mean_distance(g, directed=TRUE)

cc <- clusters(g) # clustering membership
gGC <- induced.subgraph(g,vids = which(cc$membership==which.max(cc$csize))) # giant component
cc$csize
is.connected(gGC)

## GENERATE RANDOM GRAPH
random_g <- erdos.renyi.game(n=217,p.or.m = 2672,type = "gnm",directed = TRUE)

plot(random_g)
summary(degree(random_g))
summary(log(degree(random_g)))
plot(degree_distribution(random_g,cumulative=F),type="s") # degree distribution
plot(degree_distribution(random_g,cumulative=F),type="s",log='xy') # degree distribution
assortativity_degree(random_g)
transitivity(random_g, type="global")
mean_distance(random_g, directed=TRUE)

## CONFIGURATION MODEL NETWORK
d0=degree(g)
random_network<-sample_degseq(d0)

plot(random_network)
summary(degree(random_network))
summary(log(degree(random_network)))
plot(degree_distribution(random_network,cumulative=F),type="s") # degree distribution
plot(degree_distribution(random_network,cumulative=F),type="s",log='xy') # degree distribution
assortativity_degree(random_network)
transitivity(random_network, type="global")
mean_distance(random_network, directed=TRUE)


## LOUVAIN METHOD
undirected<-as.undirected(g,mode = "collapse")
clusterlouvain <- cluster_louvain(undirected)
plot(undirected, vertex.color=rainbow(3, alpha=0.6)[clusterlouvain$membership])


## BETWEENNESS AND CLOSENESS CENTRALITY
bet <- betweenness(g)  
bet <- as.data.frame(bet)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g,niter=500)
V(g)$size = betweenness(g)/200 
E(g)$color <- "grey"

plot(g, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)


set.seed(3952)
layout1 <- layout.fruchterman.reingold(g,niter=500)
V(g)$size=closeness(g, mode = "all")/.05
E(g)$color <- "grey"

plot(g, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)

# IN-DEGREE CENTRALITY
require(visNetwork, quietly = TRUE)
nodes <- as_data_frame(g, what="vertices")
edges <- as_data_frame(g, what="edges")
visNetwork(
  setNames(nodes, "id"),
  setNames(edges, c("from", "to", "foo"))
) %>% visOptions(selectedBy = "in-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)

## VISUALIZE COMMUNITIES
require(visNetwork, quietly = TRUE)
nodes <- as_data_frame(g, what="vertices")
edges <- as_data_frame(g, what="edges")
visNetwork(
  setNames(nodes, "id"),
  setNames(edges, c("from", "to", "foo"))
)

