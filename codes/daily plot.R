library(igraph)

iproj1 = read_graph('W15.graphml', format = "graphml")

l1 <- layout_with_kk(iproj1)

V(iproj1)$size <- 2
E(iproj1)$width <- 0.2
E(iproj1)$arrow.size <- .1
E(iproj1)$color <- 'grey'
V(iproj1)$frame.color <- NA
V(iproj1)$label =NA
ml = cluster_louvain(iproj1)
plot(iproj1, layout=l1,vertex.color=membership(ml))