## SOCIAL MEDIA IMPLEMENTATION FOR DEVELOPERS

library(igraph)
library(shiny)
library(networkD3)
library(magrittr)


setwd("C:/Users/Miya/OneDrive/BlueOptima")
link_big <- read.csv("Repositories.csv ", header=T)
dim(link_big)

### Bipartite Graph Visualization


ibig_repo <- graph_from_data_frame(link_big, directed = FALSE)
V(ibig_repo)$label <- V(ibig_repo)$name # set labels.
E(ibig_repo)$weight <- link_big$Coding.Effort
V(ibig_repo)$degree <- degree(ibig_repo)
E(ibig_repo)$date <- link_big$Month..Day..Year.of.Date # set labels.

V(ibig_repo)[1:2621]$type = 'Worker'
V(ibig_repo)[2622:4326]$type = 'Repo' 

V(ibig_repo)$degree = degree(ibig_repo)
mean(E(ibig_repo)$weight)


ibig_repo = delete.edges(ibig_repo, which(E(ibig_repo)$weight <= 1))
mean(V(ibig_repo)$degree)


ibig_repo = delete.vertices(ibig_repo, which(V(ibig_repo)[type=='Worker']$degree <= 66))

#ibig_repo = delete.vertices(ibig_repo, which(V(ibig_repo)[type =='Repo']$degree >= 101))


table(V(ibig_repo)$type)

#Repo Worker 
#1705    886

l1 <- layout_with_kk(ibig_repo)

V(ibig_repo)[type == 'Worker']$color <- 'grey'
V(ibig_repo)[type == 'Repo']$color <- 'gold'
V(ibig_repo)$size <- 0.2

V(ibig_repo)[type=='Worker']$label.color <- 'dark grey'
V(ibig_repo)[type=='Repo']$label.color <- 'black'
 
V(ibig_repo)[type == 'Worker']$label.cex <-  .18
V(ibig_repo)[type == 'Repo']$label.cex <- (V(ibig_repo)[type == 'Repo']$degree/(max(V(ibig_repo)[type == 'Repo']$degree)/2)) * 0.7 + .2


E(ibig_repo)$width <- (E(ibig_repo)$weight+.1)/max(E(ibig_repo)$weight+.1) * 0.35
E(ibig_repo)$arrow.size <- .1
E(ibig_repo)$color <- rgb(.5,.5,0,.1)
V(ibig_repo)$frame.color <- NA

plot(ibig_repo, layout=l1)




### One-Mode, Developer to developer

link07 <- read.csv("repo.csv", header=T, row.names=1)
link07 = as.matrix(link07)
dim(link07)


net07 <- graph_from_incidence_matrix(link07,weighted = TRUE)


net.bp <- bipartite.projection(net07)

iproj1 = simplify(net.bp$proj1)

V(iproj1)$degree = degree(iproj1)

### Developers work alone
which(V(iproj1)$degree==0)




iproj1 <- delete.vertices(iproj1, V(iproj1)[degree(iproj1)==0 ])


# iproj1 = delete.vertices(iproj1, which(V(iproj1)$degree <= 26))

mean(E(iproj1)$weight)


# Betweenness centrality gives a higher score to a node that sits on many shortest path of other node pairs
names(sort(betweenness(iproj1),decreasing = TRUE)[1:20])
#[1] "43410636" "43418822" "43400529" "43348316" "43290835" "43347759" "43277458" "43343250"
#[9] "43294351" "43245980" "37006522" "196883"   "43342988" "43423842" "43406714" "43194155"
#[17] "43340158" "43368494" "43419048" "43326028"



# Eigenvector centrality gives a higher score to a node if it connects to many high score nodes
names(sort(evcent(iproj1)$vector,decreasing = TRUE)[1:20])
#[1] "43368220" "43365735" "43368221" "43368218" "43370348" "310073"   "43392237" "43386237"
#[9] "43360753" "43382815" "43391333" "43397119" "43374637" "43362659" "43348643" "43390637"
#[17] "43374630" "43316840" "43368927" "43368338"


pagerank = names(sort(page.rank(iproj1)$vector,decreasing = TRUE)[1:10])
#[1] "43277458" "43401504" "43353031" "43373296" "43356224" "43308593" "43368220" "43365735"
#[9] "43222112" "43410636"


fa <- fastgreedy.community(iproj1)
modularity(fa)
# 0.9203828


le <- leading.eigenvector.community(iproj1)
modularity(le)

lp <- label.propagation.community(iproj1)
modularity(lp)




members <-membership(fa)
sizes(fa) #16


l1 <- layout_with_kk(iproj1)
V(iproj1)$size <- 1.3
V(iproj1)$label.color <- 'black'
V(iproj1)$label.cex <- 0.05


E(iproj1)$width <- 0.2
E(iproj1)$arrow.size <- .1
E(iproj1)$color <- 'grey'
V(iproj1)$frame.color <- NA

plot(iproj1, layout=l1,vertex.color=membership(fa))


### Interactive Graph

iproj1_d3 <- igraph_to_networkD3(iproj1, group = members)

forceNetwork(Links = iproj1_d3$links, Nodes = iproj1_d3$nodes, 
             Source = 'source', Target = 'target', NodeID = 'name', 
             Group = 'group',fontSize = 12,opacity = 1,zoom = TRUE) %>% saveNetwork(file = 'Net1.html')



### Explore with the biggest Community

largest_c <- which.max(sizes(fa))
subg_largest <- induced.subgraph(iproj1, which(membership(fa) == largest_c))  
V(subg_largest)$degree <- degree(subg_largest)
V(subg_largest)$size <- 3
E(subg_largest)$width <- 0.25
V(subg_largest)$label.cex <- (V(subg_largest)$degree/(max(V(subg_largest)$degree)/2)) * 0.05 + .1
V(subg_largest)$label.color <- rgb(0,0,.2,.8)
V(subg_largest)$frame.color <- NA
V(subg_largest)$label = V(subg_largest)$name
E(subg_largest)$color <- rgb(.5,.5,0,.1):
V(subg_largest)$color = 'gold'



plot(subg_largest,layout = layout.auto)



