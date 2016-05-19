## SOCIAL MEDIA IMPLEMENTATION FOR DEVELOPERS





library(igraph)
library(shiny)
library(networkD3)
library(magrittr)

setwd("C:/Users/Miya/OneDrive/BlueOptima")
link07 <- read.csv("repo.csv", header=T, row.names=1)
link07 = as.matrix(link07)
dim(link07)
## Read the matrix into a graph object

### WORKER TO WORKER

link07w <- link07 %*% t(link07)
link07w <- link07w/diag(link07w)
link07w[is.na(link07w)] <- 0
plot(density(link07w))
link07wless <- link07w
link07wless[link07wless<100] <- 0
i07w <- graph.adjacency(link07wless, weight = TRUE)
i07w <- simplify(i07w,remove.multiple=FALSE)
V(i07w)$degree <- degree(i07w)


V(i07w)$label <- V(i07w)$name
V(i07w)$label.cex <- (V(i07w)$degree/(max(V(i07w)$degree)/2)) * 0.8+ .08
V(i07w)$label.color <- rgb(0,0,.2,.8)
V(i07w)$size <- 2
V(i07w)$frame.color <- NA
V(i07w)$color <- rgb(0,0,4,.5)
E(i07w)$arrow.size <- .5
# Set edge gamma according to edge weight
egam <- (E(i07w)$weight+.1)/max(E(i07w)$weight+.1)
E(i07w)$color <- rgb(.5,.5,0,egam)
plot(i07w,layout=layout.kamada.kawai)

### REPO TO REPO 

link07r <- t(link07) %*% link07
link07r <- link07r/diag(link07r)
link07r[is.na(link07r)] <- 0
plot(density(link07r))
link07rless <- link07r
link07rless[link07rless<5000] <- 0
i07r <- graph.adjacency(link07rless, weight = TRUE)
i07r <- simplify(i07r,remove.multiple=FALSE)
V(i07r)$degree <- degree(i07r)


V(i07r)$label <- V(i07r)$name
V(i07r)$label.cex <- (V(i07r)$degree/(max(V(i07r)$degree)/2)) * 0.8+ .08
V(i07r)$label.color <- rgb(0,0,.2,.8)
V(i07r)$size <- 2
V(i07r)$frame.color <- NA
V(i07r)$color <- rgb(0,0,4,.5)
E(i07r)$arrow.size <- .3
# Set edge gamma according to edge weight
egam <- (E(i07r)$weight+.1)/max(E(i07r)$weight+.1)
E(i07r)$color <- rgb(.5,.5,0,egam)
plot(i07r,layout=layout.kamada.kawai)


# Plot Two-Modes Graph


## In graph, bipartite networks have a node attribute called
## type that is False(0) for vertise in one mode and True(1)
## for those in the other mode

#FALSE  TRUE
#  2621  1705 
plot(density(link07))

link07less <- link07
link07less[link07less<250] <- 0
net07 <- graph_from_incidence_matrix(link07less,weighted = TRUE)
net07 = simplify(net07,remove.multiple=FALSE)
table(V(net07)$type)

l1 <- layout_with_kk(net07)

V(net07)$degree <- degree(net07)
V(net07)$label[1:2621] <- V(net07)$name[1:2621]
V(net07)$label[2622:4326] <- V(net07)$name[2622:4326]
V(net07)$color[1:2621] <- 'orange'
V(net07)$color[2622:4326] <- 'sky blue'
V(net07)$label.color <- rgb(0,0,.2,.8)
V(net07)$size[1:2621] <- 1
V(net07)$size[2622:4326] <- 3
V(net07)$label.cex <- (V(net07)$degree/(max(V(net07)$degree)/2)) * 0.5+ .07
E(net07)$curved <- FALSE
E(net07)$width <- (E(net07)$weight+.1)/max(E(net07)$weight+.1) * 0.35
E(net07)$arrow.size <- .2
E(net07)$color <- rgb(.5,.5,0,.1)
egam <- (E(net07)$weight+.1)/max(E(net07)$weight+.1)
E(net07)$color <- 'gray'
V(net07)$frame.color <- NA
#plot(net, layout=layout.kamada.kawai,vertex.label=NA)
plot(net07, layout=l1)



# Plot One-Mode Graph


## generate bipartite projections for the two-mode network:
## to calculate worker-memberships
net07 <- graph_from_incidence_matrix(link07,weighted = TRUE)


net.bp <- bipartite.projection(net07)
#l2 <- layout_components(net.bp$proj1)
#l3 <- layout_with_mds(net.bp$proj1)
#l4 <- layout.fruchterman.reingold(net.bp$proj1)
iproj1 = simplify(net.bp$proj1)
iproj1 <- delete.vertices(iproj1, V(iproj1)[degree(iproj1)==0 ])
graph.density(iproj1)
plot(degree.distribution(iproj1))
lines(degree.distribution(iproj1))
edge.connectivity(iproj1)
sort(closeness(iproj1),decreasing = TRUE)[1:20] # Closeness centrality gives a higher score to a node that has short path distance to every other nodes
196883     43342988     43423842     43410636     43418822     43393865     43393574 
2.337339e-07 2.337339e-07 2.337339e-07 2.337337e-07 2.337297e-07 2.337228e-07 2.337214e-07 
43348316     43399265     43420133     43360263     43376633     43272379     43323637 
2.337212e-07 2.337204e-07 2.337203e-07 2.337181e-07 2.337179e-07 2.337176e-07 2.337176e-07 
43401874     43403175     43414295     43423677     43424253     43427991 
2.337176e-07 2.337176e-07 2.337176e-07 2.337176e-07 2.337176e-07 2.337176e-07 
# Betweenness centrality gives a higher score to a node that sits on many shortest path of other node pairs
sort(betweenness(iproj1),decreasing = TRUE)[1:20]
43410636  43418822  43400529  43348316  43290835  43347759  43277458  43343250  43294351 
100531.71  82817.45  81569.32  81325.18  80850.70  79745.38  50197.47  49664.11  46135.42 
43245980  37006522    196883  43342988  43423842  43406714  43194155  43340158  43368494 
35751.57  35266.22  30700.99  30700.99  30700.99  27765.63  27667.18  26308.46  25806.46 
43419048  43326028 
22774.64  22453.51 
# Eigenvector centrality gives a higher score to a node if it connects to many high score nodes
sort(evcent(iproj1)$vector,decreasing = TRUE)[1:20]
43368220  43365735  43368221  43368218  43370348    310073  43392237  43386237  43360753 
1.0000000 0.9993668 0.7996859 0.7535355 0.7369653 0.7214041 0.7144750 0.7110622 0.6950917 
43382815  43391333  43397119  43374637  43362659  43348643  43390637  43374630  43316840 
0.6328147 0.6296512 0.6134401 0.5771065 0.5651920 0.5138596 0.5117680 0.4888288 0.4288666 
43368927  43368338 
0.4238278 0.4196130

# Degree centrality gives a higher score to a node that has a high in/out-degree
sort(degree(iproj1),decreasing = TRUE)[1:50]
43277458 43327396 43168106 43352379 43390513 43400964 74001005 43284585 43312915 43409136 43393595 
143      115      112      106      106      106      106      105      105      105      104 
43264650 43420758   169229 43247973 43314850 43363283 43395875   288006 43184233 43209772 43213748 
103      103      102      102      102      102      102      101      101      101      101 
43322624 43324920 43389662 43419826 43420218 81037131 43226574 43309023 43313307 43378297 43404023 
101      101      101      101      101      101      100      100      100      100      100 
43410636 43186259 43251781 43305542 43236603 43260989 43347986 43386831 43414902 43201236 43329022 
100       99       99       99       98       98       98       98       98       97       97 
43404713 43405791 43414297 43176290 43214660 43227320 
97       97       97       96       96       96 
#Local cluster coefficient measures how my neighbors are inter-connected with each other, which means the node becomes less important.
clusters(iproj1)$no

# From his studies, Drew Conway has found that people with low Eigenvector centrality but high Betweenness centrality are important gate keepers, while people with high Eigenvector centrality but low Betweenness centrality has direct contact to important persons.  So lets plot Eigenvector centrality against Betweenness centrality.
plot(evcent(iproj1)$vector, betweenness(iproj1))
text(evcent(iproj1)$vector, betweenness(iproj1), 0:100,cex=0.6, pos=4)


l1 <- layout_with_kk(iproj1)
#V(iproj1)$label <- V(iproj1)$name
#V(iproj1)$label.color <- rgb(0,0,.2,.8)
#V(iproj1)$label.cex <- .6
#V(iproj1)$size <- 5
V(iproj1)$color <- rgb(0,0,1,.5)
E(iproj1)$color <- rgb(0,0,0,alpha=.2)
ego1 <- names(which.max(degree(iproj1)))
ego2 <- names(which.max(betweenness(iproj1)))
ego3 <- names(which.max(closeness(iproj1)))
ego4 <- names(which.max(evcent(iproj1)$vector))

V(iproj1)[V(iproj1) != ego1]$color= 'blue'
# make the edges nearly transparent
# and slightly yellow because there will be so many edges in this graph:
V(iproj1)[ego1]$color = 'red'

V(iproj1)[V(iproj1) != ego1]$size=1
V(iproj1)[ego1]$size = 5


windows()
plot(iproj1, vertex.label=NA,layout=l1)

## Community Detection
wc <- walktrap.community(iproj1)
modularity(wc)
members <-membership(wc)
members
1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24 
156  92  99   5  15  65 101  19   8   4  11  16   5   7   3  12   7   3   3   3  59   2   4   4 
25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48 
4   8   4   2  18  29   3  16   6  14   3   3  94   7   5   5   4  18  13   6  26   8  11  20 
49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72 
7   5   4   8   4   4  40  11   9   7   5   3  13   3   3  11   9   6   5  14   7  20   6   3 
73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96 
15   4  14   9   5   5   9   4   7   8   2   5   7  23  25  10  10  41  10  10  12  69  25  72 
97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 
4   4   3   8   6   7  36  40  11  12   5   3   2   5   6   6  22  13   8   6   3  10  34   3 
121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 
3   3   3   2   2   2   4   2   3   9   3   2   2   6   3  23   9   3   5   3   3   6   6   4 
145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 
2   6   4   3   5  10   5   4   4   4   3   2   5   3  14   5   8   4   5   3   4  13   4   4 
169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 
4   3   4   4   4   4   4   2   6   3  13  30   5   9   5   4   4   2   4   3   2   2   2   2 
193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 
2   2   3   3   2   3   6   2   2   2   3   2   2   2   6   2   3   7   3   6   3   2   2   9 
217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 
6   8   4   5   2   2   2   2   2   5   2   2   2   2   2   2   2   2   3   2   1   1 
iproj1_d3 <- igraph_to_networkD3(iproj1, group = members)

forceNetwork(Links = iproj1_d3$links, Nodes = iproj1_d3$nodes, 
             Source = 'source', Target = 'target', NodeID = 'name', 
             Group = 'group',fontSize = 12,opacity = 1,zoom = TRUE) %>% saveNetwork(file = 'Net1.html')




## WEB APP DEPLOYMENT

server <- function(input, output) {output$force <- renderForceNetwork({
  forceNetwork(Links = iproj1_d3$links, Nodes = iproj1_d3$nodes, 
               Source = 'source', Target = 'target', NodeID = 'name', 
               Group = 'group')
})}

#### UI ####

ui <- shinyUI(fluidPage(
  
  titlePanel("Shiny networkD3 "),
  sidebarLayout(
    sidebarPanel(
      sliderInput("opacity", "Opacity", 0.6, min = 0.1,
                  max = 1, step = .1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Force Network", forceNetworkOutput("force"))
      )
    )
  )
))

#### Run ####
shinyApp(ui = ui, server = server)


###TRY DIFFERENT COMMUNITY DETECTION METHODS

#  0.9265684
fa <- fastgreedy.community(iproj1)
modularity(fa)
# 0.9240254
E(iproj1)$weight <- count.multiple(iproj1)

sp <- spinglass.community(iproj1)
modularity(sp)
#

le <- leading.eigenvector.community(iproj1)
modularity(le)
# 0.9262529

lp <- label.propagation.community(iproj1)
modularity(lp)
# 0.902857

windows()
plot(iproj1, vertex.label=NA,vertex.size=3, vertex.color=membership(wc),
     layout=l1)
