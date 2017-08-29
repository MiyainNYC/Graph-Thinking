
library(igraph)

setwd("C:/Users/Miya/Desktop/cna/dataset")

iproj1 = read_graph('g_2.gml', format = "gml")

wt = cluster_walktrap(iproj1)
wt_df = data.frame(c(membership(wt)),c(V(iproj1)$label))
write.csv(wt_df, file = "wt_df.csv",row.names=FALSE)


eb = cluster_edge_betweenness(iproj1)
eb_df = data.frame(c(membership(eb)),c(V(iproj1)$label))
write.csv(eb_df, file = "eb_df.csv",row.names=FALSE)

fg = cluster_fast_greedy(iproj1)
fg_df = data.frame(c(membership(fg)),c(V(iproj1)$label))
write.csv(fg_df, file = "fg_df.csv",row.names=FALSE)


lp = cluster_label_prop(iproj1)
lp_df = data.frame(c(membership(lp)),c(V(iproj1)$label))
write.csv(lp_df, file = "lp_df.csv",row.names=FALSE)

le = cluster_leading_eigen(iproj1)
le_df = data.frame(c(membership(le)),c(V(iproj1)$label))
write.csv(le_df, file = "le_df.csv",row.names=FALSE)

co = cluster_optimal(iproj1)
co_df = data.frame(c(membership(co)),c(V(iproj1)$label))
write.csv(co_df, file = "co_df.csv",row.names=FALSE)

cs = cluster_spinglass(iproj1)
cs_df = data.frame(c(membership(cs)),c(V(iproj1)$label))
write.csv(cs_df, file = "cs_df.csv",row.names=FALSE)

info = cluster_infomap(iproj1)
## ungroup nodes type
table(V(iproj1)[which(sizes(info)==1)]$type)
info_df = data.frame(c(membership(info)),c(V(iproj1)$label))
write.csv(info_df, file = "info_df.csv",row.names=FALSE)


ml = cluster_louvain(iproj1)
members <-membership(ml)
sizes(ml) #16
modularity(ml)
ml_df = data.frame(c(membership(ml)),c(V(iproj1)$label))
write.csv(ml_df, file = "ml_df.csv",row.names=FALSE)
