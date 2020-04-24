#---------------------------------------------------------------------------------------------------------
# Set working directory
#----------------------
setwd()

#---------------------------------------------------------------------------------------------------------
# Libraries
#----------
library(igraph)
#library(tidyr)

#---------------------------------------------------------------------------------------------------------
# Background
#-----------
# A weighted graphwork representing strengths of relationships among confirmed members of a London street 
# gang, 2005-2009. Nodes are gang members, and the edge weight gives the level of relationship (1: hang 
# out together; 2: co-offend together; 3: co-offend together, serious crime; 4: co-offend together, 
# serous crime, kin). Metadata include age, birthplace, residence, arrests, convictions, prison, music, 
# ranking.

#---------------------------------------------------------------------------------------------------------
# Loading data
#-------------------
dati <- read.csv("LONDON_GANG.csv", row.names=1)
attr <- read.csv("LONDON_GANG_ATTR.csv")

#---------------------------------------------------------------------------------------------------------
# Summary statistics
#--------------------
names(attr)
# Age
par(mar=c(3,3,3,3))
sd(attr$Age)
hist(attr$Age, col = "peachpuff" , main ="",prob = T)
lines(density(attr$Age))
#
table(attr$Birthplace)
# c("West Africa","Caribbean","UK","East Africa")
hist(attr$Birthplace, col = "turquoise", main = "")
var1 <- c(rep("West Africa",12), rep("Caribbean",12), rep("UK",24), rep("East Africa",6))
barplot(table(var1), col = "turquoise", main ="")
#
table(attr$Residence)
#
table(attr$Music)

# arrest conviction prison ranking 
mean(attr$Arrests)
sd(attr$Arrests)
hist(attr$Arrests, col = "gold", main = "", prob = T)
lines(density(attr$Arrests))
#
mean(attr$Convictions)
sd(attr$Convictions)
hist(attr$Convictions, col = "orange", main = "", prob = T)
lines(density(attr$Convictions))
#
table(attr$Prison)
#
table(attr$Ranking)
par(mar=c(3,3,1,3))
barplot(table(attr$Ranking),col = "yellowgreen", main = "")

#---------------------------------------------------------------------------------------------------------
# Create Adjacency Matrix and Graph
#--------------------
# as.matrix
dati <- as.matrix(dati)
# diag <- Na
diag(dati) <- NA
# graph
london.net <- graph_from_adjacency_matrix(dati, weighted = T, mode = "undirected", diag = F)
london.net1 <- london.net
# edge and nodes
ecount(london.net)
vcount(london.net)

#---------------------------------------------------------------------------------------------------------
# Create Plots
#--------------------

#-------------
# Classic plot
#-------------
E(london.net1)$width = E(london.net1)$weight
london.net1 <- simplify(london.net1, remove.multiple = F, remove.loops = T)
par(mar=c(1,2,2,1))
plot(london.net1, layout=layout_on_sphere(london.net1), vertex.label = NA, main = "London Gang Network")

#----------------------------
# Colored plot for Birthplace
#----------------------------
# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(london.net)$label <- NA
# V(london.net)$size <- attr$Arrests*2
colrs <- c("gray50", "tomato", "gold", "navy")
V(london.net)$color <- colrs[attr$Birthplace]
# Set edge width based on weight:
# E(london.net)$width <- E(london.net)$weight/24
# change arrow size and edge color:
# E(london.net)$arrow.size <- .2
E(london.net)$edge.color <- "gray80"
# E(london.net)$width <- 1+E(london.net)$weight/24

# plot w\ birthplace
par(mar=c(1,1,1,1))
plot(london.net, edge.curved=0, layout = layout_on_sphere(london.net))
legend("topright", legend=c("West Africa","Caribbean","UK","East Africa"), col = c("gray50", "tomato", "gold", "navy"), 
       fill=colrs ,cex=0.8)
# legend("bottomright", legend = "size proportional \n to the number of arrests", cex = 0.8, box.lty=0)


