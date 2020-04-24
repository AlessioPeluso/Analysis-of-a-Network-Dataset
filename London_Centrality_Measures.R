# ---------------------
# CENTRALITY MEASURES 
# ---------------------
# INDEX:
# 1) degree centrality
# 2) closeness centrality
# 3) betweenness centrality
# 4) eigenvector centrality

# 1) DEGREE CENTRALITY
# ---------------------
# A node is central if it is connected to many other nodes
# zeta_i^d = sum_j y_ij
adj <- ifelse(dati>1,1,dati)
london.net <- graph_from_adjacency_matrix(adj, mode = "undirected", diag = F)
V(london.net)$label = 1:nrow(adj)
  
zid.london = rowSums(adj, na.rm = T)
zid.london

summary(zid.london)

# let us graphically represent the degree distribution 
par(mfrow = c(1,1), mar = c(4.5,4.5,4.5,4.5))
barplot(table(zid.london), col = "peachpuff", xlab = "degree centrality" , ylab = "number of nodes")

#to simplify comparison... 
# standardized degree centrality
# -------------------------------
n = vcount(london.net)
zid.st.london = zid.london/(n-1)

# let us consider the summary of the standardized degree centralities
summary(zid.st.london)
# what can we say about nodes?
# -------------------------------
# ES.
# london NET: 
# most of the nodes have from moderately high to high centrality
# FRIEND NET: 
# most of the nodes have from moderately low centrality, the maximum is however close to 1
# REPORT NET: 
# almost all nodes have very low centrality

# let us graphically represent standardized centrality
par(mfrow = c(1,1), mar=c(1,1,1,1))
plot(london.net, vertex.size = zid.st.london*50, vertex.label.cex = 0.5,
       layout=layout.sphere(london.net))
# what can we say about centrality of the nodes?
# si potrebbe dire che non ? troppo omogenea, quindi non ? troppo centralised

# color in blue the top 3 nodes
ord = order(zid.st.london, decreasing = T)
V(london.net)$color = "orange"
V(london.net)$color[ord[1:3]] = "lightblue"

plot(london.net, vertex.size = zid.st.london*50, vertex.label.cex = 0.5,
     layout=layout.sphere(london.net))
#colora di celeste i tre nodi con degree centrality maggiore, per vedere il loro posizionamento all'interno della rete

# degree centrality
degree(london.net)
zid.london
degree(london.net, normalized = T)
zid.st.london


# 2) CLOSENESS CENTRALITY
# ------------------------
# A node is central if it is "close" to many other nodes
# z_i^c = 1/sum_j d_ij
D.london = distances(london.net)
D.london 
zic.london = 1/rowSums(D.london)

# standardized closeness centrality
# ----------------------------------
zic.st.london = zic.london*(n-1)
summary(zic.st.london)

# let us graphically represent standardized centrality
par(mfrow = c(1,1), mar = c(0,0,2,0))
# color in blue the top 3 nodes

ord = order(zic.st.london, decreasing = T)
V(london.net)$color = "orange"
V(london.net)$color[ord[1:3]] = "lightblue"
plot(london.net, vertex.size = zic.st.london*30, vertex.label.cex = 0.5,
     layout=layout.sphere(london.net))

# more easily...
# -----------------
# closeness centrality
closeness(london.net.sy)
zic.london

closeness(london.net.sy, normalized = T)
zic.st.london

# 3) BETWEENNESS CENTRALITY
# --------------------------
# a node is central if it is located between many nodes

zib.london = betweenness(london.net, directed = F, normalized = F)
summary(zib.london)

zib.st.london = betweenness(london.net, directed = F, normalized = T)
summary(zib.st.london)

# let us graphically represent standardized centrality
par(mfrow = c(1,1), mar = c(0,0,0,0))
# color in blue the top 3 nodes

ord = order(zib.st.london, decreasing = T)
V(london.net)$color = "orange"
V(london.net)$color[ord[1:3]] = "lightblue"
plot(london.net, vertex.size = zib.st.london*300, vertex.label.cex = 0.5,
     layout=layout.sphere(london.net))

# 4) EIGENVECTOR CENTRALITY
# --------------------------
# a node is central if it is connected to other central nodes
zie.st.london = eigen_centrality(london.net, scale = T)$vector
summary(zie.st.london)


# let us graphically represent standardized centrality
par(mfrow = c(1,1), mar = c(0,0,0,0))
# color in blue the top 3 nodes

ord = order(zie.st.london, decreasing = T)
V(london.net)$color = "orange"
V(london.net)$color[ord[1:7]] = "lightblue"

plot(london.net, vertex.size = zie.st.london*40, vertex.label.cex = 0.5,
     layout=layout.sphere(london.net))



# --------------------------
# NETWORK CENTRALIZATION
# --------------------------

# 1) Degree centralization
# -------------------------
d.centralization = function(c){
  n = length(c)
  max.cd = max(c)
  num = sum(max(c) - c)
  den = (n-1)*(n-2)
  return(num/den)
}


# 2) Closeness centralization
# ----------------------------
c.centralization = function(c){
  n = length(c)
  max.cd = max(c)
  num = sum(max(c) - c)
  den = (n-2)/(2*n-3)
  return(num/den)
}


# 3) betweenness centralization
# ---------------------------
b.centralization = function(c){
  n = length(c)
  max.cd = max(c)
  num = sum(max(c) - c)
  den = (n-1)^2*(n-2)/2
  return(num/den)
}

centr_degree(london.net, loops = F)$centralization
centr_clo((london.net))$centralization
centr_betw(london.net, directed = F)$centralization
