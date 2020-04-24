# -----------------------------------
# Q1: How connected is the network?
# ----------------------------------- 

# DENSITY
# --------
# Adjacency
graph.density(london.net)

# ------------------------------------
# what can we say about the networks?
# ------------------------------------
# directed or undirected networks?

# conteggio edges

# adjacency matrix

# ---------------------------------------------------------------------------------
# Q2: is there a tendency for the nodes in a directed network to return relations?
# ---------------------------------------------------------------------------------
# mine is undirected

# RECIPROCITY 
# ------------
# R = n. reciprocated ties / n. of observed ties
reciprocity(london.net)

#es.
# Q2: what can we say about the networks?
# in the advice network, reciprocal relations are slightly more frequent 
# than in the friendship network
# reciprocity does not play a role in the report-to network
# this is due to the nature of the relation -- corporate hierarchy


# --------------------------------------------------------------------------
# Q3: is there a tendency of the nodes in the network to cluster together?
# --------------------------------------------------------------------------
# I think only for directed

# ----------------------------------------------------------------
# Q3: how do the three networks differ on this network statistic? 
# ----------------------------------------------------------------
# I only have one

# transitivity coefficient
tr.london = transitivity(london.net)
#prop di osservare una relazione tra due nodi che condividono un "amico" 

#es.
# ----------------------------------------------------------------
# how do the three networks differ wrt this network statistic? 
# ----------------------------------------------------------------
# there is a higher tencency to create small clusters in the advice network 
# than in the friendship one
# as for the reciprocity, transitivity does not play a role in the report net

# -----------------------------------------
# How to normalize the transitivity index? 
# -----------------------------------------
# let us compare it with the density
dens.london = graph.density(london.net)

# compute the log-odds ratio
# london network
#odd density
odd.dens.london = dens.london/(1-dens.london)
odd.dens.london
#odd transitivity
odd.tr.london = tr.london/(1-tr.london)
odd.tr.london
#odds ratio
odd.tr.london/odd.dens.london # Esempio: ho 3 volte piu' prob di essere collegato ad un nodo se we 
#share a common friend
# the chance of observing a relation between nodes sharing a common relation
# is more than three times higher than that of observing a relation between two
# randomly selected nodes

# --------------------------------------------------
# transitivity -- success and failure probabilities
# --------------------------------------------------
tr.success = c(tr.london)
tr.failure = 1-c(tr.london)
# --------------------------------------------------
# density -- success and failure probabilities
# --------------------------------------------------
de.success = c(dens.london)
de.failure = 1 - c(dens.lonodn)
#
odd.tr = tr.success / tr.failure
odd.de = de.success / de.failure
odd.tr/odd.de

# ------------------------------------------------------
# Q4: are highly connected nodes similar to each other?
# ------------------------------------------------------
# ASSORTATIVE MIXING
# -------------------
# Let us read the file with nodal attributes
head(attr)
#
table(attr$Birthplace)
summary(attr$Age)
# ----------------------
# add vertex attributes
#aggiungo attributi alla mia rete, aggiungo il dipartimento e l'eta'
V(london.net)$Birthplace = attr$Birthplace
V(london.net)$Age = attr$Age
V(london.net)$Ranking = attr$Ranking
V(london.net)$Arrests = attr$Arrests
V(london.net)$Prison = attr$Prison
V(london.net)$Residence = attr$Residence
V(london.net)$Convictions = attr$Convictions
V(london.net)$Music = attr$Music



# ---------------------------------------------------------------------
# let us focus on the categorical variable Birthplace  -- 4 categories
plot(london.net, vertex.size = 20,
     vertex.label.cex = 1.5, main = "London Gang", 
     vertex.color = attr$Ranking, layout = layout_as_star(london.net))
# ---------------------------------------------------------------------
# qualititative attributes
# ---------------------------
# start from modularity
# Q = fraction of observed ties within the same category - 
#     expected fraction of ties within the same category in a random graph
# we always have Q < 1
# negative values --  disassortative mixing
# positive values -- assortative mixing

# then normalize it -- assortative coefficient
# -1 <= r <= 1
# r = -1 -> perfect disassortative mixing
# r = +1 -> perfect assortative mixing
#calcolo rispetto al Dept
assortativity(london.net, V(london.net)$Birthplace, directed = F) 

# ----------------------------------------
# Q6: what can we say about the networks?
# ----------------------------------------

# scalar attributes
#calcolo rispetto all'eta'
Assortativity <- c("Birthplace", assortativity(london.net, V(london.net)$Birthplace) ,
"Age", assortativity(london.net, V(london.net)$Age),
"Ranking",assortativity(london.net, V(london.net)$Ranking),
"Arrests", assortativity(london.net, V(london.net)$Arrests),
"Prison", assortativity(london.net, V(london.net)$Prison),
"Convictions", assortativity(london.net, V(london.net)$Convictions),
"Residence", assortativity(london.net, V(london.net)$Residence),
"Music", assortativity(london.net, V(london.net)$Music))



# DA FINIRE PER GLI ALTRI ATTRIBUTI