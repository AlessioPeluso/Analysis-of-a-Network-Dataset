# Analysis-of-a-Network-Dataset

---------------------------------------------------------------------------------------------------------
Background
---------------------------------------------------------------------------------------------------------
A weighted graphwork representing strengths of relationships among confirmed members of a London street gang, 2005-2009. Nodes are gang members, and the edge weight gives the level of relationship (1: hang out together; 2: co-offend together; 3: co-offend together, serious crime; 4: co-offend together, serous crime, kin). Metadata include age, birthplace, residence, arrests, convictions, prison, music, ranking.

---------------------------------------------------------------------------------------------------------
 Script explaining
---------------------------------------------------------------------------------------------------------
In London.R I have loaded the dataset, created the adjacency matrix and made some plots 

In London_Descr.R I answer some important questions for a graph dataset:
-1-  Q1: How connected is the network? 
-2-  Q2: is there a tendency for the nodes in a directed network to return relations?
-3-  Q3: is there a tendency of the nodes in the network to cluster together?
-4-  Q4: are highly connected nodes similar to each other?
-5-  Q5: what can we say about the networks?

In London_Centrality_Measures.R I compute the main Centrality Measures:
-1- degree centrality
-2- closeness centrality
-3- betweenness centrality
-4- eigenvector centrality

In London_Modelling.R I estimate some models:
-1- mod1: homogeneous binomial random graph model
-2- mod 2: non-homogeneous BRG
-3- mod 3:  p1 model
