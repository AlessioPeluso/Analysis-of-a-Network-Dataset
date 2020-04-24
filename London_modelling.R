#--------------------------------------------#
#--------------------------------------------#
#                  Lab 6                     #
#                  ERGMs                     #
#--------------------------------------------#
#--------------------------------------------#

# rm(list = ls())

# -----------------------#
# ------ 1. Set-up ----- #
# -----------------------#
library(igraph)


# estimating exponential random graph models 
# ---------------------------------------------


# -------- mod1: homogeneous binomial random graph model --------

Y = get.adjacency(london.net, sparse = F) 
diag(Y) = NA
p.MLE = mean(Y, na.rm = T)
p.MLE # p bern == media campionaria
# la prob di oss un tie tra due nodi random e' 0.22 
# => la prob di non oss un tie e' maggiore (0.78)


# how can we obtain an equivalent result by considering the  ERGM specification?
# ergm package
library(ergm) 
# ?ergm
# what shoud we note? 
# 1) the function allows to estimate an ergm 
# 2) estimation methods whiich are implemented are: 
#    - maximum-pseude likelihood (MPLE)
#    - approximate monte Markov Chain Monte Carlo ML (MLE)
#    - CD -- another approximate estimating procedure (experimental)
# 3) In the formula, the response is a "network" object 


# to start, let us transform the adjacency matrix in a network object
# ?network
net = network(Y, directed = T)
class(net)
net
summary(net)


# let us estimate parameters of a BRG model via the ergm function
# which is the sufficient statistic for theta? 
# y.. -> the number of observed edges

brg = ergm(net ~ edges, estimate = "MPLE")
brg
# MPLE Coefficients:
#   edges  
# -1.265  

# as usual, some more results?
summary(brg) # For this model, the pseudolikelihood is the same as the likelihood.
# which information do we have? 
# 1) formula: recall the estimated model
# 2) how many iterations for convergence? NA iterations
# 3) estimates -- in this case, as independence holds, 
#    a standar ML approach is used (besides the title!)
# 4) as this model simply corresponds to a logistic regression
#    we can interpret the results as usual

# look at the sign of the estimate 
# -- negative value -- that is, Y_{ij} = 0 is more likely than Y_{ij} = 1

# we can think about odds and compute
odds = exp(brg$coef)
odds # 0.2822581
# odds <1
1-odds # 0.7177419
# the risk of not observing a tie is 71% lower than that of observing a tie


# furhtermore, by applying the expit transform, we have
exp(brg$coef)/(1+exp(brg$coef))
# which is exactly equal to the density of the network
# that is it is equal to p.MLE
p.MLE



# ------ mod 2: non-homogeneous BRG ---------
# sender and receiver effects
rowIdx = row(Y)
colIdx = col(Y)
rowIdx[1:4, 1:4]
colIdx[1:4, 1:4]

y = c(Y)
rowidx = c(rowIdx)
colidx = c(colIdx)

mod = glm(y ~ factor(rowidx) + factor(colidx), family = "binomial")
mod$coefficients
summary(mod)
# row <- sender effect
# column <- receiver effect

# how can we obtain an equivalent result by considering the ERGM specification?

# which are the sufficient statistics for the vector theta? 
# y.. -> that is, the number of edges
# yi. -> that is, the set of out-degrees
# y.j -> that is, the set of in-degrees
nh_brg = ergm(net ~ edges + sender + receiver, estimate = "MPLE")
nh_brg

# as usual, some more results?
summary(nh_brg)


# looking at the estimated parameters
head(mod$coefficients)
head(nh_brg$coef)
# sono gli stessi

# to interpret these parameters, let us consider the exponential transform
exp(nh_brg$coef)
# what can we say by looking at the results? 
# for instance, the chance of sending a tie for node 3 and 4 is much higher 
# than that of node 1 (12 times higher for node 3, 5 times higher for node 4)

# indeed... 
rowSums(Y, na.rm=T)


# which model fits better to the observed data? 

# let us give a look to the likleihood 
brg$mle.lik
nh_brg$mle.lik 

#  models selection via AIC or BIC
AIC(brg)
AIC(nh_brg) # questo e' migliore

BIC(brg)
BIC(nh_brg) 
# as expected, the bic is more conservative, 
# it leads to select the more parsimonious model


#  ----------- mod 3:  p1 model --------
# which are the sufficient statistics? 
# y.. -> that is, the number of edges
# yi. -> that is, the set of out-degrees
# y.j -> that is, the set of in-degrees
# gamma --> that is, the number of reciprocal ties

p1 = ergm(net ~ edges + sender + receiver + mutual, estimate = "MPLE")
# In this case, due to model complexity, an approximate procedure is required to estimate parameters
summary(p1)
# in questo caso non ho MPLE == MLE quindi il modello e' basato su una pseudo-likelihood

# how can we interpret the mutuality parameter? 
# 1) significant effect --> mutuality plays a role in this network
# 2) positive sign indicating a higher chance to return a tie than that 
#    expected by chance

# in particular, 
exp(p1$coef[length(p1$coef)])
# the chance of returning a tie is 9 times higher than that expected by chance
# (under the independence assumption of tie-variables Y_{ij})

# confronto i modelli
BIC(p1, nh_brg, brg) # preferisce il piu' semplice ma di poco rispetto a p1
AIC(p1, nh_brg, brg) # p1 e' il preferibile

# ----- mod 4 -- Markov graph model -------
# let us consider the undirected network
advice.net.sy = as.undirected(advice.net, mode = "mutual")
Y.sy = get.adjacency(advice.net.sy, sparse = F)

net.sy = network(Y.sy, directed = F)

# which are the sufficient statistics? 
# y.. -> that is, the number of edges
# starts -- let us consider only 2/3 stars
# triangles 

mark1 = ergm(net.sy ~ edges +  kstar(2) + triangle, estimate = "MPLE")
summary(mark1)

# triangles do not seem to play a role in determining the network
# 2-stars are instead significant
# the positive sign indicates a positive tendency to form 2-stars among nodes


mark2 = ergm(net.sy ~ edges +  kstar(2), estimate = "MPLE")
summary(mark2)

# which model shoud we prefer? 
AIC(mark1, mark2)
BIC(mark1, mark2)



# what can we do for the directed network? 
# istar <- ingresso
# ostar <- uscita
mark3 = ergm(net ~ edges +  istar(2) + ostar(2) + istar(3) + ostar(3), estimate = "MPLE")
summary(mark3) 

# again, stars of size 3 are not significant
mark4 = ergm(net ~ edges +  istar(2) + ostar(2), estimate = "MPLE")
summary(mark4)

# which model should we prefer? 
AIC(mark3, mark4)
BIC(mark3, mark4)


# what about triangles? 
mark5 = ergm(net ~ edges +  istar(2) + ostar(2) + triangles, estimate = "MPLE")
summary(mark5)
# not significant
