
# 2. Is the observed network coherent with a Binomial random graph model G(n, p)?
# a naive approach based on the best case scenario
# ------------------------------------------------------------------------------------
rho.obs = mean(Y, na.rm = T)
CId.obs = centr_degree(london.net, mode = "in", loops = F)$centralization
C.obs = transitivity(london.net)
# maximum likelihood estimate of p
p.MLE = mean(Y, na.rm = T)

B = 1000
rho.sim = CId.sim = C.sim = c()
for(b in 1:B){
  tmp = rbinom(n^2,1,p.MLE)  
  Y.sim = matrix(tmp, n,n)
  diag(Y.sim) = NA
  rho.sim[b] = mean(Y.sim, na.rm = TRUE)
  g.sim = graph_from_adjacency_matrix(Y.sim)
  CId.sim[b] = centr_degree(g.sim, mode = "in", loops = F)$centralization
  C.sim[b] = transitivity(g.sim)
}

# Graphical comparison
par(mfrow = c(1,3))
low = pmin(min(rho.sim), rho.obs) - 0.05
up = pmax(max(rho.sim), rho.obs) + 0.05
hist(rho.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = rho.obs, col = "red", lwd=2)


low = pmin(min(CId.sim), CId.obs) - 0.05
up = pmax(max(CId.sim), CId.obs) + 0.05
hist(CId.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = CId.obs, col = "red", lwd=2)


low = pmin(min(C.sim), C.obs) - 0.05
up = pmax(max(C.sim), C.obs) + 0.05
hist(C.sim, col = "lightgray", main = "Null distribution", xlim = c(low, up))
abline(v = C.obs, col = "red", lwd=2)


# compute an approximate p-value
mean(rho.sim >= rho.obs)
mean(CId.sim >= CId.obs)
mean(C.sim >= C.obs)
# is the network coherent with the family of BRG models [G(n, p)] in terms of the above statistics?


# 3. Is the observed network coherent with a Binomial random graph model G(n, p)?
# a more formal approach based on the conditional uniform distribution
# ------------------------------------------------------------------------------------

# let us consider the degree centralization index and the transitivity coefficient
# NB. the density statistic makes no sense -- by fixing m, then rho = p is constant 
B = 1000
m =  sum(Y, na.rm = TRUE)
rho.sim = CId.sim = C.sim = c()
for(b in 1:B){
  Y.sim = matrix(, n, n) 
  ones = rep(1, m)
  zeros = rep(0, n*(n-1) - m)
  all = c(ones, zeros)
  Y.sim[col(Y.sim) != row(Y.sim)] = sample(all, n*(n-1))
  g.sim = graph_from_adjacency_matrix(Y.sim)
  CId.sim[b] = centr_degree(g.sim, mode = "in", loops = F)$centralization
  C.sim[b] = transitivity(g.sim)
}


# Graphical comparison
par(mfrow = c(1,2))
hist(CId.sim, col = "lightgray", main = "Null distribution", xlim = c(0,0.3))
abline(v = CId.obs, col = "red", lwd=2)


hist(C.sim, col = "lightgray", main = "Null distribution",xlim = c(0.35,0.55))
abline(v = C.obs, col = "red", lwd=2)


# compute an approximate p-value
mean(CId.sim >= CId.obs)
mean(C.sim >= C.obs)
# is the network coherent with a BRG model G(n, p) in terms of the above statistics? NO