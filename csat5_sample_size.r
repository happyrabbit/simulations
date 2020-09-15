simulation <- function(p,n,nintr = 1000){
  # p: a list of probability
  # n: sample size
  # nintr: number of iterations
  ps = NULL
  a = c(5,4,3,2,1)
  for (i in 1:nintr){
    s1 = sample(a, n, prob = p, replace = T)
    ps = c(ps,mean(s1 >= 4)) 
  }
  
  q1 = quantile(ps,0.05)
  q2 = quantile(ps,0.5)
  q3 = quantile(ps,0.95)
  ptop2 = p[1]+p[2]
  delta = (q3-q1)/2
  
  return(c(ptop2 = ptop2,size = n, q1, q2, q3, delta = delta))
}

p1 = c(0.2,0.2,0.3,0.2,0.1)
p2 = c(0.3,0.2,0.2,0.2,0.1)
p3 = c(0.3,0.3,0.2,0.1,0.1)
p4 = c(0.5,0.2,0.1,0.1,0.1)
p5 = c(0.5,0.3,0.05,0.1,0.05)

pls = list(p1,p2,p3,p4,p5)
ns = c(10,30,50,100,200,300,400,500,1000)

res = NULL

for (l in 1:length(pls)) {
  for (i in seq_along(ns)) {
    res = rbind(res,simulation(p = pls[[l]], n = ns[i]))
  }
}

res = data.frame(res)
names(res) <- c("top2pct","size",'q5','q50','q95','error')
res$top2pct = as.factor(res$top2pct)
res$size = as.factor(res$size)

library(ggplot2)
ggplot(res, aes(top2pct, size)) +
  geom_tile(aes(fill = error)) + 
  geom_text(aes(label = round(error, 2))) +
  scale_fill_gradient(low = "white", high = "orange") 
