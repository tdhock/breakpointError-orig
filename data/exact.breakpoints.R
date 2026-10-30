### This script saves the exact breakpoints in lambda for the cghseg.k
### model selection criterion. 

lambda_sequence <- structure(function # Calculation of lambda
### Given a set of optimal cost from 1 segments to K segments, return a
### set of consecutive (k, lambda) with k being the optimal seg for
### for lambda k is the best up to the next lambda store in the list
(cost,
### numeric vector: optimal costs.
 n
### integer: length of signal.
 ){
  Kmax = length(cost)
  Kcurrent = Kmax
  Lcurrent = 0

  vK = Kmax
  vL = 0
  i <- 2
  while(Kcurrent > 1){
    smallerK <- 1:(Kcurrent-1)
    ## DO NOT DIVIDE BY N, then we can use the log.n feature and the
    ## L1 norm will drive it to zero, not to 1.
    cost.term <- (cost[Kcurrent] - cost[smallerK]) ##/ n
    lambdaTransition <-  cost.term / ( smallerK - Kcurrent)
    Kcurrent <- which.min(lambdaTransition)
    Lcurrent <- min(lambdaTransition)
    vL[i] <- Lcurrent
    vK[i] <- Kcurrent
    i <- i+1
  }
  breakpoints <- rep(NA,length(cost))
  breakpoints[vK] <- vL
  ## vL[i] stores the smallest lambda such that vK[i] segments is
  ## optimal. i
  list(K=vK, lambda=vL, breakpoints=breakpoints)
},ex=function(){
  n=3*10^4
  x = rnorm(n) + rep(c(0, 1, 0), c(n/3, n/3, n/3))
  res = cghseg:::segmeanCO(x, K=100)
  Kseq <- seq_along(res$J)
  lambda.grid <- 10^seq(-5,1,l=100)
  kstar.grid <- sapply(lambda.grid,function(lambda){
    which.min(Kseq * lambda + res$J/n)
  })
  plot(log10(lambda.grid),kstar.grid,pch=16)
  la = lambda_sequence(res$J.est,n)
  points(log10(la$lambda),la$K)
})

load("data/segmentation.list.RData")
exact.breakpoints <- lapply(segmentation.list,function(seg.info){
  with(seg.info,lambda_sequence(J.est,n))
})
save(exact.breakpoints,file="data/exact.breakpoints.RData")
