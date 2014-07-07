## experiment: calculate the breakpoint error and show the curve that
## demonstrates what exponent we need to pick for the estimate of
## variance.

source("data/precise.breakpoint.cost.R")
source("data/run.cghseg.R")

seg.size <- 100
means <- c(-3,0,3,0,2,-1,3)/3
mu <- do.call(c,lapply(means,rep,seg.size))
locations <- seq_along(mu)
d <- length(mu)
mu.break.after <- which(diff(mu)!=0)

sample.signal <- function(signal,mu){
  result <- run.cghseg(signal)
  result$mu <- mu
  result$locations <- locations
  result$signal <- signal
  result$med.abs.diff <- median(abs(diff(signal)))
  result$breaks <- apply(result$smooth,2,function(sm){
    break.after.i <- which(diff(sm)!=0)
    break.after <- locations[break.after.i]
    break.before <- locations[break.after.i+1]
    floor((break.after+break.before)/2)
  })
  result$cost <- sapply(result$breaks,function(x){
    bkpt.err(mu.break.after,x,d)
  })
  result$crit <- function(lambda,alpha){
    J <- result$J.est
    Kseq <- seq_along(J)
    lambda * Kseq * result$med.abs.diff^alpha + J
  }
  result$lambda <- 10^seq(-8,9,l=200)
  result$kstar <- function(lambda,a){
    which.min(result$crit(lambda,a))
  }
  result$lambda.error <- function(a){
    lseq <- result$lambda
    k <- sapply(lseq,result$kstar,a)
    result$cost[k]
  }
  result$lambda.star <- function(a){
    error <- result$lambda.error(a) 
    l <- which(error == min(error))
    chosen <- l[ceiling(length(l)/2)]
    result$lambda[chosen]
  }
  ##plot(result$cost,type="o")
  result
}
set.seed(1)
variable.scale.signals <- list()
n.signals <- 4
multiply.by <- 10^seq(0,3,l=n.signals)
sig <- rnorm(d,mu)
for(sig.i in 1:n.signals){
  cat(sprintf("simulating signal %4d / %4d\n",sig.i,n.signals))
  fact <- multiply.by[sig.i]
  sig.scaled <- sig * fact
  variable.scale.signals[[sig.i]] <- sample.signal(sig.scaled,mu*fact)
}

save(variable.scale.signals,file="data/variable.scale.signals.RData")

