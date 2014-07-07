## experiment: calculate the breakpoint error and show the curve that
## demonstrates what exponent we need to pick for the signal length.

source("data/precise.breakpoint.cost.R")
source("data/run.cghseg.R")


sample.signal <- function(until){
  locations <- round(seq(1,until,l=signal.size))
  last <- locations[length(locations)]
  signal <- noisy.signal[locations]
  result <- run.cghseg(signal,round(until/500))
  result$mu <- mu[locations]
  result$mu.breaks <- locations[which(diff(result$mu)!=0)]
  result$locations <- locations
  result$signal <- signal
  stopifnot(sum(is.na(result$signal))==0)
  result$size <- until
  result$breaks <- apply(result$smooth,2,function(sm){
    break.after.i <- which(diff(sm)!=0)
    break.after <- locations[break.after.i]
    break.before <- locations[break.after.i+1]
    floor((break.after+break.before)/2)
  })
  result$cost <- sapply(result$breaks,function(x){
    bkpt.err(result$mu.breaks,x,last)
  })
  result$crit <- function(lambda,alpha){
    J <- result$J.est
    Kseq <- seq_along(J)
    lambda * Kseq * result$size^alpha + J
  }
  result$lambda <- 10^seq(-5,13,l=200)
  result$kstar <- function(lambda,a){
    which.min(result$crit(lambda,a))
  }
  result$lambda.error <- function(a){
    k <- sapply(result$lambda,result$kstar,a)
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

## the mean signal (green line)
seg.size <- 1000
means <- rep(c(-3,0,3,0,2,-1,3),100)/2.5
mu <- do.call(c,lapply(means,rep,seg.size))
locations <- seq_along(mu)

## the noisy signal (black points)
set.seed(1)
noisy.signal <- rnorm(length(mu),mu)

go.until <- c(2000,7000,5000,10000,20000,40000,80000)
signal.size <- min(go.until)
n.signals <- length(go.until)
variable.breaks.constant.size <- list()
for(sig.i in 1:n.signals){
  cat(sprintf("simulating signal %4d / %4d\n",sig.i,n.signals))
  size <- go.until[sig.i]
  variable.breaks.constant.size[[sig.i]] <- sample.signal(size)
}

save(variable.breaks.constant.size,
     file="data/variable.breaks.constant.size.RData")

