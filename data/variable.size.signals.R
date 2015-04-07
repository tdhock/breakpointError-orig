## experiment: calculate the breakpoint error and show the curve that
## demonstrates what exponent we need to pick for the signal length.

source("data/precise.breakpoint.cost.R")
source("data/run.cghseg.R")


sample.signal <- function(until){
  locations <- 1:until
  signal <- sig[locations]
  result <- run.cghseg(signal,round(until/50))
  result$mu <- mu[locations]
  result$mu.breaks <- which(diff(result$mu)!=0)
  result$locations <- locations
  result$signal <- signal[locations]
  result$size <- until
  result$breaks <- apply(result$smooth,2,function(sm){
    break.after.i <- which(diff(sm)!=0)
    break.after <- locations[break.after.i]
    break.before <- locations[break.after.i+1]
    floor((break.after+break.before)/2)
  })
  result$cost <- sapply(result$breaks,function(x){
    bkpt.err(result$mu.breaks,x,d)
  })
  result$crit <- function(lambda,alpha,beta=0.5){
    J <- result$J.est
    Kseq <- seq_along(J)
    lambda * Kseq * (length(result$mu)/result$size)^alpha + J #constant
    lambda * Kseq * length(result$mu)^alpha + J
    lambda * Kseq * result$size^alpha * length(result$mu)^beta + J 
  }
  result$lambda <- 10^seq(-6,10,l=200)
  result$kstar <- function(lambda,a,b=0.5){
    which.min(result$crit(lambda,a,b))
  }
  result$lambda.error <- function(a,b=0.5){
    k <- sapply(result$lambda,result$kstar,a,b)
    result$cost[k]
  }
  result$lambda.star <- function(a,b=0.5){
    error <- result$lambda.error(a,b)
    l <- which(error == min(error))
    chosen <- l[ceiling(length(l)/2)]
    result$lambda[chosen]
  }
  ##plot(result$cost,type="o")
  result
}

## the mean signal (green line)
seg.size <- 100
means <- rep(c(-3,0,3,0,2,-1,3),100)/3.5
mu <- do.call(c,lapply(means,rep,seg.size))
locations <- seq_along(mu)
d <- length(mu)

## the noisy signal (black points)
set.seed(1)
sig <- rnorm(d,mu)

go.until <- c(200,400,700,1000,2000,4000,7000,10000)
n.signals <- length(go.until)
variable.size.signals <- list()
for(sig.i in 1:n.signals){
  cat(sprintf("simulating signal %4d / %4d\n",sig.i,n.signals))
  size <- go.until[sig.i]
  variable.size.signals[[sig.i]] <- sample.signal(size)
}

save(variable.size.signals,file="data/variable.size.signals.RData")

