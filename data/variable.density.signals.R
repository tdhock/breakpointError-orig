## experiment: calculate the breakpoint error and show the curve that
## demonstrates what exponent we need to pick for the number of points.

source("data/precise.breakpoint.cost.R")
source("data/run.cghseg.R")
library(flsa)

sample.signal <- function(d){
  locations <- round(seq(1,length(mu),l=d))
  last <- as.integer(locations[length(locations)])
  this.signal <- signal[locations]
  result <- run.cghseg(this.signal)
  result$mu.break.after <- mu.break.after
  result$bases.per.probe <- factor(round(last/d))
  result$flsa <- flsa
  calc.cost <- function(breaks){
    bkpt.err(mu.break.after,breaks,last)
  }
  result$mu <- mu[locations]
  result$locations <- locations
  result$signal <- this.signal
  result$breaks <- apply(result$smooth,2,function(sm){
    break.after.i <- which(diff(sm)!=0)
    break.after <- locations[break.after.i]
    break.before <- locations[break.after.i+1]
    as.integer(floor((break.after+break.before)/2))
  })
  result$cost.details <- lapply(result$breaks,function(breaks){
    bkpt.err.details(mu.break.after,breaks,last)
  })
  sumDetails <- function(component){
    sapply(result$cost.details,function(L)sum(L[[component]]))
  }
  result$FP <- sumDetails("false.positive")
  result$FN <- sumDetails("false.negative")
  result$I <- sumDetails("imprecision")
  components <- with(result,cbind(FP,FN,I))
  result$cost <- rowSums(components)
  ##plot(result$cost,type="o")

  ## Cghseg exponent penalties.
  result$crit <- function(lambda,alpha,beta=0){
    J <- result$J.est
    Kseq <- seq_along(J)
    lambda * Kseq * d^alpha * (log(d)^beta) + J
  }
  result$lambda <- 10^seq(-9,9,l=200)
  result$kstar <- function(lambda,a, b=0){
    which.min(result$crit(lambda,a, b))
  }
  result$lambda.error <- function(a, b=0){
    lseq <- result$lambda
    k <- sapply(lseq,result$kstar,a, b)
    result$cost[k]
  }
  result$lambda.star <- function(a, b=0){
    error <- result$lambda.error(a, b) 
    l <- which(error == min(error))
    chosen <- l[ceiling(length(l)/2)]
    result$lambda[chosen]
  }
  result$flsa.error <- function(a,lambda){
    fit <- result$flsa(result$signal,lambda2=lambda*(d^a))
    n.lambda <- nrow(fit)
    flsa.breaks <- apply(fit,1,function(x)which(diff(x)!=0))
    if(length(lambda)==1){
      return(calc.cost(flsa.breaks))
    }
    n.breaks <- sapply(flsa.breaks,length)
    ok <- n.breaks<length(result$breaks)
    if(all(ok==FALSE)){
      print(n.breaks)
      stop("lambda grid isn't wide enough to get few enough breaks")
    }
    flsa.cost <- rep(NA,n.lambda)
    flsa.cost[ok] <- sapply(flsa.breaks[ok],calc.cost)
    flsa.cost[!ok] <- max(flsa.cost[ok])
    ##plot(flsa.cost,type="o")
    flsa.cost
  }
  result$flsa.alpha <- c(0,0.5,seq(0.9,1.1,by=0.01),1.5,2)
  result$flsa.lambda <- 10^seq(-7,5,l=300)
  result$flsa.cost <-
    matrix(NA,length(result$flsa.alpha),length(result$flsa.lambda))
  for(a.i in seq_along(result$flsa.alpha)){
    a <- result$flsa.alpha[a.i]
    cat(sprintf("flsa alpha=%10f %4d / %4d\n",a,a.i,length(result$flsa.alpha)))
    result$flsa.cost[a.i,] <- result$flsa.error(a,result$flsa.lambda)
  }
  result
}
  
set.seed(1)
seg.size <- 10000
means <- c(-3,0,3,0,2,-1,3)/3
mu <- do.call(c,lapply(means,rep,seg.size))
mu.break.after <- which(diff(mu)!=0)
signal <- rnorm(length(mu),mu,1)

variable.density.signals <- list()
signal.size <- as.integer(length(means)*10^seq(1,4,l=8))
for(i in 1:6){
  signal.size <- c(signal.size, i*10^(2:4))
}
n.signals <- length(signal.size)
for(sig.i in 1:n.signals){
  cat(sprintf("simulating signal %4d / %4d\n",sig.i,n.signals))
  d <- signal.size[sig.i]
  variable.density.signals[[sig.i]] <- sample.signal(d)
}

save(variable.density.signals,file="data/variable.density.signals.RData")

## write bedGraph file for annotating on SegAnnDB.
v.df <- do.call(rbind,lapply(seq_along(variable.density.signals),function(chr){
  sig <- variable.density.signals[[chr]]
  with(sig,data.frame(chr=sprintf("chr%d",chr),
                      chromStart=locations,chromEnd=locations+1,signal))
}))
header <- paste('track',
             'type=bedGraph',
             'db=hg19',
             'visibility=full',
             'maxSegments=20',
             'alwaysZero=on',
             'share=public',
             'graphType=points',
             'yLineMark=0',
             'yLineOnOff=on',
             'name=vd',
             'description="variable density simulation"')
f <- gzfile("data/variable.density.signals.bedGraph.gz","w")
writeLines(header,f)
write.table(v.df,f,quote=FALSE,row.names=FALSE,col.names=FALSE)
close(f)


