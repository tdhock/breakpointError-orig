## experiment: calculate the breakpoint error and show the curve that
## demonstrates what exponent we need to pick for the signal length.

source("data/precise.breakpoint.cost.R")
source("data/run.cghseg.R")

sample.signal <- function(until, by){
  locations <- seq(1, until, by=by)
  signal <- sig[locations]
  result <- run.cghseg(signal,round(until/50))
  result$signal <- signal
  result$by <- by
  result$mu <- mu[locations]
  get.breaks <- function(sm){
    break.after.i <- which(diff(sm)!=0)
    break.after <- locations[break.after.i]
    break.before <- locations[break.after.i+1]
    floor((break.after+break.before)/2)
  }
  result$mu.breaks <- get.breaks(result$mu)
  result$locations <- locations
  result$size <- until
  result$breaks <- apply(result$smooth,2,get.breaks)
  result$cost <- sapply(result$breaks,function(x){
    bkpt.err(result$mu.breaks,x,until)
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
means <- rep(c(-3,0,3,0,2,-1,3),100)/3
mu <- do.call(c,lapply(means,rep,seg.size))
locations <- seq_along(mu)
d <- length(mu)

## the noisy signal (black points)
set.seed(1)
sig <- rnorm(d,mu)

go.until <- c(200,400,700,1000,2000,4000,7000,10000)
go.until <- c(200,400,800)
go.by <- c(1, 2, 4)
go.by <- 1
go.by <- c(1, 2, 4)
n.signals <- length(go.until)
variable.size.signals <- list()
for(by in go.by){
  for(sig.i in 1:n.signals){
    cat(sprintf("simulating signal %4d / %4d\n",sig.i,n.signals))
    size <- go.until[sig.i]
    variable.size.signals[[paste(size, by)]] <- sample.signal(size, by)
  }
}

cost.df <- do.call(rbind,lapply(variable.size.signals, with, {
  data.frame(segments=seq_along(cost),cost,length=size, data=length(mu),by)
}))
library(ggplot2)
source("scripts/signal.colors.R")
signal.df <- do.call(rbind,lapply(variable.size.signals, with, {
  data.frame(locations,signal,mu,length=size, data=length(mu),by)
}))
ggplot(signal.df,aes(locations,signal))+
  geom_point(pch=21)+
  geom_line(aes(y=mu),colour=signal.colors["latent"],lwd=1.1)+
  facet_grid(length + data ~.,labeller=label_both)+
  xlab("position in base pairs")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  facet_grid(length + by ~ ., labeller=label_both)
ggplot(cost.df, aes(segments, cost))+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  facet_grid(length + by ~ ., labeller=label_both)

save(variable.size.signals,file="data/variable.size.signals.RData")

