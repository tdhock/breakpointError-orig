estimate.error <- function(a,train,test){
  l.star <- train$lambda.star(a)

  test.k <- test$kstar(l.star,a)
  test$cost[test.k]
}

test.all.pairs <- function(a,signal.list){
  n.signals <- length(signal.list)
  test.error <- c()
  for(i in 1:(n.signals-1)){
    cat(sprintf("alpha=%10f signal %5d / %5d\n",a,i,n.signals-1))
    for(j in (i+1):n.signals){
      err <- estimate.error(a,signal.list[[i]],signal.list[[j]])
      err2 <- estimate.error(a,signal.list[[j]],signal.list[[i]])
      test.error <- c(test.error,err,err2)
    }
  }
  test.error
}

load("data/variable.scale.signals.RData")
a.df <- data.frame()
a.grid <- c(0,1,seq(1.5,2.5,by=0.1),3)
for(a in a.grid){
  test.error <- test.all.pairs(a,variable.scale.signals)
  err.df <- data.frame(test.error,pair=seq_along(test.error),alpha=a)
  a.df <- rbind(a.df,err.df)
}

train.df <- do.call(rbind,lapply(a.grid,function(a){
  err.mat <- sapply(variable.scale.signals,function(sig){
    sig$lambda.error(a)
  })## matrix[lambda,signal]
  error <- rowSums(err.mat)
  data.frame(alpha=a,mean=min(error),sd=NA,what="train")
}))
library(plyr)
test.df <- ddply(a.df,.(alpha),summarize,
      mean=mean(test.error),sd=sd(test.error),what="test")
stat.df <- rbind(train.df,test.df)
library(ggplot2)
p <- ggplot(stat.df,aes(alpha,mean))+
  geom_ribbon(aes(ymin=ifelse(mean-sd<0,0,mean-sd),ymax=mean+sd),alpha=1/2)+
  geom_line()+
  ##geom_point(aes(alpha,test.error),data=a.df)+
  ##coord_cartesian(ylim=c(0,10))+
  facet_grid(what~.,scales="free")+
  ylab("total error relative to true breakpoints (breakpointError)")+
  xlab("penalty exponent $\\alpha$")
print(p)

##pdf("figure-variable-scale-error-alpha.pdf",h=4)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-variable-scale-error-alpha.tex",h=4,w=6)
print(p)
dev.off()
