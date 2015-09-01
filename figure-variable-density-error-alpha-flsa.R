estimate.error <- function(a.i,train,test){
  error <- train$flsa.cost[a.i,]
  is.min <- which(min(error)==error)
  chosen <- floor(mean(is.min))
  
  test$flsa.cost[a.i,chosen]
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

load("data/variable.density.signals.RData")
a.grid <- variable.density.signals[[1]]$flsa.alpha
a.df.list <- lapply(seq_along(a.grid),function(a.i){
  test.error <- test.all.pairs(a.i,variable.density.signals)
  data.frame(test.error,pair=seq_along(test.error),alpha=a.grid[a.i])
})
a.df <- do.call(rbind,a.df.list)

train.df <- do.call(rbind,lapply(seq_along(a.grid),function(a.i){
  err.mat <- sapply(variable.density.signals,function(sig){
    sig$flsa.cost[a.i,]
  })## matrix[lambda,signal]
  error <- rowSums(err.mat)
  data.frame(alpha=a.grid[a.i],mean=min(error),sd=NA,what="train")
}))
library(plyr)
test.df <- ddply(a.df,.(alpha),summarize,
      mean=mean(test.error),sd=sd(test.error),what="test")
stat.df <- rbind(train.df,test.df)
minima <- ddply(stat.df,.(what),subset,mean==min(mean))
opt.lim <- ddply(minima,.(what),summarize,min=min(alpha),max=max(alpha))
opt.lim$y <-
  ddply(stat.df,.(what),summarize,max=max(mean,mean+sd,na.rm=TRUE))$max
library(ggplot2)
p <- ggplot(stat.df,aes(alpha,mean))+
  geom_vline(aes(xintercept=max),data=opt.lim,linetype="dashed")+
  geom_text(aes(max,y,label=max),data=opt.lim,hjust=1,vjust=1)+
  geom_ribbon(aes(ymin=ifelse(mean-sd<0,0,mean-sd),ymax=mean+sd),alpha=1/2)+
  geom_line()+
  ##geom_point(aes(alpha,test.error),data=a.df)+
  ##coord_cartesian(ylim=c(0,10))+
  facet_grid(what~.,scales="free")+
  scale_y_continuous("total error relative to true breakpoints (breakpointError)")+
   ##                  breaks=c(5,10,15,47,48))+
  xlab("penalty exponent $\\alpha$")

##pdf("figure-variable-density-error-alpha-flsa.pdf",h=3)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-variable-density-error-alpha-flsa.tex",h=3.5,w=6)
print(p)
dev.off()
