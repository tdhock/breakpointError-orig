estimate.error <- function(a,b,train,test){
  l.star <- train$lambda.star(a,b)

  test.k <- test$kstar(l.star,a,b)
  test$cost[test.k]
}

test.all.pairs <- function(a,b,signal.list){
  n.signals <- length(signal.list)
  test.error <- c()
  for(i in 1:(n.signals-1)){
    cat(sprintf("alpha=%10f signal %5d / %5d\n",a,i,n.signals-1))
    for(j in (i+1):n.signals){
      err <- estimate.error(a,b,signal.list[[i]],signal.list[[j]])
      err2 <- estimate.error(a,b,signal.list[[j]],signal.list[[i]])
      test.error <- c(test.error,err,err2)
    }
  }
  test.error
}

load("data/variable.density.signals.RData")
a.df <- data.frame()
a.grid <- sort(c(2,seq(0,1.5,by=0.1),-0.5,-1,0.45,0.55))
b.grid <- sort(c(-1, seq(-0.5, 1.5, by=0.1), 2))
for(a in a.grid){
  for(b in b.grid){
    test.error <- test.all.pairs(a,b,variable.density.signals)
    err.df <- data.frame(test.error,pair=seq_along(test.error),alpha=a,beta=b)
    a.df <- rbind(a.df,err.df)
  }
}

train.df <- NULL
for(a in a.grid){
  for(b in b.grid){
    err.mat <- sapply(variable.density.signals,function(sig){
      sig$lambda.error(a, b)
    })## matrix[lambda,signal]
    error <- rowSums(err.mat)
    train.df <- rbind(train.df, {
      data.frame(alpha=a,beta=b,mean=min(error),sd=NA,what="train")
    })
  }
}
library(plyr)
test.df <- ddply(a.df,.(alpha, beta),summarize,
      mean=mean(test.error),sd=sd(test.error),what="test")
stat.df <- rbind(train.df,test.df)
library(ggplot2)
some.df <- subset(stat.df, beta %in% c(-1, -0.5, 0, 0.5, 1, 1.5, 2))
train.best <- subset(subset(some.df, what=="train"), mean==min(mean))
p <- ggplot(some.df,aes(alpha,mean, group=what))+
  geom_ribbon(aes(ymin=ifelse(mean-sd<0,0,mean-sd),ymax=mean+sd),alpha=1/2)+
  geom_point(data=train.best)+
  geom_line()+
  ##geom_point(aes(alpha,test.error),data=a.df)+
  ##coord_cartesian(ylim=c(0,10))+
  facet_grid(beta ~ .,scales="free")+
  ylab("total error relative to latent breaks")+
  xlab("penalty exponent $\\alpha$")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(p)
p <- ggplot(some.df,aes(alpha,mean, group=beta))+
  geom_ribbon(aes(ymin=ifelse(mean-sd<0,0,mean-sd),ymax=mean+sd),alpha=1/2)+
  geom_line()+
  ##geom_point(aes(alpha,test.error),data=a.df)+
  ##coord_cartesian(ylim=c(0,10))+
  facet_grid(what ~ .,scales="free")+
  ylab("total error relative to latent breaks")+
  xlab("penalty exponent $\\alpha$")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(p)
alpha.zero <- subset(stat.df, alpha==0)
p <- ggplot(alpha.zero,aes(beta,mean))+
  geom_ribbon(aes(ymin=ifelse(mean-sd<0,0,mean-sd),ymax=mean+sd),alpha=1/2)+
  geom_line()+
  ##geom_point(aes(alpha,test.error),data=a.df)+
  ##coord_cartesian(ylim=c(0,10))+
  facet_grid(what ~ .,scales="free")+
  ylab("total error relative to latent breaks")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(p)

##pdf("figures/variable-density-error-alpha.pdf",h=3)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figures/variable-density-error-alpha.tex",h=3.5,w=6)
print(p)
dev.off()
