load("data/segmentation.list.RData")

pid <- "1.1"
seg <- segmentation.list[[pid]]
d <- nrow(seg$smooth)
mse <- seg$J.est/d
Kseq <- 1:length(mse)
line.df <- data.frame(intercept=mse,slope=Kseq)
cross.df <- data.frame()
optimal.segs <- length(mse)#current optimum.
lambda.left <- 0
crit.left <- mse[optimal.segs]
opt.df <- data.frame()
for(i in rev(Kseq[-1])){
  others <- (i-1):1
  denom <- Kseq[i]-Kseq[others]
  num <- mse[others]-mse[i]
  lambda <- num/denom
  crit <- i*lambda + mse[i]
  if(i==optimal.segs){
    j <- which.min(lambda)
    lambda.right <- lambda[j]
    crit.right <- crit[j]
    opt.df <- rbind(opt.df,{
      data.frame(lambda.left,lambda.right,crit.left,crit.right,
                 optimal.segs)
    })
    optimal.segs <- others[j]
    lambda.left <- lambda.right
    crit.left <- crit.right
  }
  cross.df <- rbind(cross.df,data.frame(i,lambda,crit))
}
## opt.df <- rbind(opt.df,{
##   data.frame(lambda.left,lambda.right=Inf,
##              crit.left,crit.right=Inf,
##              optimal.segs=1)
## })
lab.df <- within(line.df,{
  y <- 0.057
  x <- (y-intercept)/slope
})

library(ggplot2)
crit.plot <- ggplot()+
  geom_abline(aes(slope=slope,intercept=intercept),data=line.df)+
  geom_point(aes(lambda,crit),data=cross.df,pch=21,size=3)+
  geom_segment(aes(lambda.left,crit.left,xend=lambda.right,yend=crit.right),
                data=opt.df,colour="red")+
  xlab("smoothness parameter $\\lambda$")+
  ylab("crit$_i^k(\\lambda)$")
mymethod <- list(function(d,...){
  subset(d,as.integer(as.character(d$groups))%in%c(1:6,20))
},"big.boxes","draw.rects")
library(directlabels)
dl <- crit.plot+
  geom_dl(aes(x,y,label=slope),data=lab.df,method=mymethod)

##pdf("figure-ireg-exact-breakpoints.pdf",w=5,h=3.5)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-exact-breakpoints.tex",w=5,h=3.5)
print(dl)
dev.off()
