load("data/variable.breaks.constant.size.show.RData")

show <- variable.breaks.constant.size.show
lerr.df <- do.call(rbind,lapply(show,function(sig){
  do.call(rbind,lapply(c(0,-1/2,-1),function(a){
    error <- sig$lambda.error(a)
    l.star <- sig$lambda.star(a)
    lambda <- sig$lambda
    optimal <- l.star == lambda
    data.frame(error,lambda,alpha=a,optimal,length=max(sig$location))
  }))
}))
library(ggplot2)
labeller <- function(var,val){
  var <- ifelse(var=="alpha","\\alpha","\\textrm{length}")
  sprintf("$%s = %s$",var,val)
}
p <- ggplot(lerr.df,aes(log10(lambda),error))+
  geom_line()+
  geom_point(fill="white",pch=21,data=subset(lerr.df,optimal))+
  facet_grid(length~alpha,scales="free",labeller=labeller)+
  xlab("model complexity tradeoff parameter $\\log_{10}(\\lambda)$")+
  scale_y_continuous("error $E_i^\\alpha(\\lambda)$",
                     breaks=c(0,1,2,40,60,80))
print(p)

##pdf("figures/variable-breaks-constant-size-berr.pdf",w=7,h=3)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figures/variable-breaks-constant-size-berr.tex",w=6,h=3)
print(p)
dev.off()


