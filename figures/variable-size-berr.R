load("data/variable.size.show.RData")

lerr.df <- do.call(rbind,lapply(variable.size.show,function(sig){
  do.call(rbind,lapply(c(0,-1/2,-1),function(a){
    error <- sig$lambda.error(a)
    l.star <- sig$lambda.star(a)
    lambda <- sig$lambda
    optimal <- l.star == lambda
    data.frame(error,lambda,alpha=a,optimal,base.pairs=sig$size)
  }))
}))
bp <- round(lerr.df$base.pairs)
lerr.df$base.pairs <- factor(bp,sort(unique(bp)))
library(ggplot2)
leg <- "base pairs"
sizes <- c(1.5,0.5)
s.colors <- c("#00bfc4","#f8766d")
names(sizes) <- names(s.colors) <- levels(lerr.df$base.pairs)
labeller <- function(var,val){
  tmp <- ifelse(var=="alpha",
                "$\\alpha = %s$",
                "\\textrm{length}\n%s")
  sprintf(tmp,val)
}
p <- ggplot(lerr.df,aes(log10(lambda),error))+
  geom_line()+
  geom_point(fill="white",pch=21,data=subset(lerr.df,optimal))+
  facet_grid(base.pairs~alpha,scales="free",labeller=labeller)+
  scale_size_manual(leg,values=sizes)+
  xlab("model complexity tradeoff parameter $\\log_{10}(\\lambda)$")+
  scale_y_continuous("error $E_i^\\alpha(\\lambda)$",
                     breaks=c(0,1,2,30,60,90))

##pdf("figures/variable-size-berr.pdf",w=7,h=4)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figures/variable-size-berr.tex",w=6,h=2)
print(p)
dev.off()


