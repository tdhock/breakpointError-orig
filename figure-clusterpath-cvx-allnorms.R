### Figure 2. 10 points to demonstrate different path geometries

## dest <- "data/cvx.RData"
## if(!file.exists(dest)){
##   download.file("http://cbio.ensmp.fr/~thocking/clusterpath-figure-2.RData",dest)
## }
## load(dest)

load("data/sim.cvx.RData")
means <- data.frame(alpha=t(colMeans(sim$mat)))
library(ggplot2)
library(grid)
p <- ggplot(cvx,aes(alpha.1,alpha.2))+
  ##geom_text(data=means,label="$\\bar X$",col="grey")+
  geom_point(data=means,label="$\\bar X$",col="grey")+
  ##geom_point(aes(size=s),alpha=1/4)+
  geom_path(aes(group=row),colour="black",lwd=1)+
  facet_grid(gamma~norm,labeller=function(var,val){
    val <- as.character(val)
    if(var=="gamma")var <- "\\gamma"
    else var <- sprintf("\\textrm{%s}",var)
    val[val=="inf"] <- "\\infty"
    sprintf("$%s=%s$",var,val)
  })+
  coord_equal()+
  scale_size("$s$")+
  geom_point(data=data.frame(alpha=sim$mat),pch=21,fill="white")+
  scale_x_continuous("",breaks=-10)+
  scale_y_continuous("",breaks=-10)+
  ##geom_text(data=means,vjust=-0.5,label="$\\bar X$",col="grey")+
  geom_point(data=means,label="$\\bar X$",fill="grey",pch=21)+
  theme_bw()+
  theme(panel.margin=unit(0,"lines"))

library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-clusterpath-cvx-allnorms.tex",height=3,width=5.8)
print(p)
dev.off()
