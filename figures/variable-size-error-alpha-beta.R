load("data/variable.size.alpha.beta.RData")
library(plyr)
stat.df <- ddply(variable.size.alpha.beta$stats,.(what),
                 transform,
                 mean.norm=(mean-min(mean))/(max(mean)-min(mean)))
library(ggplot2)
p <- ggplot(stat.df,aes(alpha,mean,group=beta))+
  geom_ribbon(aes(ymin=ifelse(mean-sd<0,0,mean-sd),ymax=mean+sd),alpha=1/2)+
  geom_line()+
  ##geom_point(aes(alpha,test.error),data=a.df)+
  ##coord_cartesian(ylim=c(0,10))+
  facet_grid(what~beta,scales="free")+
  scale_y_continuous("total breakpointError")+
  xlab("penalty exponent $\\alpha$")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
##print(p)

p <- 
ggplot()+
  geom_tile(aes(alpha,beta,fill=mean.norm),data=stat.df)+
  ##stat_contour(aes(alpha,beta,z=mean),data=stat.df)+
  facet_grid(.~what)+
  theme_bw()+
  ##scale_fill_gradient2(low="skyblue", high="black")+
  theme(panel.margin=grid::unit(0, "cm"))

pdf("figures/variable-size-error-alpha-beta.pdf",h=3,w=6)
##library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
##tikz("figures/variable-size-error-alpha-beta.tex",h=3,w=6)
print(p)
dev.off()
