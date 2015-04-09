load("data/variable.size.alpha.beta.RData")
library(plyr)
lim <- 1.21
some.stats <- 
subset(variable.size.alpha.beta$stats,
       -lim <= alpha & alpha <= lim &
         -lim <= beta & beta <= lim)
stat.df <- ddply(some.stats,.(what),
                 transform,
                 mean.norm=(mean-min(mean))/(max(mean)-min(mean)))
min.df <- ddply(some.stats,.(what),
                 subset,
                 mean==min(mean))
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
  xlab("beta (exponent of signal length in bases)")+
  ylab("alpha (exponent of number of data points)")+
  geom_tile(aes(alpha,beta,fill=mean.norm),data=stat.df)+
  ## stat_contour(aes(alpha,beta,z=mean.norm),
  ##              data=stat.df,
  ##              breaks=seq(0, 0.2, by=0.05),
  ##              color="red")+
  scale_color_manual(values=c(minimum="red", expected="white"))+
  scale_shape_manual(values=c(minimum=20, expected=1))+
  geom_point(aes(alpha,beta,shape=value, color=value),
             data=data.frame(min.df, value="minimum"))+
  geom_point(aes(alpha,beta, shape=value, color=value),
             data=data.frame(alpha=-0.5,beta=0.5, value="expected"))+
  ## geom_dl(aes(alpha,beta,z=mean.norm,label=..level..),
  ##         data=stat.df, color="red",
  ##         method="bottom.pieces",
  ##         stat="contour")+
  facet_grid(.~what)+
  theme_grey()
  ##theme_bw()+  theme(panel.margin=grid::unit(0, "cm"))


pdf("figures/variable-size-error-alpha-beta.pdf",h=4,w=7)
##library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
##tikz("figures/variable-size-error-alpha-beta.tex",h=3,w=6)
print(p)
dev.off()
