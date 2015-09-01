load("data/moons.iris.RData")
toplot <- subset(moons.iris,k<12 & k>1 & !is.nan(mean.rand))
levels(toplot$method) <- gsub(" ","\n",levels(toplot$method))
##toplot$method <- factor(toplot$method,sort(levels(toplot$method)))
##levels(toplot$method) <-
##  c("$\\gamma=0.5$","$\\gamma=2$","$\\gamma=10$","GMM","HC","$k$-means")
##toplot$method <- reorder(toplot$method,-toplot$mean.rand,mean,na.rm=TRUE)
library(ggplot2)
p <- ggplot(toplot,aes(k,mean.rand,colour=method,linetype=method))+
  geom_line(size=2)+
  geom_linerange(aes(ymin=mean.rand-sd.rand,ymax=mean.rand+sd.rand),size=2)+
  scale_y_continuous("Normalized Rand index\n",breaks=c(0.4,0.6,0.8,1))+
  scale_x_continuous("Number of clusters $k$",
                     breaks=c(2,3,5,10,15,20))+
  coord_cartesian(xlim=c(2,11),ylim=c(0.2,1))+
  facet_grid(data~.,
             labeller=function(var,val)sprintf("%s: %s",var,val))+
  guides(colour=guide_legend(
           ##label.position="bottom"
           keyheight=3
           ))
##library(directlabels)
##p <- xyplot(mean.rand~k|data,toplot,groups=method,type="l",
##            par.settings=simpleTheme(lwd=1.5,lty=1:5),xlim=c(2,14))
##p2 <- direct.label(p,list("\\gamma=10"))
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-clusterpath-moons-iris.tex",h=5)
print(p)
dev.off()



