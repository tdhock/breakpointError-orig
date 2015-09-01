### Figure 4. L2 path split
x <- cbind(c(1.1,1.2,4,2),
           c(2.2,2.5,3,0))
Wmat <- matrix(0,4,4)
Wmat[2,1] <- Wmat[1,2] <- 9
Wmat[3,1] <- Wmat[1,3] <- 20
Wmat[3,2] <- Wmat[2,3] <- 1
Wmat[4,1] <- Wmat[1,4] <- 1
Wmat[4,2] <- Wmat[2,4] <- 20
Wmat[4,3] <- Wmat[3,4] <- 1
w <- as.dist(Wmat)

library(clusterpath)
dfs <- lapply(c(1,0),function(check.splits){
  clusterpath.l2.general(x,w,gamma=NA,
                         lambda=0.001,
                         lambda.factor=1.02,
                         join.thresh=0.01,
                         check.splits=check.splits)
})
res <- do.call(rbind,dfs)
levels(res$solver) <- gsub("descent.(.+)","\\1",levels(res$solver))
lvals <- c(0,0.005,0.02,0.06,0.11)
cvx <- cvxcheck(res,lambda=lvals)
pts <- data.frame(alpha=x,row=1:nrow(x))
xbarm <- t(colMeans(x))
xbar <- data.frame(alpha=xbarm)
xbart <- transform(xbar,alpha.1=alpha.1+0.2,alpha.2=alpha.2-0.2)
pt.colors <- c("$\\alpha^*_i$ cvxmod"="black","$\\bar X$"="grey")
pt.df <- rbind(data.frame(cvx[,1:2],point=names(pt.colors)[1]),
               data.frame(xbar,point=names(pt.colors[2])))
library(ggplot2)
library(directlabels)
p <- ggplot(res,aes(alpha.1,alpha.2,colour=factor(row)))+
  ##geom_text(aes(colour=NULL),data=xbar,label="$\\bar X$",hjust=0,vjust=1)+
  geom_path(aes(group=interaction(row,solver),linetype=solver),
            alpha=1/2,lwd=1.5)+
  ##geom_point(data=cvx,pch=21,fill="white")+
  ##scale_colour_identity()+
  scale_size("$\\lambda$ cvxmod",breaks=round(lvals[-1],3))+
  scale_linetype("descent\nsolver")+
  ##geom_point(aes(colour=NULL),data=xbar,size=4)+
  geom_point(aes(colour=NULL,fill=point),pch=21,data=pt.df,size=2)+
  scale_fill_manual(values=pt.colors)+
  geom_dl(aes(label=row),data=pts,
          method=list("big.boxes","draw.rects"))+
  scale_x_continuous("$\\alpha^1$",breaks=1:4)+
  scale_y_continuous("$\\alpha^2$",breaks=0:3)+
  coord_equal()+
  guides(colour="none")+
  theme_bw()
print(p)

library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-clusterpath-l2split.tex",height=5,width=6)
print(p)
dev.off()

