library(clusterpath)
### Figure 1. geometric interpretation
set.seed(3)
x <- replicate(2,rnorm(3))
pts <- data.frame(alpha=x,row=1:nrow(x))
getlines <- function(x){
  require(foreach)
  N <- nrow(x)
  foreach(i=1:(N-1),.combine=rbind)%do%{
    foreach(j=(i+1):N,.combine=rbind)%do%{
      start <- x[i,]
      end <- x[j,]
      data.frame(start.1=start[1],end.1=end[1],start.2=start[2],end.2=end[2],
                 norm=factor(2))
    }
  }
}
getpolys <- function(alpha.df){
  W <- attr(alpha.df,"w")
  alpha <- alpha.df[,c("row",attr(alpha.df,"alphacolnames"))]
  clusters <- unique(round(alpha[,-1],6))
  N <- nrow(clusters)
  close <- function(u,v)sqrt(sum((v-u)^2))<1e-6
  equal <- function(u)apply(alpha[,-1],1,close,u)
  polys <- data.frame()
  ws <- data.frame()
  width <- data.frame()
  for(i in 1:(N-1))for(j in (i+1):N){
    ci <- as.matrix(clusters[i,])
    cj <- as.matrix(clusters[j,])
    m <- (ci[2]-cj[2])/(ci[1]-cj[1])
    rects <- expand.grid(i=alpha[equal(ci),"row"],j=alpha[equal(cj),"row"])
    rects$wij <- W[as.matrix(rects)]
    total <- sum(rects$wij)
    ## this is the horizontal displacement from ci
    intercept <- ci[2]-ci[1]*m
    int2 <- ci[1]/m+ci[2]
    m2 <- -1/m
    iperp <- function(x)m2*(x-ci[1])+ci[2]
    jperp <- function(x)m2*(x-cj[1])+cj[2]
    xfound <- ci[1]-sqrt(((total/2)^2)/(m2^2+1))
    xoff <- abs(ci[1]-xfound)
    xvals <- xoff*c(-1,1)+ci[1]
    ## this is the angle of the rectangle from the horizontal
    wsum <- c(0,cumsum(rects$wij))
    rate <- xoff/total*2
    ix <- function(w)w*rate+ci[1]-xoff
    jx <- function(w)w*rate+cj[1]-xoff
    for(k in 1:(length(wsum)-1)){
      IX <- c(ix(wsum[k]),ix(wsum[k+1]))
      JX <- c(jx(wsum[k+1]),jx(wsum[k]))
      m <- cbind(c(IX,JX),c(iperp(IX),jperp(JX)))
      row <- t(c(start=colMeans(m[c(1,4),]),end=colMeans(m[c(2,3),])))
      width <- rbind(width,data.frame(row,rect.num=k))
      ij <- with(rects,sprintf("%d%d",i[k],j[k]))
      ws <- rbind(ws,data.frame(alpha=t(colMeans(m)),
                                label=sprintf("\\tiny$w_{%s}$",ij)))
      polys <- rbind(polys,data.frame(alpha=m,rect.num=factor(k),
                    pair=ij,row.names=NULL))
    }
  }
  list(polys=polys,clusters=clusters,ws=ws,width=width)
}
l2 <- getlines(x)
l1 <- data.frame(start=rbind(x,x),
                 end.1=c(x[1,1],x[1,1],x[3,1],x[3,1],x[3,1],x[3,1]),
                 end.2=c(x[2,2],x[2,2],x[2,2],x[1,2],x[2,2],x[1,2]),
                 norm="1")
linf <- data.frame(start=x[c(1,2,1),],
                   end.1=c(x[3,1],x[3,1],x[1,1]),
                   end.2=c(x[1,2],x[2,2],x[2,2]),
                   norm="\\infty")
lvec <- c("1","2","\\infty")
norms <- data.frame(alpha.1=-1,alpha.2=0.5,
                    label=sprintf("$\\ell_%s$",lvec),norm=lvec)
segs <- data.frame()
for(df in list(l1,l2,linf)){
  segs <- rbind(segs,df[,names(l1)])
}
library(ggplot2)
theme_set(theme_bw())
## calculate this first to get proper scale
#path <- cvxmod.cluster(x,regularization.points=20)
#biggest <- path[path$s==1,]
 #big.polys <- getpolys(biggest)
interpretation <- function(...){
  p <- ggplot(NULL,aes(alpha.1,alpha.2))+
    coord_equal()+
    scale_x_continuous("$\\alpha^1$ survival",limits=c(-1.1,0.4),breaks=c(-10))+
    scale_y_continuous("$\\alpha^2$ number of breakpoints",
                       limits=c(-1.3,0.6),breaks=c(-10))+
    geom_text(aes(label=label),
            data=data.frame(alpha.1=c(-1,x[2,1]-0.05,0.35),
              alpha.2=c(-1.25,0.35,x[3,2]),
              label=sprintf("\\scriptsize$X_%d$",1:3)))
    #geom_blank(data=big.polys$polys)
  L <- list(...)
  for(l in L)p <- p+l
  p+  geom_point(data=pts,pch=21,fill="white")
}
drawlines <- function(N){
interpretation(
  geom_segment(aes(start.1,start.2,xend=end.1,yend=end.2),
               data=subset(segs,norm==N),lwd=2,colour="grey"),
  geom_text(aes(label=label),data=subset(norms,norm==N))
)
}
p <- interpretation()+
  geom_point(data=data.frame(alpha.1=c(-0.88,-0.05),
               alpha.2=c(-1.04,0.06)))+
  geom_text(aes(label=label),
            data.frame(alpha.1=c(-0.7,-0.6),alpha.2=c(-1.05,0.1),
                       label=c("\\scriptsize$\\alpha_1$",
                         "\\scriptsize$\\alpha_{C}=\\alpha_2=\\alpha_3$")))
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-clusterpath-interpretation.tex",width=2.5,height=4)
print(p)
dev.off()
