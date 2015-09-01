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
getpolys <- function(path,l=0){
  gamma <- as.numeric(as.character(path$gamma[1]))
  W <- as.matrix(exp(-gamma*dist(attr(path,"weight.pts"))))
  alpha <- subset(path,lambda==l)[,c("row",attr(path,"alphacolnames"))]
  clusters <- unique(alpha[,-1])
  N <- nrow(clusters)
  equal <- function(u)apply(alpha[,-1],1,function(v)all(v==u))
  plot(as.matrix(clusters),type="n",asp=1,xlim=c(-2,1),ylim=c(-2,1))
  with(alpha,text(alpha.1,alpha.2,row))
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
    abline(intercept,m)
    int2 <- ci[1]/m+ci[2]
    m2 <- -1/m
    iperp <- function(x)m2*(x-ci[1])+ci[2]
    jperp <- function(x)m2*(x-cj[1])+cj[2]
    abline(int2,m2)
    xfound <- ci[1]-sqrt(((total/2)^2)/(m2^2+1))
    xoff <- abs(ci[1]-xfound)
    xvals <- xoff*c(-1,1)+ci[1]
    points(xvals,iperp(xvals),pch="+")
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
idname <- "Identity weights,\n$t=\\Omega(X)$"
line.df <- data.frame(getlines(x),figure=factor(idname))
path <- clusterpath.l2(x,verbose=1,join.thresh=0.01,gamma=1.3)
plot(path)
lval <- path$lambda[330]
poly.list <- getpolys(path,lval)
jointitle <- sprintf("Decreasing weights\nafter join, $t<\\Omega(X)$")
add_polygon <- function(p,L,figure){
  df <- data.frame(L$ws,figure)
  print(df)
  tochange <- c("12"=0.1,"13"=-0.1)
  for(i in seq_along(tochange)){
    FIND <- names(tochange)[i]
    yadd <- tochange[i]
    this <- grepl(FIND,df$label)
    df$alpha.2[this] <- df$alpha.2[this]+yadd
  }
  p+
  geom_polygon(aes(group=interaction(pair,rect.num)),
               data=data.frame(L$polys,figure),
               fill="grey",colour="white",lwd=1.2)+
  geom_segment(aes(start1,start2,xend=end1,yend=end2),
               data=data.frame(L$width,figure),
               colour="white")+
  geom_text(aes(label=label),
            data=df)
}
library(ggplot2)
p <- ggplot(poly.list$clusters,aes(alpha.1,alpha.2))+
  coord_equal()+
  geom_point(pch=21,fill="white")
add_polygon(p,poly.list,jointitle)
apart <- "Decreasing weights,\n$t=\\Omega(X)$"
apart.list <- getpolys(path,path$lambda[1])
outside <- data.frame(start=rbind(x,x),figure=idname,
                      end.1=c(x[1,1],x[1,1],x[3,1],x[3,1],x[3,1],x[3,1]),
                      end.2=c(x[2,2],x[2,2],x[2,2],x[1,2],x[2,2],x[1,2]))
segs <- rbind(outside,line.df[,names(outside)])
getcenter <- function(d,ann,xoff=0,yoff=0){
  with(d,data.frame(x=(start.1+end.1)/2+xoff,y=(start.2+end.2)/2+yoff,
                    label=sprintf("\\scriptsize$\\ell_%s$",ann),
                    figure=idname))
}
inf <- getcenter(outside,"\\infty")[c(1,4,5),]
inf$y[1] <- inf$y[1]+0.2
inf$x[-1] <- inf$x[-1]+0.2
inf$y[3] <- inf$y[3]+0.1
norms <- rbind(getcenter(line.df,"2"),getcenter(outside,"1"),inf)
norms$y[6] <- norms$y[6]+0.05
norms$y[8] <- norms$y[8]+0.1
p <- ggplot(NULL,aes(alpha.1,alpha.2))+
  geom_segment(aes(start.1,start.2,xend=end.1,yend=end.2),
               data=segs,lwd=2,colour="grey")+
  geom_text(aes(x,y,label=label),data=norms)
p2 <- add_polygon(p,apart.list,apart)+
  geom_path(aes(group=row),data=data.frame(path,figure=jointitle),
            subset=.(lambda<=lval))
p3 <- add_polygon(p2,poly.list,jointitle)+
  geom_point(data=data.frame(poly.list$clusters,figure=jointitle))+
  coord_equal()+
  geom_text(aes(label=label),
            data=data.frame(alpha.1=c(-1,x[2,1]-0.05,0.35),
              alpha.2=c(-1.25,0.35,x[3,2]),
              label=sprintf("\\scriptsize$X_%d$",1:3),
              figure=rep(c(jointitle,idname,apart),each=3)))+
  facet_grid(.~figure)+
  geom_text(aes(label=label),
            data.frame(alpha.1=c(-0.6,-0.6),alpha.2=c(-1.05,0.1),
                       figure=jointitle,
                       label=c("\\scriptsize$\\alpha_1$",
                         "\\scriptsize$\\alpha_{C}=\\alpha_2=\\alpha_3$")))
levs <- unique(unlist(lapply(p3$layers,function(l)levels(l$data$figure))))
allpts <- do.call(rbind,lapply(levs,function(l)data.frame(pts,figure=l)))
library(grid)
p4 <- p3+geom_point(data=allpts,pch=21,fill="white")+
 scale_x_continuous("",limits=c(-1.1,0.4),breaks=c(-10))+
 scale_y_continuous("",breaks=c(-10))+
  theme_bw()+
  theme(panel.margin=unit(0,"lines"))
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-clusterpath-geometry.tex",height=3.2,width=6.5)
print(p4)
dev.off()

