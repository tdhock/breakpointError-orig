fun <- function(fun,...){
  list(fun=fun,min=list(...))
}
pt <- function(x,label="global min",f=0){
  data.frame(x,label,f)
}
funs <- list(convex=list("$x^2$"=fun(function(x)(x)^2,pt(0)),
               "$|x|$"=fun(abs,pt(0)),
"piecewise\naffine"=fun(function(x){
  ifelse(x< -2,-2-x,ifelse(x< -1,0,x+1))
},c(-2,-1))),
             "non-convex"=list(
   "$1_{x\\neq 0}$"=fun(function(x)5,
     pt(0,"open",5),pt(0,"closed"),pt(0,"global min")),
"multiple\nminima"=fun(function(x){
  ifelse(x<0,0.5*(x+2)^2,(x-1)^2+1)
},pt(-2),pt(1,"local min",1))))

x <- seq(-3,3,by=0.1)
lines.df <- do.call(rbind,lapply(names(funs),function(convex){
  L <- funs[[convex]]
  do.call(rbind,lapply(names(L),function(name){
    data.frame(f=L[[name]]$fun(x),x,name,convex)
  }))
}))

pts.df <- do.call(rbind,lapply(names(funs),function(convex){
  L <- funs[[convex]]
  do.call(rbind,lapply(names(L),function(name){
    m <- L[[name]]$min
    if(is.data.frame(m[[1]])){
      d <- do.call(rbind,m)
      data.frame(convex,name,d)
    }else data.frame()
  }))
}))

seg.df <- do.call(rbind,lapply(names(funs),function(convex){
  L <- funs[[convex]]
  do.call(rbind,lapply(names(L),function(name){
    m <- L[[name]]$min
    if(is.numeric(m[[1]])){
      data.frame(convex,name,x=m[[1]][1],xend=m[[1]][2])
    }else data.frame()
  }))
}))
    

library(ggplot2)
fill.col <- c(open="white",closed="black",
              "global min"="red","local min"="violet")
edges <- fill.col
edges[] <- NA
edges["open"] <- "black"
sizes <- rep(5,length(fill.col))
names(sizes) <- names(fill.col)
sizes[c("global min","local min")] <- 3
p <- ggplot(,aes(x,f))+
  geom_segment(aes(xend=xend),y=0,colour="red",yend=0,data=seg.df,lwd=3)+
  geom_line(data=lines.df,lwd=1.5)+
  facet_grid(name~.,scales="free")+
  geom_point(aes(colour=label,fill=label,size=label),pch=21,data=pts.df)+
  scale_fill_manual(values=fill.col)+
  scale_colour_manual(values=edges)+
  scale_size_manual(values=sizes)+
  ylab("$f(x)$")+
  xlab("$x$")
print(p)
               
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-convexity.tex",h=5)
print(p)
dev.off()
