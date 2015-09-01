library(lattice)
set.seed(1)
p <- xyplot(jitter(Petal.Length)~jitter(Sepal.Length),iris,groups=Species)
library(directlabels)
get.polygon.indices <- function(e,edges){
  from <- e$ind1
  to <- e$ind2
  df <- data.frame(from,to)
  move <- function(At,Next){
    Next.Next <- subset(edges,ind2==Next&ind1!=At)$ind1
    if(length(Next.Next)){
      Next.Next
    }else{
      subset(edges,ind1==Next&ind2!=At)$ind2
    }
  }
  while(with(df,all(to != from[1]))){
    new.to <- move(from,to)
    from <- to
    to <- new.to
    df <- rbind(df,data.frame(from,to))
  }
  df[,1]
}
ids <- c(setosa=1,virginica=2,versicolor=3)
draw.ahull <- function(d,...){
  center <- get.means(d)[names(ids),]
  with(center,{
    size <- 1/2
    grid.rect(x,y,size,size,default.units="cm",gp=gpar(fill="white",col=NA))
    grid.text(sprintf("$c_%s$",ids),
              x,y,
              default.units="cm")
  })
  edges <- ahull.points(d)
  ##with(edges, grid.segments(x1, y1, x2, y2, default.units = "cm"))
  i <- get.polygon.indices(edges[1,],edges)
  ##with(d, grid.text(seq_along(x), x, y,default.units="cm"))
  ##with(edges, grid.text(ind1,x1,y1,default.units="cm"))
  ##with(edges, grid.text(ind2,x2,y2,default.units="cm"))
  with(d[i,], {
    grid.polygon(x, y, default.units = "cm",
                 gp=gpar(fill="black", alpha = 1/4))
  })
  d
}
notation <- function(d,...){
  with(d,grid.points(x,y,pch=20,default.units="cm"))
  ## Notations.
  grid.text("$H$",5.5,2.5,default.units="native")
  setosa <- 6
  versicolor <- 7.5
  lists <- function(k, lr, to.x, to.y, hjust){
    id <- ids[[k]]
    list(list(to=unit.c(unit(to.x,"native"),unit(d[k,"y"],"cm")),
              from=unit(d[k,c(lr,"y")],"cm"),
              label=sprintf("$B_%s(L_%s)$",id,id),
              hjust=hjust),
         list(from=unit(d[k,c("x","y")],"cm"),
              to=unit(c(to.x, to.y),"native"),
              label=sprintf("$L_%s$",id),
              hjust=hjust))
  }
  b <- c(lists("setosa","right", 6, 2, 0),
         lists("versicolor","right", 7.5, 4.5, 0),
         lists("virginica","left", 5.5, 5.5, 1))
  for(L in b){
    to <- L$to
    from <- L$from
    grid.segments(from[1],from[2],to[1],to[2],gp=gpar(col="black"))
    grid.text(L$label,to[1],to[2],hjust=L$hjust)
  }
  d
}
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-iris-non-convex.tex",h=7,w=6)
methods <-
  list(Legend="legend",
       "Direct labels"=list("draw.ahull","outside.ahull",
         "draw.rects","notation"))
dlcompare(list(p), methods,
          rects=FALSE,
          row.items="posfuns")
dev.off()
