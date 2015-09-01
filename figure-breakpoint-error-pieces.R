make.fun <- function(L,x,R){
  force(L)
  force(x)
  force(R)
  function(g){
    ifelse(g<L,1,{
      ifelse(g<x,(x-g)/(x-L),{
        ifelse(g<R,(g-x)/(R-x),1)
      })
    })
  }
}

pieces <- make.fun(1,5,7)
curve(pieces(x),0,10)

make.args <- function(break.vec,signal.size){
  stopifnot(is.vector(break.vec))
  stopifnot(length(break.vec)>0)
  args.list <- list()
  for(i in seq_along(break.vec)){
    x <- break.vec[i]
    left <- if(i == 1){
      1
    }else{
      right+1
    }
    right <- if(i == length(break.vec)){
      signal.size-1
    }else{
      floor((x+break.vec[i+1])/2)
    }
    args.list[[i]] <- list(left,x,right)
  }
  args.list
}

set.seed(1)
reduce.by <- 3
offset.by <- 100
seg.mean <- c(-1,0,0.5)/reduce.by+offset.by
seg.size <- c(4,10,8)
ends <- cumsum(seg.size)+1 # up to not including this base
starts <- ends-seg.size
seg.df <- data.frame(starts,ends,seg.mean,what="signal")
breaks <- ends[-length(ends)]-1
size <- ends[length(ends)]-1
base.df <- data.frame(base=sample(c("N"),size,replace=TRUE),
                      position=1:size,what="signal",signal=-0.5/reduce.by+offset.by)
last.break <- size-1
piece.args <- make.args(breaks,size)
## label the region definition.
regions.df <- do.call(rbind,lapply(seq_along(piece.args),function(i){
  L <- piece.args[[i]]
  base <- unlist(L)
  symbol <- sprintf(c("$\\underline r_%d$","$B_%d$","$\\overline r_%d$"),i)
  hjust <- c(0,0,1)
  data.frame(base,symbol,what="error",cost=1.3,hjust,i)
}))
piece.funs <- lapply(piece.args,function(L)do.call(make.fun,L))
base <- 1:last.break
midpoints <- breaks[-1]-diff(breaks)/2
knots <- sort(c(-Inf,regions.df$base,Inf))
point.df <- do.call(rbind,lapply(seq_along(breaks),function(i){
  fun <- piece.funs[[i]]
  cost <- fun(base)
  data.frame(cost,base,i,what="error")
}))
text.df <- do.call(rbind,lapply(seq_along(piece.args),function(i){
  L <- piece.args[[i]]
  this.curve <- point.df[point.df$i==i,]
  min.point <- this.curve[which.min(this.curve$cost),]
  label <- sprintf("$\\ell_%d = C_{%d,%d,%d}$",i,L[[1]],L[[2]],L[[3]])
  data.frame(min.point,label)
}))
text.df$base <- text.df$base+c(1.5,2)
fun.df <- do.call(rbind,lapply(seq_along(breaks),function(i){
  fun <- piece.funs[[i]]
  cost <- fun(knots)
  data.frame(cost,base=knots,i,what="error")
}))
break.df <- do.call(rbind,lapply(seq_along(breaks),function(i){
  data.frame(base=breaks[i],i,what="error")
}))

library(ggplot2)
source("scripts/signal.colors.R")
x.breaks <- c(regions.df$base,
              max(point.df$base)+1)
p <- ggplot()+
  geom_text(aes(base,cost,colour=factor(i),
                label=symbol,hjust=hjust),
            data=regions.df,vjust=1)+
  geom_point(aes(base,cost,group=i,colour=factor(i)),
             data=point.df,pch=1,size=3)+
  geom_line(aes(base,cost,group=i,colour=factor(i),size=factor(i)),
            data=fun.df)+
  geom_rug(data=break.df)+
  scale_size_manual(values=c("1"=1.5,"2"=0.8)*1.5)+
  ##theme(title="Exact breakpoint error functions")+
  geom_segment(aes(starts-0.5,seg.mean,xend=ends-0.5,yend=seg.mean),
               data=seg.df,lwd=2,colour=signal.colors["latent"])+
  facet_grid(what~.,scales="free",space="free")+
  geom_text(aes(position,signal,label=base),data=base.df,size=3)+
  geom_text(aes(base,cost,group=i,colour=factor(i),label=label),
            data=text.df,hjust=0,vjust=-0.5)+
  guides(colour="none",size="none")+
  scale_y_continuous("",breaks=c(1,0),minor_breaks=NULL)+
  scale_x_continuous("base position", breaks=x.breaks,minor_breaks=NULL)
print(p)

library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-breakpoint-error-pieces.tex",w=5,h=2.5)
print(p)
dev.off()
