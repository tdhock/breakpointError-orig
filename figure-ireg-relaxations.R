## make a figure that compares the various relaxations.

## the list of profiles to calculate some stats on each.
if(!"signal.list"%in%ls())load("data/signal.list.RData")
## we have already calculated the target interval:
if(!"L.min.max"%in%ls())load("data/L.min.max.RData")
if(!"exact.cost.RData"%in%ls())load("data/exact.cost.RData")
ann.set <- "detailed.low.density"
log.lambda <- L.min.max[[ann.set]]
interval.df.list <- exact.cost[[ann.set]]


source("scripts/interval-regression.R")
li.funs <- lapply(phi.list,function(phi){
  stopifnot(is.function(phi))
  function(L,left,right)phi(L-left)+phi(right-L)
})
  


feature.vector <- function(pid.chr){
  Y <- signal.list[[pid.chr]]$logratio
  c(med.abs.diff=log10(median(abs(diff(Y)))),
    var=var(Y),
    sd=sd(Y),
    size=length(Y))
}
one.vector <- feature.vector("1.1")
print(one.vector)

## construct some features and see if they are related to the selected
## lambda.
if(!"X"%in%ls()){
  X <- do.call(rbind,lapply(rownames(log.lambda),feature.vector))
}

## small example
##set.seed(6)
##small.i <- sample(1:nrow(X),15)[-11]
set.seed(101200110)
small.i <- sample(1:nrow(X),50)
small.X <- X[small.i,1,drop=FALSE]
small.limits <- log.lambda[small.i,]
rownames(small.X) <- rownames(small.limits)
## inputs of the solver are this matrix and X

left.present <- is.finite(small.limits[,1])
right.present <- is.finite(small.limits[,2])
show.pid.chr <- c("#619cff",
                  "brown",
                  "#00ba38",
                  "orange")
gap <- small.limits[,2]-small.limits[,1]
names(show.pid.chr) <-
  rownames(small.limits)[c(which(left.present & (!right.present))[1],
                           which(is.finite(gap) & gap < 2)[1],
                           which(is.finite(gap) & gap > 2)[1],
                           which(!(left.present) & right.present)[1])]
show.X <- small.X[names(show.pid.chr),]
ordered <- names(show.X)[order(show.X)]

## these limits are needed for the first plot as well.
limits.df <-
  rbind(data.frame(
                   log.lambda=small.limits[,1],
                   pid.chr=rownames(small.limits),
                   limit="left"),
        data.frame(
                   log.lambda=small.limits[,2],
                   pid.chr=rownames(small.limits),
                   limit="right"))

E.lab <- "annotation\nerror\n$E_i(L)$"

error.df <- data.frame()
prev.max <- 0
for(pid.chr in ordered){
  this.df <- data.frame(pid.chr,interval.df.list[[pid.chr]],
                        curve=E.lab)
  this.df$errors <- this.df$cost+prev.max+2
  error.df <- rbind(error.df,this.df)
  prev.max <- max(this.df$errors)
}

L.range <- with(error.df,c(min(max.L),max(min.L)))

label.df <- data.frame()
for(j in seq_along(show.pid.chr)){
  pid.chr <- names(show.pid.chr)[j]
  pid.chr.disp <- paste("profile",sub("[.]","chr",pid.chr),sep="")
  pid.chr.col <- show.pid.chr[j]
  sub.df <- error.df[error.df$pid.chr == pid.chr,]
  sub.df$max.L[sub.df$max.L == Inf] <- L.range[2]+1
  sub.df$min.L[sub.df$min.L == -Inf] <- L.range[1]-1

  label.df <- rbind(label.df,{
    data.frame(L=L.range[1],error=min(sub.df$errors)-1,label=pid.chr.disp)
  })
}

l.lab <- "surrogate\nloss\n$l_i(L)$"

li.df <- data.frame()
seg.df <- data.frame()

for(loss.name in names(li.funs)){
  
  li <- li.funs[[loss.name]]

  for(j in seq_along(show.pid.chr)){
    pid.chr <- names(show.pid.chr)[j]
    pid.chr.disp <- paste("profile",sub("[.]","chr",pid.chr),sep="")
    pid.chr.col <- show.pid.chr[j]
    sub.df <- error.df[error.df$pid.chr == pid.chr,]
    sub.df$max.L[sub.df$max.L == Inf] <- L.range[2]+1
    sub.df$min.L[sub.df$min.L == -Inf] <- L.range[1]-1

    m <- min(sub.df$error)
    pid.chr.limits <- log.lambda[pid.chr,]
    L.grid <- c(seq(L.range[1]-1,L.range[2]+1,l=100))
    surrogate.error <- li(L.grid, pid.chr.limits[1], pid.chr.limits[2])
    surrogate.error[surrogate.error > max(sub.df$cost)+1/2] <- NA
    sub.df$loss.name <- loss.name
    this.df <- 
      data.frame(loss.name,L=L.grid,
                 cost=min(sub.df$errors)+surrogate.error,
                 pid.chr,
                 curve=l.lab)

    li.df <- rbind(li.df,this.df)
    seg.df <- rbind(seg.df,sub.df)
  }

  li.df
}

library(ggplot2)
colorkey <- c("black","red")
sizekey <- c(2,1)
names(colorkey) <- c(E.lab,l.lab)
names(sizekey) <- c(E.lab,l.lab)
p <- ggplot()+
  geom_segment(aes(min.L,errors,xend=max.L,yend=errors,
                   colour=curve,size=curve),
               data=seg.df)+
  geom_line(aes(L,cost,colour=curve,size=curve,group=pid.chr),data=li.df)+
  facet_grid(loss.name~.)+
  scale_colour_manual(values=colorkey)+
  scale_size_manual(values=sizekey)+
  ##geom_text(aes(L,error,label=label),data=label.df)+
  guides(colour=guide_legend(keyheight=5))+
  xlab("Linear penalty exponent $L$")+
  scale_y_continuous("error/loss",breaks=NULL)

library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-relaxations.tex")
print(p)
dev.off()
