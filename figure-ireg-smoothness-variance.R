## this script makes a figure which explains the interval regression
## procedure in order to predict lambda for a new profile based on
## some stats on that profile.

source("scripts/pick.best.index.R")
source("scripts/left.right.colors.R")
## the lambda matrices which quantify the annotation error for each
## signal.
if(!"signal.list"%in%ls())load("data/signal.list.RData")
if(!"lambda.matrices"%in%ls())load("data/lambda.matrices.RData")
if(!"L.min.max"%in%ls())load("data/L.min.max.RData")
if(!"exact.cost.RData"%in%ls())load("data/exact.cost.RData")
ann.set <- "detailed.low.density"
L <- lambda.matrices[[sprintf("web.%s",ann.set)]]
all.limits <- L.min.max[[ann.set]]
interval.df.list <- exact.cost[[ann.set]]

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
  X <- do.call(rbind,lapply(rownames(all.limits),feature.vector))
}
## log.limits <- log10(all.limits)
## par(mfrow=c(ncol(X),1))
## for(j in 1:ncol(X)){
##   x <- X[,j]
##   plot(range(x),range(log.limits,na.rm=TRUE),type="n")
##   segments(x,log.limits[,1],x,log.limits[,2])
## }


## ## scale the data?
## head(X)
## apply(X,2,mean)

library.install <- function(x,repos=getOption("repos"),type="source"){
  if(!require(x,character.only=TRUE)){
    install.packages(x,repos=repos,type=type)
    library(x,character.only=TRUE)
  }
}
library.install("quadmod",repos="http://r-forge.r-project.org")
library.install("quadprog")
sv.regression <- function(features,limits,tune.C=1){
  ## reality checks.
  stopifnot(nrow(features)==nrow(limits))
  if(ncol(limits)!=2){
    cat("str(limits)=\n")
    str(limits)
    stop("limits should be a 2-column matrix, with NA for no limit")
  }
  stopifnot(is.matrix(features))
  
  ## check if there are any flat error curves, which have no limits.
  has.limits <- apply(!is.na(limits),1,any)
  ## we train the model on this subset.
  some.limits <- limits[has.limits,]
  some.features <- features[has.limits,,drop=FALSE]


  scaled <- scale(some.features)
  mu <- attr(scaled,"scaled:center")
  sigma <- attr(scaled,"scaled:scale")

  n <- nrow(scaled)
  p <- ncol(scaled)
  vars <- make.ids(slack=n,intercept=1,normal=p)
  cat("slack positivity constraint construction\n")
  ## when we have 2 limits \__/ 3 constraints are necessary in this
  ## case, but not in this case \/. maybe we can check and only add
  ## the constraint if necessary?
  constraints <- vars$slack >= 0 
  for(i in 1:n){
    cat(sprintf("slack example constraints %5d / %5d\n",i,n))

    left <- some.limits[i,1]
    if(is.finite(left)){
      ivars <- with(vars,{
        intercept * 1 + sum(normal)*scaled[i,] + slack[i]
      })
      constraints <- c(constraints,list(ivars >= 1 + left))
    }

    right <- some.limits[i,2]
    if(is.finite(right)){
      ivars <- with(vars,{
        intercept * -1 + sum(normal)*scaled[i,]*-1 + slack[i]
      })
      constraints <- c(constraints,list(ivars >= 1 - right))
    }

  }
  const.info <- standard.form.constraints(constraints,vars)
  n.vars <- length(unlist(vars))
  Dvec <- rep(1e-6,n.vars)
  Dvec[vars$normal] <- 1
  D <- diag(Dvec)
  d <- rep(0,n.vars)
  d[vars$slack] <- -tune.C ## like C in svm
  cat(sprintf("solving for %d variables and %d constraints... ",
              n.vars,length(constraints)))
  ##browser()
  sol <- solve.QP(D,d,const.info$A,const.info$b0)
  cat("solved!\n")
  sol$scaled <- scaled
  sol$log.limits <- some.limits
  sol$normal <- sol$solution[vars$normal]
  sol$intercept <- sol$solution[vars$intercept]
  sol$slack <- sol$solution[vars$slack]
  ## this function will be applied to new data before applying the
  ## model.
  sol$normalize <- function(X){
    mu.mat <- matrix(mu,nrow(X),ncol(X),byrow=TRUE)
    s.mat <- matrix(sigma,nrow(X),ncol(X),byrow=TRUE)
    (X-mu.mat)/s.mat
  }
  sol$f <- function(x){
    sum(x*sol$normal)+sol$intercept
  }
  sol$predict <- function(X){
    stopifnot(is.matrix(X))
    X.norm <- sol$normalize(X)
    normal.mat <- matrix(sol$normal,nrow(X),ncol(X),byrow=TRUE)
    L.hat <- rowSums(X.norm * normal.mat) + sol$intercept
    exp(L.hat)
  }
  sol$L.pred <- apply(scaled,1,sol$f)
  sol$lambda.pred <- sol$predict(features)
  sol
}

## small example
#set.seed(6)
#small.i <- sample(1:nrow(X),15)[-11]
##set.seed(101200110) ##TODO: need to actually specify the pid.chr data to
seed <- 13
##set.seed(seed <- seed+1)
set.seed(seed)
               ##use for this plot.
##small.i <- c(16,908,239,1949,7,2,6,15)##sample(1:nrow(X),10)[-c(8,2)]
small.i <- sample(1:nrow(X),10)
small.X <- X[small.i,1,drop=FALSE]
small.limits <- all.limits[small.i,]
rownames(small.X) <- rownames(small.limits)



## inputs of the solver are this matrix and X
small.input.limits <- all.limits[small.i,]




## plot setup.
left.present <- is.finite(small.input.limits[,1])
right.present <- is.finite(small.input.limits[,2])
pid.chr.colors <- c("#619cff",
                  "brown",
                  "#00ba38")
names(pid.chr.colors) <-
  rownames(small.input.limits)[c(which(left.present & (!right.present))[2],
                                 which(left.present & right.present)[2],
                                 which(!(left.present) & right.present)[1])]
show.X <- small.X[names(pid.chr.colors),]
ord <- names(show.X)[order(show.X)]




## apply the solver
result <- sv.regression(small.X,small.input.limits,1e6)
regularized <- sv.regression(small.X,small.input.limits,0.08)

## these limits are needed for the first plot as well.
scaled.feature <- result$scaled[,1]
limits.df <-
  rbind(data.frame(scaled.feature,
                   log.lambda=result$log.limits[,1],
                   pid.chr=names(scaled.feature),
                   limit="left"),
        data.frame(scaled.feature,
                   log.lambda=result$log.limits[,2],
                   pid.chr=names(scaled.feature),
                   limit="right"))

## calculate info for drawing the regression lines.
result.list <- list(regularized,result)
margins.df <- do.call(rbind,lapply(result.list,function(result){
  l.hat <- sapply(scaled.feature,result$f)
  margins <- rbind(data.frame(left=result$log.limits[,1],
                              right=l.hat,
                              scaled.feature),
                   data.frame(left=l.hat,
                              right=result$log.limits[,2],
                              scaled.feature))
  margins$margin <- margins$right - margins$left
  min.margin <- min(margins$margin,na.rm=TRUE)
  is.margin <- margins$margin == min.margin
  print(margins)
  margins[which(is.margin),]
}))

error.df <- list()
prev.max <- 0
for(pid.chr in ord){
  this.df <- data.frame(pid.chr,interval.df.list[[pid.chr]])
  this.df$errors <- this.df$cost+prev.max+1
  error.df <- rbind(error.df,this.df)
  prev.max <- max(this.df$errors)
}
L.range <- with(error.df,c(min(max.L),max(min.L)))
L.grid <- seq(L.range[1]-1,L.range[2]+1,l=100)
shown.E <- interval.df.list[rownames(small.limits)]
E.grid <- sapply(L.grid,function(L){
  sum(sapply(shown.E,function(df){
    subset(df,min.L < L & L < max.L)$cost
  }))
})
stopifnot(min(E.grid)>0)

line.df <-
  data.frame(intercept=sapply(result.list,with,-intercept/normal),
             slope=sapply(result.list,with,1/normal))
best.constant.lambda <-
  L.grid[pick.best.index(E.grid)]


drawlines <- function(){
  for(j in 1:nrow(line.df)){
    with(line.df[j,],abline(intercept,slope))
  }
  
  abline(v=best.constant.lambda)
}

make.finite <- function(x){
  x[x[,"min.L"]==-Inf,"min.L"] <- L.range[1]-1
  x[x[,"max.L"]==Inf,"max.L"] <- L.range[2]+1
  x
}
error.df <- make.finite(error.df)


## base graphics plot
##pdf("figure-ireg-smoothness-variance.pdf",5,6)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-smoothness-variance.tex",5,6)
par(mfrow=c(3,1),mar=c(0,4,2,2),las=1,xpd=NA)
plot(L.range,c(min(error.df$errors),max(error.df$errors)+1),type="n",
     xaxt="n",
     ylab="adjusted annotation error for 3 signals",
     xlab="")
par(xpd=FALSE)

for(pid.chr in ord){
  pid.chr.disp <- paste("profile",sub("[.]","chr",pid.chr),sep="")
  pid.chr.col <- pid.chr.colors[pid.chr]
  sub.df <- error.df[error.df$pid.chr == pid.chr,]
  with(sub.df,{
    text(max.L[1],errors[1],pid.chr.disp,col=pid.chr.col,adj=c(0,-0.5))
    ## imprecise lines.
    ##lines(log.lambda,errors,col=pid.chr.col,lwd=3)
    segments(min.L,errors,max.L,errors,col=pid.chr.col,lwd=2)
  })
  sub.lim <- limits.df[limits.df$pid.chr==pid.chr,]
  sub.lim$errors <- min(sub.df$errors)
  with(sub.lim,{
    points(log.lambda,errors,
           bg=left.right.colors[as.character(limit)],
           pch=21)
  })
}


## plot solver results
result.limits <- result$log.limits
result.limits[is.na(result.limits[,1]),1] <- L.range[1]
result.limits[is.na(result.limits[,2]),2] <- L.range[2]
bottom.lines <- 5
par(mar=c(bottom.lines/2,4,bottom.lines/2,2),xpd=NA)
plot(L.range,range(result$scaled),type="n",
     xaxt="n",xlab="",
     ylab="log(variance estimate) intervals")
par(xpd=FALSE)
seg.colors <- rep("grey40",nrow(result.limits))
names(seg.colors) <- rownames(result.limits)
seg.colors[names(pid.chr.colors)] <- pid.chr.colors
result.limits <- make.finite(result.limits)
seg.lwd <- rep(1,nrow(result.limits))
seg.lwd[rownames(result.limits)%in%names(pid.chr.colors)] <- 2
segments(result.limits[,1],result$scaled,
         result.limits[,2],result$scaled,
         lwd=seg.lwd,col=seg.colors)

##abline(result$intercept,result$normal)

  
drawlines()
## manual slack calculation.
slack <- with(result,cbind(1+log.limits[,1]-L.pred,1+L.pred-log.limits[,2]))
slack[slack < 0] <- 0
slack[is.na(slack)] <- 0
cbind(apply(slack,1,max),result$slack)
slack <- with(result,cbind(1+log.limits[,1]-L.pred,1+L.pred-log.limits[,2]))
almost.zero <- function(x)abs(x) < 1e-6
is.sv <- almost.zero(slack)
left.sv <- which(is.sv[,1])
right.sv <- which(is.sv[,2])
left.slack <- which(almost.zero(slack[,1]-result$slack) & !is.sv[,1])
right.slack <- which(almost.zero(slack[,2]-result$slack) & !is.sv[,2])
## the point where we start picking up slack (i.e. the margin, but I
## don't think it is really the same thing here).
slack.df <-
  rbind(data.frame(scaled.feature=result$scaled[left.slack],
                   min.log.lambda=result$L.pred[left.slack],
                   max.log.lambda=result$log.limits[left.slack,1]+1,
                   limit=rep("left",length(left.slack))),
        data.frame(scaled.feature=result$scaled[right.slack],
                   min.log.lambda=result$log.limits[right.slack,2]-1,
                   max.log.lambda=result$L.pred[right.slack],
                   limit=rep("right",length(right.slack))))
sv.df <-
  rbind(data.frame(scaled.feature=result$scaled[left.sv],
                   log.lambda=result$log.limits[left.sv,1]+1,
                   limit=rep("left",length(left.sv))),
        data.frame(scaled.feature=result$scaled[right.sv],
                   log.lambda=result$log.limits[right.sv,2]-1,
                   limit=rep("right",length(right.sv))))
## plot using points since it is less cluttered.
with(limits.df,{
  points(log.lambda,scaled.feature,bg=left.right.colors[limit],pch=21)
})

par(mar=c(bottom.lines,4,0,2),xpd=NA)
## Plot 3: points + model residuals.
plot(L.range,range(scaled.feature),type="n",
     ylab="log(variance estimate) points",
     xlab=paste("<- more breakpoints",
       "Linear penalty exponent $L$",
       "fewer breakpoints ->",
       sep="\\hskip 1cm"))
par(xpd=FALSE)
with(limits.df,{
  points(log.lambda,scaled.feature,bg=left.right.colors[limit],pch=21)
})
drawlines()
with(margins.df,segments(left,scaled.feature,right,scaled.feature,
                         col="red",lty=c("solid","dashed")))
y.text <- min(scaled.feature)+0.6
par(cex=0.5)
text(best.constant.lambda+0.1,y.text,
     "1 error\nconstant",adj=c(0,0.5))
x.text <- with(line.df,(y.text - intercept)/slope)
ann.text <- c("0 errors\nsmall margin","0 errors\nlarge margin")
text(x.text+c(0.4,-0.6),y.text,ann.text)


dev.off()##;system("evince figure-ireg-smoothness-variance.pdf")

## rotated plot using ggplot2.
## library(ggplot2)
## ggplot(limits.df,aes(log.lambda,scaled.feature,colour=limit))+
##   geom_point(data=limits.df)+
##   geom_segment(aes(y=scaled.feature,yend=scaled.feature,
##                    x=min.log.lambda,xend=max.log.lambda),data=slack.df)+
##   geom_point(data=sv.df,size=5,pch=2)+
##   geom_abline(aes(intercept=intercept,slope=slope),data=line.df)


## References


## Regression with Interval Ouput Values. Hisashi Kashima, Kazutaka
## Yamasaki, Akihiro Inokuchi, and Hiroto Saigo.

## A numerical solution method to interval quadratic programming.
## Shiang-Tai Liu, Rong-Tsu Wang. 2007 Applied Mathemetics and
## Computation.

