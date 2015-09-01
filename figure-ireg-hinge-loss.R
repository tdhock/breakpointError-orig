## the list of profiles to calculate some stats on each.  this script
## makes a figure which explains the interval regression procedure in
## order to predict lambda for a new profile based on some stats on
## that profile.

if(!"signal.list"%in%ls())load("data/signal.list.RData")
## the lambda matrices which quantify the annotation error for each
## signal.
if(!"lambda.matrices"%in%ls())load("data/lambda.matrices.RData")
L <- lambda.matrices$web.detailed.low.density

lambda.limits <- function(cost){
  is.min <- cost == min(cost)
  indices <- which(is.min)
  breaks <- which(diff(indices)>1)
  ends <- indices[c(breaks,length(indices))]
  starts <- indices[c(1,breaks+1)]
  picked <- which.max(ends-starts)
  limits <- c(starts[picked],ends[picked])
  L$lambda.grid[limits]
}
all.limits <- t(apply(L$cost,2,lambda.limits))

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

library.install <- function(x){
  if(!require(x,character.only=TRUE)){
    install.packages(x,repos="http://r-forge.r-project.org",type="source")
    require(x,character.only=TRUE)
  }
}
library.install("quadmod")
library.install("quadprog")
sv.regression <- function
### Support vector interval regression using a quadratic programming
### (QP) solver. The idea is that we first normalize the feature
### matrix, giving normalized features x_i in R^p. Then we use a
### linear function f(x_i) = w'x_i + b to predict a log(lambda) that
### falls between the log limits L_i^left and L_i^right. So the
### optimization problem is: min_f ||f|| + sum_i C*hinge(L_i^left,
### L_i^right, f(x_i)). Since we assume f is linear the problem
### becomes min_{w,b,z} w'w + sum_i C*z_i, with these constraints for
### all relevant i: z_i >= 0, z_i >= 1 + L_i^left - b - w'x_i, z_i >=
### 1 - b - w'x + L_i^right. We call z_i slack, w normal, b intercept.
(features,
### Matrix n x p of inputs: n signals, each with p features. We will
### scale these internally.
 limits,
### Matrix n x 2 of output lambda. Each row corresponds to the lower
### and upper bound of an interval on the lambda which is optimal with
### respect to annotation error. Lower bound can be 0 and upper bound
### can be Inf, which correspond to zero asymptotic cost. Internally,
### we take the log.
 tune.C=1){
  ## reality checks.
  stopifnot(nrow(features)==nrow(limits))
  if(any(limits < 0,na.rm=TRUE)){
    stop("limits should not be negative")
  }
  if(ncol(limits)!=2){
    cat("str(limits)=\n")
    str(limits)
    stop("limits should be a 2-column matrix")
  }
  stopifnot(is.matrix(features))
  
  ## check if there are any flat error curves, which have no limits.
  has.limits <- apply(is.finite(limits),1,any)
  ## we train the model on this subset.
  some.limits <- limits[has.limits,]
  some.features <- features[has.limits,,drop=FALSE]

  log.limits <- log(some.limits)

  scaled <- scale(some.features)
  mu <- attr(scaled,"scaled:center")
  sigma <- attr(scaled,"scaled:scale")

  n <- nrow(scaled)
  p <- ncol(scaled)
  vars <- make.ids(slack=n,intercept=1,normal=p)
  constraints <- list()
  for(i in 1:n){
    cat(sprintf("slack example constraints %5d / %5d",i,n))

    left <- log.limits[i,1]
    if(is.finite(left)){
      ivars <- with(vars,{
        intercept * 1 + sum(normal)*scaled[i,] + slack[i]
      })
      constraints <- c(constraints,list(ivars >= 1 + left))
    }

    right <- log.limits[i,2]
    if(is.finite(right)){
      ivars <- with(vars,{
        intercept * -1 + sum(normal)*scaled[i,]*-1 + slack[i]
      })
      constraints <- c(constraints,list(ivars >= 1 - right))
    }

    ## positivity.
    ## when we have 2 limits \__/ 3 constraints are necessary in this
    ## case, but not in this case \/. 
    gap <- right-left
    cat(sprintf(" gap=%4.2f",gap))
    if( (!is.finite(gap)) || (gap > 2) ){
      cat(" positivity constraint")
      constraints <- c(constraints,vars$slack[i] >= 0)
    }

    cat("\n")

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
  sol <- solve.QP(D,d,const.info$A,const.info$b0)
  cat("solved!\n")
  sol$scaled <- scaled
  sol$log.limits <- log.limits
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
### List of solver results. For a feature matrix X with p columns, you
### can use list$predict(X) to get model estimates of lambda.
}

## small example
#set.seed(6)
#small.i <- sample(1:nrow(X),15)[-11]
set.seed(101200110)
small.i <- sample(1:nrow(X),50)
small.X <- X[small.i,1,drop=FALSE]
small.limits <- all.limits[small.i,]
rownames(small.X) <- rownames(small.limits)
small.log <- log10(small.limits)



## inputs of the solver are this matrix and X
input.limits <- all.limits
input.limits[all.limits %in% min(L$lambda.grid)] <- 0
input.limits[all.limits %in% max(L$lambda.grid)] <- Inf
small.input.limits <- input.limits[small.i,]




## plot setup.
left.present <- small.input.limits[,1] > 0
right.present <- is.finite(small.input.limits[,2])
show.pid.chr <- c("#619cff",
                  "brown",
                  "#00ba38",
                  "orange")
ll <- log(small.input.limits)
gap <- ll[,2]-ll[,1]
names(show.pid.chr) <-
  rownames(small.input.limits)[c(which(left.present & (!right.present))[1],
                                 which(is.finite(gap) & gap < 2)[1],
                                 which(is.finite(gap) & gap > 2)[1],
                                 which(!(left.present) & right.present)[1])]
show.X <- small.X[names(show.pid.chr),]
ordered <- names(show.X)[order(show.X)]
error.df <- data.frame()
prev.max <- 0
for(pid.chr in ordered){
  this.df <- data.frame(pid.chr,errors=L$cost[,pid.chr]+prev.max+1,
             log.lambda=log(L$lambda.grid))
  error.df <- rbind(error.df,this.df)
  prev.max <- max(this.df$errors)
}




## apply the solver
result <- sv.regression(small.X,small.input.limits,1e3)

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
source("left.right.colors.R")
## base graphics plot
pdf("figure-ireg-hinge-loss.pdf")
    
par(mfrow=c(3,1),mar=c(0,4,2,2),las=1)
with(error.df,{
  plot(range(log(L$lambda.grid)),range(errors),type="n",
       main="Hinge loss relaxes the non-convex annotation loss",
     xaxt="n",
     ylab="adjusted annotation error",
     xlab="")
})
for(j in seq_along(show.pid.chr)){
  pid.chr <- names(show.pid.chr)[j]
  pid.chr.disp <- paste("profile",sub("[.]","chr",pid.chr),sep="")
  pid.chr.col <- show.pid.chr[j]
  sub.df <- error.df[error.df$pid.chr == pid.chr,]
  d <- diff(sub.df$err)
  ##print(data.frame(sub.df,diff=c(d,NA)))
  not.zero <- which(d!=0)
  seg.df <-
    data.frame(left.goes=c("inf",ifelse(d[not.zero]==-1,"up","down")),
               starts=c(1,not.zero+1),
               ends=c(not.zero,nrow(sub.df)),
               right.goes=c(ifelse(d[not.zero]==-1,"down","up"),"inf"))
  seg.df$errors <- sub.df[seg.df$starts,"errors"]
  ## adjust segment ends so that we can see the hinge loss upper bound
  ## the annotation error.
  for(seg.i in 1:nrow(seg.df)){
    if(seg.df[seg.i,"right.goes"]=="down"){
      seg.df[seg.i,"ends"] <- seg.df[seg.i,"ends"]+1
    }
    if(seg.df[seg.i,"left.goes"]=="down"){
      seg.df[seg.i,"starts"] <- seg.df[seg.i,"starts"]-1
    }
  }
  seg.df$L.min <- sub.df[seg.df$starts,"log.lambda"]
  seg.df$L.max <- sub.df[seg.df$ends,"log.lambda"]
  ## make it look like they go to infinity.
  seg.df$L.min[1] <- seg.df$L.min[1]-10
  seg.df$L.max[nrow(seg.df)] <- seg.df$L.max[nrow(seg.df)]+10
  with(sub.df,{
    ll <- log.lambda[1]-0.25
    yy <- min(errors)+0.3
    text(ll,yy,pid.chr.disp,col=pid.chr.col,adj=c(0,0.5))
    with(seg.df,segments(L.min,errors,L.max,errors,col=pid.chr.col,lwd=3))
  })
  sub.lim <- limits.df[limits.df$pid.chr == pid.chr,]
  points(sub.lim$log.lambda,rep(min(sub.df$errors),nrow(sub.lim)),
         col=left.right.colors[as.character(sub.lim$limit)],
         pch=16)
  m <- min(sub.df$error)
  surrogate.color <- "black"
  min.seg <- seg.df[which.min(seg.df$errors),]
  phi <- function(x)ifelse(x<1,1-x,0)
  li <- function(L){
    left <- phi(L-min.seg$L.min)
    right <- phi(min.seg$L.max-L)
    ifelse(left>right,left,right)
  }
  left.end <- min.seg$L.min+1
  midpoint <- with(min.seg,(L.max+L.min)/2)
  right.begin <- min.seg$L.max-1
  max.height <- max(seg.df$errors)+1/2
  left.begin <- min.seg$L.min+1-max.height+m
  right.end <- min.seg$L.max-1+max.height-m
  ll <- sort(c(left.begin,left.end,midpoint,right.begin,right.end))
  loss.value <- li(ll)+m
  lines(ll,loss.value)
}


## plot solver results
result.limits <- result$log.limits
result.limits[!is.finite(result.limits[,1]),1] <- min(log(L$lambda.grid))-1
result.limits[!is.finite(result.limits[,2]),2] <- max(log(L$lambda.grid))+1
bottom.lines <- 5
par(mar=c(bottom.lines/2,4,bottom.lines/2,2))
plot(range(log(L$lambda.grid)),range(result$scaled),type="n",
     xaxt="n",xlab="",
     ylab="log(variance estimate) intervals")
seg.colors <- rep("black",nrow(result.limits))
names(seg.colors) <- rownames(result.limits)
seg.colors[names(show.pid.chr)] <- show.pid.chr
segments(result.limits[,1],result$scaled,
         result.limits[,2],result$scaled,
         lwd=0.5,col=seg.colors)
fat.lines <- seg.colors != "black"
segments(result.limits[fat.lines,1],result$scaled[fat.lines,],
         result.limits[fat.lines,2],result$scaled[fat.lines,],
         lwd=2,col=seg.colors[fat.lines])

##library.install("iRegression")


regression.lines <- list(svr=result)
## TODO: read the references to see what optimization problem this is
## solving.
## library.install("intReg")
## y <- log(small.input.limits)
## small <- data.frame(log.var=result$scaled[,1])
## fit <- intReg(y~log.var,data=small)
## regression.lines$intReg <- list(intercept=coef(fit)[1],normal=coef(fit)[2]))
abcoefs <- function(L){
  with(L,{
    c(intercept=-intercept/normal,
      slope=1/normal)
  })
}
line.df <- data.frame(t(sapply(regression.lines,abcoefs)))
source("pick.best.index.R")
drawlines <- function(){
  for(j in 1:nrow(line.df)){
    with(line.df[j,],abline(intercept,slope))
  }
}
drawlines()
## manual slack calculation.
slack <- with(result,cbind(1+log.limits[,1]-L.pred,1+L.pred-log.limits[,2]))
slack[slack < 0] <- 0
slack[!is.finite(slack)] <- 0
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
## plot points with positive slack
## with(slack.df,{
##   segments(min.log.lambda,scaled.feature,
##            max.log.lambda,scaled.feature,
##            col="red",lwd=2)
## })
sv.df <-
  rbind(data.frame(scaled.feature=result$scaled[left.sv],
                   log.lambda=result$log.limits[left.sv,1]+1,
                   limit=rep("left",length(left.sv))),
        data.frame(scaled.feature=result$scaled[right.sv],
                   log.lambda=result$log.limits[right.sv,2]-1,
                   limit=rep("right",length(right.sv))))
## plot support vectors
## with(sv.df,{
##   points(log.lambda,scaled.feature,
##          pch=16,col="red",cex=2)
## })
## plot using points since it is less cluttered.
with(limits.df,{
  points(log.lambda,scaled.feature,col=left.right.colors[limit],pch=16)
})

par(mar=c(bottom.lines,4,0,2))
## Plot 3: points + model residuals.
plot(range(log(L$lambda.grid)),range(scaled.feature),type="n",
     ylab="log(variance estimate) points",
     xlab=paste("<- more breakpoints",
       "log(lambda) degree of smoothness",
       "fewer breakpoints ->",
       sep="                           "))
with(limits.df,{
  points(log.lambda,scaled.feature,col=left.right.colors[limit],pch=16)
})
drawlines()
with(sv.df,{
  points(log.lambda,scaled.feature,
         pch=16,col="red",cex=2)
})
with(slack.df,{
  segments(min.log.lambda,scaled.feature,
           max.log.lambda,scaled.feature,
           col="red",lwd=2)
})



## rotated plot using ggplot2.
## library(ggplot2)
## ggplot(limits.df,aes(log.lambda,scaled.feature,colour=limit))+
##   geom_point(data=limits.df)+
##   geom_segment(aes(y=scaled.feature,yend=scaled.feature,
##                    x=min.log.lambda,xend=max.log.lambda),data=slack.df)+
##   geom_point(data=sv.df,size=5,pch=2)+
##   geom_abline(aes(intercept=intercept,slope=slope),data=line.df)


## References

## Regression with Interval Output Values. Hisashi Kashima, Kazutaka
## Yamasaki, Akihiro Inokuchi, and Hiroto Saigo.

## A numerical solution method to interval quadratic programming.
## Shiang-Tai Liu, Rong-Tsu Wang. 2007 Applied Mathemetics and
## Computation.

dev.off()
