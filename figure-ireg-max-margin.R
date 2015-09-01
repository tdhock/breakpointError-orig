## Do an interval regression on a small separable data set to see the
## max margin separator.

for(item in c("signal.list","L.min.max","exact.cost")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

library(quadmod)
library(quadprog)
max.margin <- function
### Support vector interval regression for separable data. The idea is
### that we first normalize the feature matrix, giving normalized
### features x_i in R^p. Then we use a linear function f(x_i) = w'x_i
### + b to predict a log(lambda) that falls between all the log limits
### L_i^left and L_i^right and maximizes the margin. So the
### optimization problem is: max_{M,f} subject to, for all finite
### limits, L_i^right - f(x_i) >= M and f(x_i) - L_i^left >= M. Since
### we assume f is linear the problem becomes min_{w,b,M} -M subject
### to -M - w'x - b >= -L_i^right and -M + w'x + b >= L_i^left. We
### call M margin, w weights, b intercept.
(features,
### Matrix n x p of inputs: n signals, each with p features. We will
### scale these internally.
 limits,
### Matrix n x 2 of output lambda. Each row corresponds to the lower
### and upper bound of an interval on the log(lambda) which is optimal
### with respect to annotation error. Lower bound can be -Inf and
### upper bound can be Inf, which correspond to zero asymptotic
### cost. 
 verbose=0,
 ...
### ignored.
 ){
  ## reality checks.
  stopifnot(nrow(features)==nrow(limits))
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

  scaled <- scale(some.features)
  mu <- attr(scaled,"scaled:center")
  sigma <- attr(scaled,"scaled:scale")

  n <- nrow(scaled)
  p <- ncol(scaled)
  vars <- make.ids(margin=1,intercept=1,weights=p)
  constraints <- list(vars$margin*1 >= 0)
  for(i in 1:n){
    if(verbose >= 1)cat(sprintf("example constraints %5d / %5d",i,n))

    left <- some.limits[i,1]
    if(is.finite(left)){
      ivars <- with(vars,{
        intercept * 1 + sum(weights)*scaled[i,] + margin*-1
      })
      constraints <- c(constraints,list(ivars >= left))
    }

    right <- some.limits[i,2]
    if(is.finite(right)){
      ivars <- with(vars,{
        intercept * -1 + sum(weights)*scaled[i,]*-1 +margin*-1
      })
      constraints <- c(constraints,list(ivars >=  - right))
    }

    if(verbose >= 1)cat("\n")

  }
  const.info <- standard.form.constraints(constraints,vars)
  n.vars <- length(unlist(vars))
  Dvec <- rep(1e-10,n.vars)
  D <- diag(Dvec)
  d <- rep(0,n.vars)
  d[vars$margin] <- 1
  if(verbose >= 1)cat(sprintf("solving for %d variables and %d constraints... ",
              n.vars,length(constraints)))
  sol <- solve.QP(D,d,const.info$A,const.info$b0)
  if(verbose >= 1)cat("solved!\n")
  sol$mu <- mu
  sol$sigma <- sigma
  sol$scaled <- scaled
  sol$log.limits <- some.limits
  sol$weights <- sol$solution[vars$weights]
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
    sum(x*sol$weights)+sol$intercept
  }
  sol$predict <- function(X){
    stopifnot(is.matrix(X))
    X.norm <- sol$normalize(X)
    weights.mat <- matrix(sol$weights,nrow(X),ncol(X),byrow=TRUE)
    L.hat <- rowSums(X.norm * weights.mat) + sol$intercept
    L.hat
  }
  sol$L.pred <- apply(scaled,1,sol$f)
  sol$lambda.pred <- sol$predict(features)
  sol
### List of solver results. For a feature matrix X with p columns, you
### can use list$predict(X) to get model estimates of log(lambda).
}



ann.set <- "detailed.low.density"
log.lambda.limits <- L.min.max[[ann.set]]
interval.df.list <- exact.cost[[ann.set]]

feature.vector <- function(pid.chr){
  Y <- signal.list[[pid.chr]]$logratio
  c(med.abs.diff=log10(median(abs(diff(Y)))))
}
one.vector <- feature.vector("1.1")
print(one.vector)

if(!"feature.matrix"%in%ls()){
  feature.matrix <- do.call(rbind,lapply(names(signal.list),feature.vector))
  rownames(feature.matrix) <- names(signal.list)
}


set.seed(19)
pid.chr <- sample(rownames(log.lambda.limits),40)

train.features <- feature.matrix[pid.chr,,drop=FALSE]
train.limits <- log.lambda.limits[pid.chr,]

data.sets <-
  list(unbalanced=list(features=train.features,limits=train.limits))

right <- is.finite(train.limits[,2]) & !is.finite(train.limits[,1])
n.right <- sum(right)
left <- is.finite(train.limits[,1]) & !is.finite(train.limits[,2])
both <- is.finite(train.limits[,1]) & is.finite(train.limits[,2])
some.left <- sample(which(left),n.right)

pid.chr <- names(c(which(right),some.left,which(both)))
train.features <- feature.matrix[pid.chr,,drop=FALSE]
train.limits <- log.lambda.limits[pid.chr,]

data.sets$balanced <-
  list(features=train.features,limits=train.limits)

predict.df <- data.frame()
slack.df <- data.frame()
train.points.df <- data.frame()
for(balance in names(data.sets)){
  data.set <- data.sets[[balance]]

  L <- data.set$lim
  fit <- max.margin(data.set$feat,L,1)
  feature <- t(t(range(data.set$feat)))
  log.lambda <- fit$predict(feature)
  
  dist.to.separator <-
    L*matrix(c(-1,1),nrow(L),2,byrow=TRUE) -cbind(-fit$L.pred,fit$L.pred)
  margin <- min(dist.to.separator)
  support.vectors <- dist.to.separator-margin < 1e-3

  getdf <- function(margin,line){
    data.frame(balance,line,
               L.min=log.lambda[1]+margin,L.max=log.lambda[2]+margin,
               f.min=feature[1],f.max=feature[2])
  }
  L.sv <- L[support.vectors]
  x.sv <- data.set$feat[row(L)[support.vectors]]
  L.regressed <- fit$predict(t(t(x.sv)))
  model.df <- rbind(getdf(0,"regression"),
                    getdf(margin,"limit"),
                    getdf(-margin,"limit"),
                    data.frame(balance,line="margin",
                               L.min=L.sv,L.max=L.regressed,
                               f.min=x.sv,f.max=x.sv))
  predict.df <- rbind(predict.df,model.df)
  
  ## construct data.frame of points to plot.
  data.df <- do.call(rbind,lapply(c("min","max"),function(limit){
    colname <- sprintf("%s.L",limit)
    support.vector <-
      factor(ifelse(support.vectors[,colname],"yes","no"),c("yes","no"))
    d <- data.frame(limit,balance,
                    feature=as.vector(data.set$feat),
                    support.vector,
                    log.lambda=data.set$lim[,colname])
    subset(d,is.finite(log.lambda))
  }))
  train.points.df <- rbind(train.points.df,data.df)
}
library(ggplot2)

train.points.df <- subset(train.points.df,balance=="unbalanced")
L.latex <- c(min="$\\underline L_i$",max="$\\overline L_i$")
latex.sc <- c(1,20)
names(latex.sc) <- L.latex
train.points.df$L <- L.latex[as.character(train.points.df$limit)]
predict.df <- subset(predict.df,balance=="unbalanced")
lt.vals <- c(margin=1,regression=2,limit=3)
L.df <- data.frame(latex=c("$\\underline L_i$","$\\overline L_i$"),
                   L=c(-4,1),
                   shat=c(-1.1,-1.3))
p <- ggplot()+
  geom_text(aes(L,shat,label=latex),data=L.df)+
  geom_point(aes(log.lambda,feature,shape=L),
             data=train.points.df)+
  geom_segment(aes(L.min,f.min,xend=L.max,yend=f.max,linetype=line),
              data=predict.df)+
  scale_shape_manual("interval",values=latex.sc)+
  theme_bw()+
  ##opts(title="Maximum margin interval regression function")+
  ##facet_grid(.~balance)+
  scale_linetype_manual(values=lt.vals)+
  scale_size_manual("support\nvector",values=c(yes=4,no=2))+
  xlab("Linear penalty exponent $L$")+
  ylab("variance estimate $\\log\\sigma_i$")+
  guides(linetype="none",size="none",shape="none")
print(p)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-max-margin.tex")
print(p)
dev.off()
