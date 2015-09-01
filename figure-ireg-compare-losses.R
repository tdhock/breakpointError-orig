## Do an interval regression on a small data set to see what is going
## wrong with the log loss.

for(item in c("signal.list","L.min.max","exact.cost")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

ann.set <- "detailed.low.density"
log.lambda.limits <- L.min.max[[ann.set]]
interval.df.list <- exact.cost[[ann.set]]

source("interval-regression.R")

feature.vector <- function(pid.chr){
  Y <- signal.list[[pid.chr]]$logratio
  c(mad=log10(median(abs(diff(Y)))))
}
one.vector <- feature.vector("1.1")
print(one.vector)

if(!"feature.matrix"%in%ls()){
  feature.matrix <- do.call(rbind,lapply(names(signal.list),feature.vector))
  rownames(feature.matrix) <- names(signal.list)
}

set.seed(1)
pid.chr <- sample(rownames(log.lambda.limits),50)

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

regression.funs$hinge <- hinge.interval.regression

phi <- function(x)log(1+exp(-x))

predict.df <- data.frame()
coef.df <- data.frame()
train.points.df <- list()
for(balance in names(data.sets)){
  data.set <- data.sets[[balance]]

  ## construct data.frame of points to plot.
  data.df <- do.call(rbind,lapply(c("min","max"),function(limit){
    d <- data.frame(limit,balance,
                    feature=as.vector(data.set$feat),
                    log.lambda=data.set$lim[,sprintf("%s.L",limit)])
    subset(d,is.finite(log.lambda))
  }))
  train.points.df <- rbind(train.points.df,data.df)

  ## run each method on these data.
  results <- list()
  train.features <- data.set$features
  train.limits <- data.set$limits
  losses <- rep(NA,length(regression.funs))
  names(losses) <- names(regression.funs)
  for(method in names(regression.funs)){
    FUN <- regression.funs[[method]]
    fit <- FUN(train.features,train.limits,verbose=0)
    results[[method]] <- fit
    coef.df <- rbind(coef.df,with(fit,{
      data.frame(method,balance,
                 intercept=mu-sigma*intercept/weights,slope=sigma/weights)
    }))
    test.grid <- t(t(seq(min(train.features),max(train.features),l=10)))
    colnames(test.grid) <- colnames(train.features)
    d <- data.frame(log.lambda=fit$predict(test.grid),
                    feature=as.vector(test.grid),
                    method,balance)
    predict.df <- rbind(predict.df,d)
    
    L.hat <- fit$predict(train.features)
    loss <- phi(L.hat-train.limits[,1])+phi(train.limits[,2]-L.hat)
    losses[method] <- sum(loss)
  }
  print(balance)
  print(losses)
}
library(ggplot2)
library(directlabels)
bottom.qp.expand <-
  list(gapply.fun(transform(d[which.min(d$y),],hjust=0.5,vjust=1)),
       "calc.boxes",
       dl.trans(w=w+0.2),
       "calc.borders",
       qp.labels("x","w"))
source("loss.colors.R")
p <- ggplot(,aes(log.lambda,feature))+
  geom_point(aes(shape=limit),data=train.points.df)+
  #geom_abline(aes(intercept=intercept,slope=slope,linetype=method),data=coef.df)+
  geom_line(aes(colour=method,linetype=method),data=predict.df,lwd=1.5)+
  scale_colour_manual(values=loss.colors)+
  scale_shape_manual(values=c(min=1,max=20))+
  theme_grey()+
  opts(title=
       paste("Comparison of interval regression loss functions on separable data",
       #"coefficients (thin lines) agree with prediction functions (thick lines)",
         sep="\n"))+
  facet_grid(~balance)
labeled <- direct.label(p,"bottom.qp.expand")+
  guides(linetype="none",colour="none")
print(labeled)
pdf("figure-ireg-compare-losses.pdf",w=10)
print(labeled)
dev.off()
