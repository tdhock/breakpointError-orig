## Do cross-validation to compare interval regression with the usual
## cghseg.k constant model.

for(item in c("lambda.matrices","signal.list","L.min.max","exact.cost")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

ann.set <- "detailed.low.density"
log.lambda.limits <- L.min.max[[ann.set]]
interval.df.list <- exact.cost[[ann.set]]
lambda.info <- lambda.matrices[[sprintf("web.%s",ann.set)]]

source("interval-regression.R")
source("annotation.error.R")

feature.vector <- function(pid.chr){
  Y <- signal.list[[pid.chr]]$logratio
  c(log.med.abs.diff=log10(median(abs(diff(Y)))))
}
one.vector <- feature.vector("1.1")
print(one.vector)

if(!"feature.matrix"%in%ls()){
  feature.matrix <- do.call(rbind,lapply(names(signal.list),feature.vector))
}
rownames(feature.matrix) <- names(signal.list)

source("pick.best.index.R")

fl.fun <- function(FUN){
  force(FUN)
  function(pid.chr,...){
    f <- feature.matrix[pid.chr,,drop=FALSE]
    l <- log.lambda.limits[pid.chr,,drop=FALSE]
    FUN(features=f,limits=l,...)
  }
}
### a list of functions: each takes a vector of pid.chr ids in the
### training set, and returns a list with a predict function that
### returns predicted lambda values.
model.funs <- list(cghseg.k=function(pid.chr,...){
  train.error <- rowSums(lambda.info$cost[,pid.chr])
  best <- pick.best.index(train.error)
  list(predict=function(X){
    rep(log(lambda.info$lambda.grid[best]),nrow(X))
  })
},
##                    intReg=fl.fun(function(features,limits){
##   df <- data.frame(features)
##   fit <- intReg(limits~.,data=df)
##   list(predict=function(X){
##     predict(fit,X)
##   })
## }),
   ##hinge.loss=fl.fun(hinge.interval.regression),
   hinge.1e5=fl.fun(function(...)hinge.interval.regression(tune.C=1e5,...)))
for(loss.name in names(regression.funs)){
  model.funs[[loss.name]] <- fl.fun(regression.funs[[loss.name]])
}

all.pid.chr <- names(interval.df.list)
train.sizes <- c(20,40,60,80,120,150)
cv.results <- data.frame()
max.folds <- 20
for(train.size in train.sizes){
  set.seed(1)
  n.folds <- floor(length(all.pid.chr)/train.size)
  folds <- sample(rep(1:n.folds,l=length(all.pid.chr)))
  n.folds <- max.folds ## for speed!
  for(train.fold in 1:n.folds){
    cat(sprintf("size %4d fold %5d / %5d\n",train.size,train.fold,n.folds))
    is.train <- folds == train.fold
    is.test <- !is.train
    train.pid.chr <- all.pid.chr[is.train]
    test.pid.chr <- all.pid.chr[is.test]
    test.X <- feature.matrix[test.pid.chr,,drop=FALSE]
    for(FUN.name in names(model.funs)){
      print(FUN.name)
      FUN <- model.funs[[FUN.name]]
      seconds <- system.time({
        fit <- FUN(train.pid.chr)
      })["elapsed"]
      test.Lhat <- fit$predict(test.X)
      test.errors <- test.Lhat
      test.counts <- lambda.info$count[test.pid.chr]
      for(test.i in seq_along(test.pid.chr)){
        pid.chr <- test.pid.chr[test.i]
        intervals <- interval.df.list[[pid.chr]]
        tL <- test.Lhat[test.i]
        test.errors[test.i] <- annotation.error(tL,intervals)
      }
      new.row <-
        data.frame(test.errors=sum(test.errors),
                   test.anns=sum(test.counts),
                   model=FUN.name,
                   train.fold,
                   train.size,
                   seconds)
      cv.results <- rbind(cv.results,new.row)
    }
  }
}
save(cv.results,file="data/cv.results.RData")
