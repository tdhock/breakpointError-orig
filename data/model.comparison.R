## Do cross-validation to compare regularized hi-dimensional interval
## regression, unregularized low-dimensional interval regression, and
## the usual cghseg.k constant model.

for(item in c("lambda.matrices","signal.features","L.min.max",
              "exact.cost","cost.matrices")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

source("scripts/display.coefs.R")
source("scripts/interval-regression.R")
source("scripts/pick.best.index.R")
source("scripts/annotation.error.R")

fl.fun <- function(variables=colnames(signal.features),
                   FUN=regression.funs$square){
  force(FUN)
  force(variables)
  function(train.pid.chr,...){
    f <- signal.features[train.pid.chr,variables,drop=FALSE]
    l <- log.lambda.limits[train.pid.chr,,drop=FALSE]
    fit <- FUN(features=f,limits=l,...)
    list(fit=fit,predict=function(test.pid.chr){
      X <- signal.features[test.pid.chr,variables,drop=FALSE]
      fit$predict(X)
    })
  }
}
### a list of functions: each takes a vector of pid.chr ids in the
### training set, and returns a list with a predict function: pid.chr
### -> log.lambda values.
model.funs <- list(cghseg.k=function(train.pid.chr){
  train.error <- rowSums(lambda.info$cost[,train.pid.chr])
  best <- pick.best.index(train.error)
  l <- lambda.info$lambda.grid[best]
  list(predict=function(test.pid.chr){
    di <- signal.features[test.pid.chr,"n"]
    log(l*di)
  })
})
## add univariate interval regression methods.
univariate.models <- c("mad","log.mad","hall","log.hall","log.n")
for(variable in univariate.models){
  model.funs[[variable]] <- fl.fun(variable)
}

## multivariate linear model inspired by the optimal penalty
model.funs$log.hall.log.n <- fl.fun(c("log.hall","log.n"))
model.funs$log.hall.n.bases <- fl.fun(c("log.hall","log.n","log.bases"))
model.funs$log.hall.log.n.log2.n <- fl.fun(c("log.hall","log.n","log2.n"))
model.funs$log.hall.emilie <- fl.fun(c("log.hall","emilie"))
model.funs$log.hall.n.emilie <- fl.fun(c("log.hall","log.n","emilie"))

cv.interval.regression <- function
### l1-regularized model tuned by internal cv loop.
(features,
### numeric feature matrix n x p.
 limits,
### numeric log(lambda) matrix n x 2.
 n.folds=10,
### cross-validation folds.
 seed=1,
 calc.grad=calc.grad.list$square,
 calc.loss=calc.loss.list$square,
 ...
### passed to regularized.interval.regression.
 ){
  pid.chr.all <- rownames(limits)
  set.seed(seed)
  fold <- sample(rep(1:n.folds,l=length(pid.chr.all)))
  best.gamma <- rep(NA,n.folds)
  fold.results <- list()
  for(test.fold in 1:n.folds){
    cat(sprintf("fold %5d / %5d\n",test.fold,n.folds))
    is.test <- fold == test.fold
    is.train <- !is.test
    fit <- regularized.interval.regression(features[is.train,],limits[is.train,],
                                           calc.grad=calc.grad,
                                           calc.loss=calc.loss)
    L.hat <- fit$predict(features[is.test,])
    error.mat <- matrix(NA,nrow(L.hat),ncol(L.hat))
    rownames(error.mat) <- rownames(L.hat)
    count.mat <- matrix(NA,nrow(L.hat),ncol(L.hat))
    rownames(count.mat) <- rownames(L.hat)
    for(i in 1:nrow(L.hat)){
      pid.chr <- rownames(L.hat)[i]
      if(pid.chr %in% names(interval.df.list)){ ## we have no anns for some
        count.mat[pid.chr,] <- ann.counts[pid.chr]
        pid.chr.intervals <- interval.df.list[[pid.chr]]
        for(j in 1:ncol(L.hat)){
          error.mat[pid.chr,j] <-
            annotation.error(L.hat[pid.chr,j],pid.chr.intervals)
        }
      }
    }
    error <- apply(error.mat,2,sum)/apply(count.mat,2,sum)
    fold.results[[test.fold]] <-
      list(fit=fit,
           error.mat=error.mat,
           count.mat=count.mat,
           L.hat=L.hat,
           error=error)
    best.gamma[test.fold] <- fit$gamma.seq[which.min(error)]
  }
  print(t(t(best.gamma)))
  gamma <- mean(best.gamma)
  cat(sprintf("fitting model on entire data set with gamma=%f\n",gamma))
  ##browser()
  fit <- smooth.interval.regression(features=features,limits=limits,gamma=gamma,
                                    calc.grad=calc.grad,
                                    calc.loss=calc.loss)
  fit$best.gamma <- best.gamma
  fit$fold.results <- fold.results
  fit$fold <- fold
  fit
}

model.funs$lasso <- fl.fun(FUN=cv.interval.regression)

##test on 2 folds to see if the code is not buggy.
## fit <-
##   cv.interval.regression(signal.features[all.pid.chr,],
##                          log.lambda.limits,
##                          n.folds=2)
## fit$predict(signal.features[all.pid.chr,])
## noreg <- model.funs$log.hall.log.n(all.pid.chr)
## noreg$predict

model.comparison <- list(error=data.frame(),fit=list())
for(ann.set in names(exact.cost)){
  log.lambda.limits <- L.min.max[[ann.set]]
  interval.df.list <- exact.cost[[ann.set]]
  lambda.info <- lambda.matrices[[sprintf("web.%s",ann.set)]]
  ann.counts <- sapply(cost.matrices[[ann.set]],nrow)

  all.pid.chr <- names(interval.df.list)
  n.folds <- 10
  set.seed(1)
  folds <- sample(rep(1:n.folds,l=length(all.pid.chr)))
  table(folds)

  model.comparison$fit[[ann.set]] <- list()
  for(test.fold in 1:n.folds){
    is.test <- folds == test.fold
    is.train <- !is.test
    train.pid.chr <- all.pid.chr[is.train]
    test.pid.chr <- all.pid.chr[is.test]

    model.comparison$fit[[ann.set]][[test.fold]] <- list()
    for(FUN.name in c("cghseg.k","log.n","log.hall.log.n","lasso")){
      cat(sprintf("%15s %15s fold %3d / %3d\n",
                  ann.set,FUN.name,test.fold,n.folds))
      FUN <- model.funs[[FUN.name]]
      seconds <- system.time({
        fit <- FUN(train.pid.chr)
      })["elapsed"]
      if(FUN.name!="cghseg.k"){
        model.comparison$fit[[ann.set]][[test.fold]][[FUN.name]] <-
          with(fit$fit,display.coefs(intercept,weights,mu,sigma))
      }
      
      test.Lhat <- fit$predict(test.pid.chr)
      stopifnot(length(test.Lhat)==length(test.pid.chr))
      test.errors <- test.Lhat
      test.counts <- lambda.info$count[test.pid.chr]
      ## judge agreement to held-out test annotations:
      for(test.i in seq_along(test.pid.chr)){
        pid.chr <- test.pid.chr[test.i]
        tL <- test.Lhat[test.i]
        intervals <- interval.df.list[[pid.chr]]
        test.errors[test.i] <- annotation.error(tL, intervals)
      }
      new.row <-
        data.frame(test.errors=sum(test.errors),
                   test.anns=sum(test.counts),
                   model=FUN.name,
                   test.fold,
                   seconds,
                   ann.set)
      model.comparison$error <- rbind(model.comparison$error,new.row)
    }
  }
}
save(model.comparison,file="data/model.comparison.RData")
