## Split the data in half to see if we can overfit with the
## multidimensional interval regression model.

source("scripts/interval-regression.R")
source("scripts/annotation.error.R")
for(item in c("signal.features","L.min.max","exact.cost","cost.matrices")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

ann.set <- "detailed.low.density"
log.lambda.limits <- L.min.max[[ann.set]]
interval.df.list <- exact.cost[[ann.set]]
ann.counts <- sapply(cost.matrices[[ann.set]],nrow)


pid.chr.all <- rownames(log.lambda.limits)
##set.seed(2)
set.seed(2)
folds <- sample(rep(1:2,l=length(pid.chr.all)))
is.test <- folds == 1
is.train <- !is.test
pid.chr.train <- pid.chr.all[is.train]
pid.chr.test <- pid.chr.all[is.test]

## train on 3 features:
all.features <- signal.features[,c("log.hall","log.n","log.bases")]
##all.features <- signal.features
train.limits <- log.lambda.limits[pid.chr.train,]
train.features <- all.features[pid.chr.train,]

loss.name <- "square"
calc.grad <- calc.grad.list[[loss.name]]
calc.loss <- calc.loss.list[[loss.name]]
calc.loss.from.lp <- calc.loss.from.lp.list[[loss.name]]
fit <- regularized.interval.regression(train.features,train.limits,
                                       calc.grad=calc.grad,
                                       calc.loss=calc.loss)
## test data
L.hat <- fit$predict(all.features)

error.mat <- matrix(NA,nrow(L.hat),ncol(L.hat))
rownames(error.mat) <- rownames(L.hat)
count.mat <- matrix(NA,nrow(L.hat),ncol(L.hat))
rownames(count.mat) <- rownames(L.hat)
for(i in 1:nrow(L.hat)){
  pid.chr <- rownames(L.hat)[i]
  cat(sprintf("%10s %5d / %5d\n",pid.chr,i,nrow(L.hat)))
  if(pid.chr %in% names(interval.df.list)){ ## we have no anns for some
    count.mat[pid.chr,] <- ann.counts[pid.chr]
    pid.chr.intervals <- interval.df.list[[pid.chr]]
    for(j in 1:ncol(L.hat)){
      error.mat[pid.chr,j] <- annotation.error(L.hat[pid.chr,j],pid.chr.intervals)
    }
  }
}

overfitting <- list(fit=fit,error.mat=error.mat,count.mat=count.mat,
                    log.lambda.limits=log.lambda.limits,
                    pid.chr.train=pid.chr.train,
                    pid.chr.test=pid.chr.test,
                    L.hat=L.hat,
                    calc.loss.from.lp=calc.loss.from.lp)
                    
save(overfitting,file="data/overfitting.RData")

