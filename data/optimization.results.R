## Do cross-validation to compare interval regression with the usual
## cghseg.k constant model.

for(item in c("signal.list","L.min.max")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

ann.set <- "detailed.low.density"
log.lambda.limits <- L.min.max[[ann.set]]

source("optimization.R")

feature.vector <- function(pid.chr){
  Y <- signal.list[[pid.chr]]$logratio
  c(med.abs.diff=log10(median(abs(diff(Y)))))
}
one.vector <- feature.vector("1.1")
print(one.vector)

if(!"feature.matrix"%in%ls()){
  feature.matrix <- do.call(rbind,lapply(names(signal.list),feature.vector))
}
rownames(feature.matrix) <- names(signal.list)

all.pid.chr <- rownames(log.lambda.limits)
train.sizes <- c(20,40,60,80,120,150,300)
optimization.results <- data.frame()
max.folds <- 10
for(train.size in train.sizes){
  set.seed(1)
  n.folds <- floor(length(all.pid.chr)/train.size)
  folds <- sample(rep(1:n.folds,l=length(all.pid.chr)))
  n.folds <- max.folds ## for speed!
  for(train.fold in 1:n.folds){
    is.train <- folds == train.fold
    is.test <- !is.train
    train.pid.chr <- all.pid.chr[is.train]
    train.X <- feature.matrix[train.pid.chr,,drop=FALSE]
    train.limits <- log.lambda.limits[train.pid.chr,,drop=FALSE]
    for(FUN.name in names(calc.grad.list)){
      calc.grad <- calc.grad.list[[FUN.name]]
      for(acceleration in c("none","vandenberghe")){
        for(step.size.type in c("bb","constant")){
          cat(sprintf("%10s %10s %10s size %4d fold %5d / %5d\n",
           FUN.name,acceleration,step.size.type,train.size,train.fold,n.folds))
          seconds <- system.time({
            fit <-
              smooth.interval.regression(train.X,train.limits,
                                         calc.grad=calc.grad,
                                         acceleration=acceleration,
                                         step.size.type=step.size.type)
          })["elapsed"]
          new.row <- data.frame(fit$iterations,
                                problem.size=train.size,
                                loss=FUN.name,
                                data.set=train.fold,
                                step.size.type=step.size.type)
          optimization.results <- rbind(optimization.results,new.row)
        }
      }
    }
  }
}
save(optimization.results,file="data/optimization.results.RData")
