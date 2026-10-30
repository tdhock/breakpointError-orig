load("data/overfitting.RData")

sets <- list(train=overfitting$pid.chr.train,
             test=overfitting$pid.chr.test)

overfit.df <- do.call(rbind,lapply(names(sets),function(set){
  pid.chr <- sets[[set]]
  counts <- overfitting$count.mat[pid.chr,]
  errors <- overfitting$error.mat[pid.chr,]
  L.set.lim <- overfitting$log.lambda.limits[pid.chr,]
  do.call(rbind,lapply(1:ncol(errors),function(j){
    L.set.pred <- overfitting$L.hat[pid.chr,j]
    loss <- overfitting$calc.loss.from.lp(L.set.pred, L.set.lim)
    gamma <- overfitting$fit$gamma.seq[j]
    stat.list <-
      list(annotation.error.percent=100*sum(errors[,j])/sum(counts[,j]),
           surrogate.loss=loss)
    base.df <- data.frame(j,set,gamma)
    do.call(rbind,lapply(names(stat.list),function(stat){
      data.frame(base.df,stat,value=stat.list[[stat]])
    }))
  }))
}))

save(overfit.df,file="data/overfit.df.RData")
