### Active learning path figure.

load("data/lambda.matrices.RData")
source("active.rankers.R")
compare.methods <- c("each.side","random")
  "other.side"
linfo <- lambda.matrices$narrower.orig
set.seed(1)
n.folds <- 10
folds <- sample(rep(1:n.folds,l=length(linfo$count)))
methods.result <-
  lapply(rankers[compare.methods],function(rank.fun){
    active(linfo,folds,1,rank.fun,seed=1)
  })
methods.err.curves <-
  do.call(rbind,lapply(names(methods.result),function(method){
  L <- methods.result[[method]]
  do.call(rbind,lapply(seq_along(L),function(train.chroms){
    train.errors <- L[[train.chroms]]$train.errors
    data.frame(train.errors,train.chroms,method,
               parameter=seq_along(train.errors))
  }))
}))
methods.optimal <-
  do.call(rbind,lapply(names(methods.result),function(method){
  L <- methods.result[[method]]
  do.call(rbind,lapply(seq_along(L),function(train.chroms){
    this.round <- L[[train.chroms]]
    train.errors <- min(this.round$train.errors)
    data.frame(train.errors,train.chroms,method,
               test.errors=this.round$test.errors,
               parameter=this.round$optimal.lambda)
  }))
}))
library(ggplot2)
one.path <- ggplot(,aes(parameter,train.errors,colour=method))+
  geom_line(data=methods.err.curves)+
  geom_point(aes(size=test.errors),data=methods.optimal)+
  facet_grid(train.chroms~.)
pdf("figure-ireg-path.pdf")
print(one.path)
dev.off()
