load("data/lambda.matrices.RData")
source("active.rankers.R")

n.folds <- 10
n.seeds <- 50
mat.results <- list()
mat.names <- 
  c("web.original","narrower.original","wider.original","more.original")
ranker.names <- names(rankers)
##  c("each.side","k.range","random")
for(mat.name in mat.names){
  lambda.info <- lambda.matrices[[mat.name]]
  chrom.names <- names(lambda.info$count)
  n.annotated.chroms <- length(chrom.names)
  n.annotations <- sum(lambda.info$count)
  set.seed(1)
  folds <- sample(rep(1:n.folds,l=n.annotated.chroms))

  print(mat.name)
  for(ranker.name in ranker.names){

    print(ranker.name)
        
    several.seeds <- list()
    for(seed in 1:n.seeds){
      rank.chroms <- rankers[[ranker.name]]
      several.folds <- list()
      for(test.fold in 1:n.folds){
        one.path <- active(lambda.info,folds,test.fold,rank.chroms,seed=seed)
        test.errors <- sapply(one.path,"[[","test.errors")
        result.name <-
          sprintf("%d.%d.%s.%s",seed,test.fold,ranker.name,mat.name)
        several.folds[[test.fold]] <-
          data.frame(test.errors,train.chroms=seq_along(test.errors))
      }
      seed.df <- do.call(rbind,several.folds)
      by.train.chroms <- split(seed.df,seed.df$train.chroms)
      several.seeds[[seed]] <- sapply(by.train.chroms,function(df){
        sum(df$test.errors)/n.annotations*100
      })
    }
    size.seed.err <- do.call(cbind,several.seeds)
    means <- apply(size.seed.err,1,mean)
    sds <- apply(size.seed.err,1,sd)

    rname <- sprintf("%s.%s",mat.name,ranker.name)
    mat.results[[rname]] <- data.frame(train.chroms=seq_along(means),
               mean=means,sd=sds,ranker.name,mat.name)

  }
}

active.results <- do.call(rbind,mat.results)
rownames(active.results) <- NULL
save(active.results,file="data/active.results.RData")

