estimate.error <- function(a,b,train,test){
  l.star <- train$lambda.star(a,b)

  test.k <- test$kstar(l.star,a,b)
  test$cost[test.k]
}

test.all.pairs <- function(a,b,signal.list){
  n.signals <- length(signal.list)
  test.error <- c()
  for(i in 1:(n.signals-1)){
    cat(sprintf("alpha=%.1f beta=%.1f signal %5d / %5d\n",a,b,i,n.signals-1))
    for(j in (i+1):n.signals){
      err <- estimate.error(a,b,signal.list[[i]],signal.list[[j]])
      err2 <- estimate.error(a,b,signal.list[[j]],signal.list[[i]])
      test.error <- c(test.error,err,err2)
    }
  }
  test.error
}

load("data/variable.size.signals.RData")
a.df <- data.frame()
a.grid <- seq(-2,2,by=0.1)
b.grid <- a.grid
train.df.list <- list()
for(a in a.grid){
  for(b in b.grid){
    test.error <- test.all.pairs(a,b,variable.size.signals)
    err.df <- data.frame(test.error,pair=seq_along(test.error),alpha=a,beta=b)
    a.df <- rbind(a.df,err.df)
    ##Train error:
    err.mat <- sapply(variable.size.signals,function(sig){
      sig$lambda.error(a,b)
    })## matrix[lambda,signal]
    error <- rowSums(err.mat)
    train.df.list[[paste(a,b)]] <- 
      data.frame(alpha=a,beta=b,mean=min(error),sd=NA,what="train")
  }
}
train.df <- do.call(rbind, train.df.list)

library(plyr)
test.df <- ddply(a.df,.(alpha,beta),summarize,
      mean=mean(test.error),sd=sd(test.error),what="test")
stat.df <- rbind(train.df,test.df)

variable.size.alpha.beta <-
  list(train=train.df, test=test.df,
       stats=stat.df)

save(variable.size.alpha.beta, file="data/variable.size.alpha.beta.RData")
