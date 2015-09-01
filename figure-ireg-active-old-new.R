## the range of k that appear on the plot of kstar vs lambda.
load("data/lambda.matrices.RData")
linfo <- lambda.matrices$web.detailed.low.density
param.range <- apply(linfo$optimal.seg.mat,2,function(x)diff(range(x)))

chrom.names <- names(param.range)

## another way to quantify ambiguity is with the maximum of the counts
## of kstar.
param.counts <- apply(linfo$optimal.seg.mat,2,function(x)table(x))
max.count <- sapply(param.counts,max)

## locations of breakpoints in the kstar vs lambda plot for each chrom.
kstar.diffs <- apply(linfo$optimal.seg.mat,2,diff)


## easy lookup of model agreement based on region_id
cost.mat <- linfo$cost
annotation.counts <- linfo$count

## active learning question: out of all the sampling strategies,
## should we pick profiles randomly, or can we calculate a statistic
## for every chrom that does better? in terms of training error? in
## terms of test error?
source("pick.best.index.R")

n.seeds <- 200
n.folds <- 10
set.seed(1)
folds <- sample(rep(1:n.folds,l=length(chrom.names)))

old.active <- function(test.fold,rank.chroms,max.train.chroms=20,seed=1){
  ## these are the names of the chroms in the training and test sets,
  ## note that these do NOT mean the same thing as in active.R.
  chrom.names.train <- chrom.names[folds != test.fold]
  chrom.names.test <- chrom.names[folds == test.fold]
  results <- list()
  test.annotations <- sum(annotation.counts[chrom.names.test])
  selected <- c()
  not.selected <- chrom.names.train
  train.errors <- rep(0,nrow(cost.mat))
  optimal.lambda <- pick.best.index(train.errors)
  set.seed(seed)
  while(length(selected) < max.train.chroms){
    ## choose the next chromosome to annotate via min score
    min.i <- which(min(train.errors) == train.errors)
    not.optimal <- min(train.errors) < train.errors
    left.optimal <- seq_along(train.errors) < optimal.lambda
    right.optimal <- optimal.lambda < seq_along(train.errors)
    scores <- rank.chroms(chrom.names=not.selected,
                          param.range=param.range,
                          train.errors=train.errors,
                          optimal.lambda=optimal.lambda,
                          left=max(which(not.optimal & left.optimal),1),
                          right=min(which(not.optimal & right.optimal),100))
    names(scores) <- not.selected
    best.scores <- which(scores == min(scores))
    to.sample <- not.selected[sample(best.scores,1)] ## RANDOM
    not.selected <- not.selected[not.selected != to.sample]
    selected <- c(selected,to.sample)

    ## calculate train error wrt annotations to pick best lambda
    cost.mat.train <- cost.mat[,selected,drop=FALSE]
    train.errors <- apply(cost.mat.train,1,sum)
    optimal.lambda <- pick.best.index(train.errors)

    ## calculate test error and save results
    test.err.mat <- cost.mat[optimal.lambda,chrom.names.test]
    test.errors <- sum(test.err.mat)
    i <- length(selected)
    cat(sprintf("test.fold: %d training.set.chroms: %s seed: %d\n",
                test.fold,i,seed))
    results[[i]] <- list(test.err.percent=test.errors/test.annotations * 100,
                         test.err.mat=test.err.mat,
                         test.errors=test.errors,
                         to.sample=to.sample,
                         scores=scores,
                         train.errors=train.errors,
                         optimal.lambda=optimal.lambda)
  }
  results
}
old.rankers <- list(random=function(chrom.names.test,...){
  rep(1,length(chrom.names.test))
},max.count=function(chrom.names.test,...){
  max.count[chrom.names.test]
},range=function(chrom.names.test,...){
  -param.range[chrom.names.test]
},range.max.count=function(chrom.names.test,train.errors,...){
  ranges <- param.range[chrom.names.test]
  largest <- ranges==max(ranges)
  counts <- max.count[chrom.names.test]
  counts[!largest] <- length(train.errors) ## these will not be picked
  counts
},range.near.min=function(chrom.names.test,...,left,right){
  sapply(chrom.names.test,function(N){
    -diff(range(linfo$optimal.seg.mat[left:right,N]))
  })
},weighted.breaks=function(chrom.names.test,train.errors,...){
  min.one <- train.errors-min(train.errors)+1
  w <- 1/min.one[-1]
  sapply(chrom.names.test,function(N){
    sum(w * kstar.diffs[,N])
  })
},near.edges=function(chrom.names.test,train.errors,...,
    left,right,optimal.lambda){
  if(train.errors[1]==0 || train.errors[length(train.errors)]==0){
    sapply(chrom.names.test,function(N){
      -diff(range(linfo$optimal.seg.mat[left:right,N]))
    })
  }else{
    indices <- unique(c((left):(left+5),(right-5):(right)))
    sapply(chrom.names.test,function(N){
      sum(kstar.diffs[,N][indices])
    })
  }
})
  
## check if the active learning statistic is really the same for the
## first round picks.
demo.errors <- rep(0,100)
first.round <- sapply(old.rankers,function(rank.fun){
  rank.fun(chrom.names,demo.errors,left=1,right=100,optimal.lambda=50)
})
range.methods <- c("near.edges","range","range.near.min","weighted.breaks")
scores <- first.round[,range.methods]
head(scores)
all.same <- apply(scores,1,function(x)all(x==x[1]))
all(apply(first.round[,range.methods],1,function(x)all(x==x[1])))
scores[!all.same,]
## run active learning strategy for 1 algo and 1 test fold
result <- old.active(1,old.rankers$range)

test.methods <- c("range","random")
old.method <- list()
for(method in test.methods){
  for(seed in 1:n.seeds){
    result <- old.active(1,old.rankers[[method]],seed=seed)
    test.err.percent <- sapply(result,"[[","test.err.percent")
    test.errors <- sapply(result,"[[","test.errors")
    key <- sprintf("%s.%d",method,seed)
    old.method[[key]] <- data.frame(test.errors,test.err.percent,method,seed,
                                    train.chroms=seq_along(test.err.percent))
  }
}
library(ggplot2)
old.df <- do.call(rbind,old.method)
## ggplot(old.df)+
##   geom_line(aes(train.chroms,test.err.percent,group=seed))+
##   facet_grid(method~.)
library(plyr)
old.stats <- ddply(old.df,.(train.chroms,method),summarize,
                   mean=mean(test.err.percent),
                   sd=sd(test.err.percent))
fold1 <- ggplot(old.stats)+
  geom_ribbon(aes(train.chroms,ymin=mean-sd,ymax=mean+sd,fill=method),
              alpha=1/2)+
  geom_line(aes(train.chroms,mean,colour=method),lwd=2)
print(fold1)
## conclusion: there is some differences in the code/data between here
## and 2012-05-22-acrive-folds.R, but there still seems to be a
## difference for the first few training examples.

source("active.rankers.R")
rankers$range <- rankers$k.range
new.method <- list()
for(method in test.methods){
  for(seed in 1:n.seeds){
    stopifnot(method%in%names(rankers))
    result <- active(linfo,folds,1,rankers[[method]],seed=seed)
    test.err.percent <- sapply(result,"[[","test.err.percent")
    test.errors <- sapply(result,"[[","test.errors")
    key <- sprintf("%s.%d",method,seed)
    new.method[[key]] <- data.frame(test.errors,test.err.percent,method,seed,
                                    train.chroms=seq_along(test.err.percent))
  }
}
new.df <- do.call(rbind,new.method)
new.stats <- ddply(new.df,.(train.chroms,method),summarize,
                   mean=mean(test.err.percent),
                   sd=sd(test.err.percent))
fold1.new <- ggplot(new.stats)+
  geom_ribbon(aes(train.chroms,ymin=mean-sd,ymax=mean+sd,fill=method),
              alpha=1/2)+
  geom_line(aes(train.chroms,mean,colour=method),lwd=2)
print(fold1.new)

## comparison
both.stats <- rbind(data.frame(old.stats,code="old"),
                    data.frame(new.stats,code="new"))
fold1.both <- ggplot(both.stats)+
  geom_ribbon(aes(train.chroms,ymin=mean-sd,ymax=mean+sd,fill=method),
              alpha=1/2)+
  geom_line(aes(train.chroms,mean,colour=method),lwd=2)+
  facet_grid(.~code)
print(fold1.both)

## compare on all folds
all.fold.list <- list()
for(method in test.methods){
  for(seed in 1:200){
    for(test.fold in 1:n.folds){
      results <-
        list(new=active(linfo,folds,test.fold,rankers[[method]],seed=seed),
             old=old.active(test.fold,old.rankers[[method]],seed=seed))
      for(code in names(results)){
        result <- results[[code]]
        test.errors <- sapply(result,"[[","test.errors")
        key <- paste(method,seed,test.fold,code,sep=".")
        all.fold.list[[key]] <-
          data.frame(test.errors,method,seed,test.fold,code,
                     train.chroms=seq_along(test.errors))
      }
    }
  }
}
total.annotations <- sum(annotation.counts)
all.fold.df <- do.call(rbind,all.fold.list)
sum.folds <- ddply(all.fold.df,.(train.chroms,seed,method,code),summarize,
      test.err.percent=sum(test.errors)/total.annotations*100)
all.fold.stats <- ddply(sum.folds,.(train.chroms,method,code),summarize,
                        mean=mean(test.err.percent),
                        sd=sd(test.err.percent))
all.fold.plot <- ggplot(all.fold.stats)+
  geom_ribbon(aes(train.chroms,ymin=mean-sd,ymax=mean+sd,fill=method),
              alpha=1/2)+
  geom_line(aes(train.chroms,mean,colour=method),lwd=2)+
  facet_grid(.~code)+
  ylab("Percent of incorrectly predicted test annotations")+
  xlab("Annotated signals in training set")
print(all.fold.plot)

pdf("figure-ireg-active-old-new.pdf")
print(all.fold.plot)
dev.off()
