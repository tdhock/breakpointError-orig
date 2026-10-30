load("data/cost.matrices.RData")

## cost.matrices is a list of list of matrix[annotation,k] and so the
## job of this script is to translate that into a data.frame for each
## interval in the k/lambda error curve space.
load("data/exact.breakpoints.RData")

bkpts <- exact.breakpoints[[1]]
cost <- cost.matrices$detailed[[1]]
exact.cost <- lapply(cost.matrices,function(cost.list){
  new.list <- lapply(names(cost.list),function(pid.chr){
    bkpts <- exact.breakpoints[[pid.chr]]
    cost <- cost.list[[pid.chr]]
    exact.cost <- cost[,bkpts$K,drop=FALSE]
    intervals <- with(bkpts,{
      data.frame(min.lambda=lambda,
                 max.lambda=c(lambda[-1],Inf),
                 segments=K,
                 cost=colSums(exact.cost))
    })
    within(intervals,{
      max.L <- log(max.lambda)
      min.L <- log(min.lambda)
      size <- max.L-min.L
    })
  })
  names(new.list) <- names(cost.list)
  new.list
})
save(exact.cost,file="data/exact.cost.RData")
