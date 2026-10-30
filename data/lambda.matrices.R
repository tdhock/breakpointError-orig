load("data/cost.matrices.RData")
load("data/segmentation.list.RData")

## cost.matrices is a list of matrix[annotation,k] and so the job of
## this script is to translate that into [pid.chr,lambda] for active
## learning.

lambda.grids <- list(web=10^seq(-4,0,l=100),
                     narrower=10^seq(-3,-1,l=100),
                     wider=10^seq(-5,1,l=100),
                     more=10^seq(-4,0,l=1000))

lambda.matrices <- list()

for(grid.name in names(lambda.grids)){
  for(set in names(cost.matrices)){
    mat.name <- sprintf("%s.%s",grid.name,set)
    print(mat.name)
    lambda.grid <- lambda.grids[[grid.name]]
    cost.by.pid.chr <- cost.matrices[[set]]
    optimal.seg.mat <- matrix(NA,length(lambda.grid),length(cost.by.pid.chr))
    colnames(optimal.seg.mat) <- names(cost.by.pid.chr)
    ann.lambda.cost <- list()
    for(pid.chr in names(cost.by.pid.chr)){
      cost.mat <- cost.by.pid.chr[[pid.chr]]
      segmentation <- segmentation.list[[pid.chr]]
      optimal.segments <- sapply(lambda.grid,function(lambda){
        which.min(with(segmentation,J.est/n+lambda*1:length(J.est)))
      })
      optimal.seg.mat[,pid.chr] <- optimal.segments
      ann.lambda.cost[[pid.chr]] <- cost.mat[,optimal.segments,drop=FALSE]
    }

    param.counts <- apply(optimal.seg.mat,2,table)

    anns.by.pid.chr <- sapply(ann.lambda.cost,nrow)
    lmat <- sapply(ann.lambda.cost,colSums)
    ## reality check:
    par(mfrow=c(1,1))
    plot(log10(lambda.grid),apply(lmat,1,sum),type="l")
    lambda.dynamic.range <- structure(function(k){
      if(all(k==k[1]))return(c(0,0))
      min.i <- which(k == min(k))
      max.i <- which(k == max(k))
      c(max(max.i),min(min.i))
    },ex=function(){
      stopifnot(lambda.dynamic.range(rep(1,100))==0)
      stopifnot(lambda.dynamic.range(c(rep(3,50),rep(2,50)))==1)
      stopifnot(lambda.dynamic.range(c(rep(20,10),rep(10,80),rep(1,10)))==81)
    })
    lambda.min.max <- t(apply(optimal.seg.mat,2,lambda.dynamic.range))
    lambda.center <- (lambda.min.max[,2]+lambda.min.max[,1])/2
    ## is this related to where the actual change in cost is?
##     change.center.df <-
##       do.call(rbind,lapply(names(lambda.center),function(pid.chr){
##         center <- lambda.center[pid.chr]
##         changes <- which(diff(lmat[,pid.chr]) != 0)
##         if( (length(changes)==0) || (center==0) )return(data.frame())
##         data.frame(pid.chr,center,changes,row.names=NULL)
##       }))
##     with(change.center.df,plot(jitter(center),jitter(changes)))
##     abline(0,1)
    n.breaks <- apply(lmat,2,function(x)sum(diff(x)!=0))
    lambda.range <- lambda.min.max[,2]-lambda.min.max[,1]
    k.range <- apply(optimal.seg.mat,2,function(x)diff(range(x)))
    plot(jitter(lambda.range),jitter(k.range))
    ## analyze just the dynamic range of lambda
    dynamic.lambdas <- lapply(1:nrow(lambda.min.max),function(i){
      l <- optimal.seg.mat[,i]
      mM <- lambda.min.max[i,]
      l[mM[1]:mM[2]]
    })
    names(dynamic.lambdas) <- rownames(lambda.min.max)
    plot(sapply(dynamic.lambdas,length),lambda.range)
    dynamic.tables <- lapply(dynamic.lambdas,table)
    dynamic.max.count <- sapply(dynamic.tables,function(count){
      if(length(count)==0)return(length(lambda.grid))
      max(count)
    })
    plot(jitter(dynamic.max.count),jitter(k.range))
    ## when does the first k=1 appear? early on => really flat curve,
    ## not interesting. late => many breakpoints.
    first.k1 <- apply(optimal.seg.mat,2,function(k)which(k==1)[1])
    first.k1[is.na(first.k1)] <- length(lambda.grid)
    plot(jitter(first.k1),jitter(k.range))
    kstar.diffs <- lapply(1:ncol(optimal.seg.mat),function(j){
      diff(optimal.seg.mat[,j])
    })
    names(kstar.diffs) <- colnames(optimal.seg.mat)
    result <- list(cost=lmat,
                   kstar.diffs=kstar.diffs,
                   count=anns.by.pid.chr,
                   dynamic.lambdas=dynamic.lambdas,
                   dynamic.tables=dynamic.tables,
                   lambda.min.max=lambda.min.max,
                   lambda.center=lambda.center,
                   first.k1=first.k1,
                   dynamic.max.count=dynamic.max.count,
                   lambda.grid=lambda.grid,
                   optimal.seg.mat=optimal.seg.mat,
                   max.count = sapply(param.counts,max),
                   k.range=k.range,
                   lambda.range=lambda.range)
    lambda.matrices[[mat.name]] <- result
  }
}
save(lambda.matrices,file="data/lambda.matrices.RData")
