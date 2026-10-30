run.cghseg <- structure(function
### Estimate the least squares model for a noisy signal. We use the
### cghseg package, which implements the pruned dynamic programming
### method of Rigaill (2010) to find, for all k=1,...,maxSegments:
### argmin_{x has k segments} ||Y-x||^2_2.
(Y,
### Numeric vector of the noisy signal to segment.
 base=seq_along(Y),
### Integer vector of bases where Y is sampled.
 maxSegments=20
### Maximum number of segments to consider.
 ){
  stopifnot(length(Y)==length(base))
  if(any(diff(base) < 0)){
    stop("need to sort signal so bases are increasing")
  }
  require(cghseg)
  n <- length(Y)
  kmax <- min(maxSegments,n)#k is the number of SEGMENTS not BREAKPOINTS
  result <- cghseg:::segmeanCO(Y,kmax)
  result$segments <- data.frame()
  result$breaks <- list()
  result$break.df <- data.frame()
  for(k in 1:kmax){
    ends <- result$t.est[k, 1:k]
    starts <- c(1,ends[-length(ends)]+1)
    signal <- rep(NA,k)
    for(seg.i in seq_along(starts)){
      start <- starts[seg.i]
      end <- ends[seg.i]
      signal[seg.i] <- mean(Y[start:end])
    }
    first.base <- base[starts]
    last.base <- base[ends]
    breaks <- floor(c(first.base[-1]+last.base[-k])/2)+1/2
    modelSegs <- data.frame(first.index=starts,last.index=ends,
                            first.base=c(first.base[1]-1/2,breaks),
                            last.base=c(breaks,last.base[k]+1/2),
                            mean=signal,segments=k)

    result$breaks[[k]] <- as.integer(breaks-1/2)

    if(k>1){
      result$break.df <- rbind(result$break.df,{
        data.frame(base=breaks,segments=k)
      })
    }

    result$segments <- rbind(result$segments,modelSegs)
  }
  result
### List containing the solutions. The "segments" element is a
### data.frame that describes the segmentation model, with 1 line for
### each segment.
},ex=function(){
  set.seed(1)
  y <- c(rnorm(50),rnorm(100,2))
  kmax <- 3
  result <- run.cghseg(y,maxSegments=kmax)
  signal.df <- data.frame(signal=y,base=seq_along(y))
  library(ggplot2)
  ggplot()+
    geom_point(aes(base,signal),data=signal.df)+
    geom_segment(aes(first.base,mean,xend=last.base,yend=mean),
                 data=result$segments,colour=signal.colors[["estimate"]],lwd=3)+
    geom_vline(aes(xintercept=base),data=result$break.df,
               colour=signal.colors[["estimate"]],linetype="dashed")+
    facet_grid(segments~.,labeller=function(var,val)sprintf("%s segments",val))
})
