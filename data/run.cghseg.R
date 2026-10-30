run.cghseg <- function(Y,maxSegments=20){
  require(cghseg)
  n <- length(Y)
  kmax <- min(maxSegments,n)#k is the number of SEGMENTS not BREAKPOINTS
  result <- cghseg:::segmeanCO(Y,kmax)
  result$smooth <- matrix(NA,n,kmax)
  for(k in 1:kmax){
    ends <- result$t.est[k, 1:k]
    starts <- c(1,ends[-length(ends)]+1)
    for(seg.i in seq_along(starts)){
      start <- starts[seg.i]
      end <- ends[seg.i]
      result$smooth[start:end,k] <- mean(Y[start:end])
    }
  }
  result
}

