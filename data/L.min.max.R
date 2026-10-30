load("data/exact.cost.RData")

largest.run.min <- function
### Find the run of minimum cost with the largest size.
(cost,
 size
 ){
  m <- min(cost)
  is.min <- cost == m
  d <- c(diff(c(FALSE,is.min,FALSE)))
  ##print(data.frame(cost=c(cost,NA),size=c(size,NA),diff=d))
  starts <- which(d==1)
  ends <- which(d==-1)-1
  runs <- data.frame(starts,ends)
  ##print(runs)
  runs$size <- sapply(seq_along(starts),function(i){
    sum(size[ starts[i]:ends[i] ])
  })
  ##print(runs)
  largest <- which.max(runs$size)
  list(start=starts[largest],end=ends[largest])
}
intervals <- exact.cost$detailed.low.density[[1]]

## test intervals in the middle
intervals$cost <- c(2,2,1,1,1,0,0,0,1,1,1,0,0,0,1)
largest <- largest.run.min(intervals$cost,intervals$size)
stopifnot(largest$start == 12)
stopifnot(largest$end == 14)

## test beginning
intervals$cost <- c(0,0,0,0,0,0,0,0,1,1,1,1,0,1,1)
largest <- largest.run.min(intervals$cost,intervals$size)
stopifnot(largest$start == 1)
stopifnot(largest$end == 8)

## test end
intervals$cost <- c(1,1,0,0,1,1,1,0,0,0,0,0,0,0,0)
largest <- largest.run.min(intervals$cost,intervals$size)
stopifnot(largest$start == 8)
stopifnot(largest$end == 15)

L.min.max <- lapply(exact.cost,function(cost.list){
  do.call(rbind,lapply(names(cost.list),function(pid.chr){
    intervals <- cost.list[[pid.chr]]
    largest <- largest.run.min(intervals$cost,intervals$size)
    L <- cbind(min.L=intervals$min.L[ largest$start ],
               max.L=intervals$max.L[ largest$end ])
    rownames(L) <- pid.chr
    L
  }))
})

save(L.min.max,file="data/L.min.max.RData")
