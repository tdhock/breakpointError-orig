
annotation.error <- function(log.lambda,intervals){
  interval <- subset(intervals,min.L < log.lambda & log.lambda < max.L)
  stopifnot(nrow(interval) == 1)
  interval$cost
}
