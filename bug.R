works_with_R("3.1.0", changepoint="1.1.2")

cpt.meanvar(c(1, -1, 1))

cpt.mean(c(1), method="SegNeigh", Q=2, class=FALSE)
cpt.mean(c(1, -1), method="SegNeigh", Q=2, class=FALSE)
cpt.mean(c(1, -1, -1.1), method="SegNeigh", Q=3, class=FALSE)

cpt.mean(c(1, -1), method="PELT", class=FALSE)
cpt.mean(c(1, 1.1, 1.01, -1, -1.01, -1.3), method="PELT", class=FALSE)
fit <- cpt.mean(c(1, 1.1, 1.01, -1, -1.01, -1.3), method="PELT")
cpts(fit)
cpt.var(c(-1, 1, -1, 10, -10, 10), method="PELT", class=FALSE)
cpt.var(c(-1, 1, -10, 10), method="PELT", class=FALSE)
cpt.var(c(-1, -10, 10), method="PELT", class=FALSE)

two.segments <- function(x){
  mu <- mean(x)
  possible.changes <- 1:(length(x)-1)
  model.info <- NULL
  for(change.after in possible.changes){
    first <- c(1, change.after+1)
    last <- c(change.after, length(x))
    log.lik <- rep(NA, length(x))
    for(segment.i in seq_along(first)){
      seg.first <- first[segment.i]
      seg.last <- last[segment.i]
      seg.indices <- seg.first:seg.last
      seg.x <- x[seg.indices]
      residual <- seg.x-mu
      var.est <- mean(residual * residual)
      log.lik[seg.indices] <- dnorm(seg.x, mu, var.est, log=TRUE)
    }
    model.info <- rbind(model.info, {
      data.frame(change.after, log.lik=sum(log.lik))
    })
  }
  list(possible.models=model.info,
       most.likely.change.after=which.max(model.info$log.lik))
}
two.segments(c(-1, -10, 10))
two.segments(c(-1, 1, 10))
two.segments(c(1, -1, -10, 10))

