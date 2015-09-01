works_with_R("3.1.0", bams="1.0")

load("data/all.stats.RData")

model <- function(name,points,length,variance){
  data.frame(points,length,variance,row.names=name)
}
exponents <- rbind(
  model("cghseg.k",1,0,0),
  model("cghseg.k.var",1,0,2),
  model("cghseg.k.sqrt.d","$1/2$",0,0),
  model("cghseg.k.sqrt","$1/2$","$-1/2$",0),
  model("cghseg.k.sqrt.d.var","$1/2$",0,"2"),
  model("cghseg.k.sqrt.var","$1/2$","$-1/2$",2)
)

stopifnot(all(rownames(exponents)%in%names(all.stats)))


results <- lapply(all.stats[rownames(exponents)],estimate.test.error)

ann.counts <- with(all.stats$cghseg.k,normal.anns+breakpoint.anns)
arm.anns <- colSums(ann.counts)

getErr <- function(algo){
  glob.err <- apply(all.stats[[algo]]$errors,1,mean,na.rm=TRUE)
  train <- min(glob.err) * 100
  test <- results[[algo]]$global["errors",] * 100
  data.frame(train,test.mean=mean(test),test.sd=sd(test),row.names=algo)
}
err.df <- do.call(rbind,lapply(names(results),getErr))

out <- cbind(exponents,err.df)
library(xtable)
xt <- xtable(out,align="rrrrrrr")
print(xt,
      ##include.rownames=FALSE,
      floating=FALSE,
      sanitize.text.function=identity,
      file="table-penalty-real-data.tex")
