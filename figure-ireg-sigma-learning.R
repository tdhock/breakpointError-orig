## the lambda matrices which quantify the annotation error for each
## signal.
for(item in c("signal.list","L.min.max","signal.features")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

limit.df <- do.call(rbind,lapply(names(L.min.max),function(ann.set){
  log.lambda.limits <- L.min.max[[ann.set]]

  do.call(rbind,lapply(c("min","max"),function(limit){
    colname <- sprintf("%s.L",limit)
    pid.chr <- rownames(log.lambda.limits)
    log.lambda <- log.lambda.limits[,colname]
    do.call(rbind,lapply(c("log.hall","log.n","emilie"),function(variable){
      data.frame(ann.set,pid.chr,limit,log.lambda,
                 feature=signal.features[pid.chr,variable],
                 variable)
    }))
  }))
}))

source("left.right.colors.R")

library(ggplot2)
p <- ggplot(subset(limit.df,is.finite(log.lambda)))+
  geom_point(aes(log.lambda,feature,colour=limit))+
  scale_colour_manual(values=left.right.colors)+
  facet_grid(variable~ann.set,scales="free")+
  ##opts("Optimal degree of smoothness depends on signal noise")+
  ylab("log10(median(abs(diff(signal)))) naive noise estimate")+
  xlab("log(lambda) degree of smoothness")
pdf("figure-ireg-sigma-learning.pdf",w=10)
print(p)
dev.off()
