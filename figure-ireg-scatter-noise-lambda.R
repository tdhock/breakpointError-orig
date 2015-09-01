## the lambda matrices which quantify the annotation error for each
## signal.
for(item in c("L.min.max","signal.features")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

limit.df <- do.call(rbind,lapply(names(L.min.max),function(ann.set){
  log.lambda.limits <- L.min.max[[ann.set]]

  do.call(rbind,lapply(c("min","max"),function(limit){
    colname <- sprintf("%s.L",limit)
    pid.chr <- rownames(log.lambda.limits)
    log.lambda <- log.lambda.limits[,colname]
    do.call(rbind,lapply(c("log.hall","log.n"),function(variable){
      data.frame(ann.set,pid.chr,limit,log.lambda,
                 feature=signal.features[pid.chr,variable],
                 variable=ifelse(variable=="log.n","log.d",variable))
    }))
  }))
}))

source("scripts/left.right.colors.R")

library(ggplot2)
toplot <- subset(limit.df,is.finite(log.lambda) &
       ann.set%in%c("original","detailed.high.density","detailed.low.density"))
library(grid)
p <- ggplot(toplot)+
  geom_point(aes(log.lambda,feature,fill=limit),pch=21,size=2)+
  scale_fill_manual(values=left.right.colors)+
  facet_grid(variable~ann.set,scales="free_y")+
  ylab("")+
  xlab("penalty exponent L")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))
##print(p)

png("figure-ireg-scatter-noise-lambda.png",w=2000,h=2000,res=250)
print(p)
dev.off()##;system("display figure-scatter-noise-lambda.png")
