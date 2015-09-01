## Plot results of doing CV with interval regression and various loss
## functions, and the usual cghseg.k constant model.

load("data/cv.results.RData")
library(plyr)
cv.results$test.error.percent <- with(cv.results,test.errors/test.anns*100)
cv.stats <- ddply(cv.results,.(model,train.size),function(d){
  result <- data.frame()
  for(stat in c("test.error.percent","seconds")){
    result <- rbind(result,{
      data.frame(mean=mean(d[,stat]),sd=sd(d[,stat]),stat)
    })
  }
  result
})

library(ggplot2)
max.folds <- max(cv.results$train.fold)
min.size <- min(cv.results$train.size)
max.size <- max(cv.results$train.size)
source("loss.colors.R")
p <- ggplot(cv.stats,aes(train.size,mean))+
  ##geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd,fill=model),alpha=1/2)+
  geom_line(aes(colour=model),lwd=2)+
  xlab("Number of annotated signals in the training set")+
  ylab("")+
  coord_cartesian(xlim=c(min.size,max.size+30))+
  scale_colour_manual(values=loss.colors)+
  facet_grid(stat~.,scales="free")+
  opts(title=paste("Learning error estimated using cross-validation",
             sprintf("averaged over %d folds",max.folds)))
library(directlabels)
dl <- direct.label(p,"last.qp")+guides(fill="none",colour="none")
pdf("figure-ireg-cv-results-all.pdf")
print(dl)
dev.off()
