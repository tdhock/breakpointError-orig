load("data/variable.density.signals.RData")
a.grid <- c(0,1/2,1)
lambda <- variable.density.signals[[1]]$lambda
err.list <- lapply(a.grid,function(a){
  err.mat <- sapply(variable.density.signals,function(sig){
    sig$lambda.error(a)
  })## matrix[lambda,signal]
  error <- rowSums(err.mat)
  data.frame(alpha=a,error,lambda,optimal=error==min(error))
})
err.curves <- do.call(rbind,err.list)
dots <- subset(err.curves,optimal)
opt.err <- do.call(rbind,lapply(err.list,function(df)subset(df,optimal)[1,]))
library(ggplot2)
opt.err$text.at <- -8
opt.err$hjust <- 0
opt.err$vjust <- -0.5
on.right <- nrow(opt.err)
opt.err$text.at[on.right] <- 8
opt.err$hjust[on.right] <- 1
opt.err$vjust[on.right] <- -0.5#1.5
opt.color <- "red"
p <- ggplot(,aes(log10(lambda),error))+
  geom_line(lwd=1.1,data=err.curves)+
  ##geom_point(pch=1,size=4,data=dots)+
  geom_segment(aes(yend=error,xend=text.at),data=opt.err,
               colour=opt.color)+
  geom_text(aes(text.at,label=round(error,1),hjust=hjust,vjust=vjust),
            data=opt.err,colour=opt.color)+
  facet_grid(.~alpha,
             labeller=function(var,val)sprintf("$\\alpha = %s$",val))+
  xlab("model complexity tradeoff parameter $\\log_{10}(\\lambda)$")+
  ylab("error $E^\\alpha(\\lambda)$")
##pdf("figures/variable-density-error-train.pdf",h=2)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figures/variable-density-error-train.tex",h=2,w=6)
print(p)
dev.off()
