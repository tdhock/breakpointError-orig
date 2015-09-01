f <- function(x){
  ifelse(x < -1, (x+1)^2,
         ifelse(x < 0, 0, x))
}
d <- function(x){
  ifelse(x < -1, 2*(x+1),
         ifelse(x < 0, 0, 1))
}
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-subdifferential.tex",h=4,w=6)

par(mfrow=c(2,1),xaxs="i",yaxs="r",las=1,mar=c(1,4,2,2))
curve(f(x),-3,3,lwd=2,xlab="$x$",ylab="function $f(x)$",ylim=c(-0.5,4),xaxt="n")
tangents <- data.frame(x=c(-2,0,0),
                       slope=c(-2,1/3,2/3),
                       intercept=c(-3,0,0))
for(i in 1:nrow(tangents)){
  with(tangents[i,],{
    abline(intercept,slope,col="red")
  })
}

par(mar=c(5,4,0,2),xpd=NA)
segs <- data.frame(x=c(-3,-1,0,0,3),
                   y=c(-4,0,0,1,1))
plot(segs,type="n",
     xlab="$x$",ylab="subdifferential $\\partial f(x)$")
par(xpd=FALSE)
abline(h=0,col="grey")
lines(segs,lwd=2)
with(tangents,{
  points(x,slope,col="red",pch=20)
})

dev.off()
