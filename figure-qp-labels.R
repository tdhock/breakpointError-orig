library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-qp-labels.tex",4,2)

par(mar=c(0,0,0,3),mfrow=c(1,2))
y <- c(1,2,5,6)
x <- rep(0,length(y))
h <- c(1.3,1.4,1,0.8)
xmin <- 0.15
xmax <- 0.35
left <- 0
xrange <- c(left,xmax)
plot(xrange,c(0,7),type="n")
bottom <- y-h/2
top <- y+h/2
rect(xmin,bottom,xmax,top,col="transparent")
i <- seq_along(y)
axis(4,y,sprintf("$t_{%s}$",i),las=2)
##axis(4,3.5,"$\\vdots$",las=2,tick=FALSE)
arrows.at <- c(0.2,0.3,0.2,0.2)
arrows(arrows.at,bottom,arrows.at,top,0.1,code=3)
text(0.23,y,sprintf("$h_{%s}$",i),adj=c(0,0.5))
##text(0.25,3.5,"$\\vdots$",adj=c(0,0.5))
set.seed(1)
left.y <- rnorm(length(y))+y
segments(left-0.1,left.y,xmin,y,lwd=3)

## plot 2: apply the QP solver.
plot(xrange,c(0,7),type="n",yaxt="n")
library(directlabels)
df <- data.frame(y,top,bottom)
solver <- qp.labels("y","bottom","top")
no.overlap <- transform(solver(df),top=y+h/2,bottom=y-h/2)
with(no.overlap,{
  rect(xmin,bottom,xmax,top,col="transparent")
  axis(4,y,sprintf("$y^*_{%s}$",i),las=2)
  arrows(arrows.at[1],bottom,arrows.at[1],top,0.1,code=3)
  text(0.23,y,sprintf("$h_{%s}$",i),adj=c(0,0.5))
})
segments(left-0.1,left.y,xmin,y,lwd=3)


dev.off()
