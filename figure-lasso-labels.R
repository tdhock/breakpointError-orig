data(prostate,package="ElemStatLearn")
pros <- subset(prostate,select=-train,train==TRUE)
ycol <- which(names(pros)=="lpsa")
x <- as.matrix(pros[-ycol])
y <- pros[[ycol]]
library(lars)
fit <- lars(x,y,type="lasso")
beta <- scale(coef(fit),FALSE,1/fit$normx)
arclength <- rowSums(abs(beta))
library(reshape2)
path <- data.frame(melt(beta),arclength)
names(path)[1:3] <- c("step","variable","standardized.coef")
library(ggplot2)
p <- ggplot(path,aes(arclength,standardized.coef,colour=variable))+
  geom_line(aes(group=variable))+
  xlim(0,20)
library(directlabels)

lasso.naive <- 
  list(rot=45,
       gapply.fun({ ## figure out where the path hits 0
         d <- d[order(d$x),]
         zero <- d$y[1]
         i <- which(d$y!=zero)[1]
         just <- as.integer(d[i,"y"]>zero)
         transform(d[i-1,],hjust=just,vjust=just)
       }))

lasso.labels <- c(lasso.naive,{
  list("calc.boxes",
       ## calculate how wide the tilted box is
       dl.trans(hyp=h/sin(2*pi*rot/360)),
       dl.trans(left=x-hyp/2,right=x+hyp/2),
       ## avoid collisions between tilted boxes
       function(d,...){
         solver <- qp.labels("x","left","right")
         ## apply the solver independently for top and bottom labels.
         solution <- data.frame()
         for(vj in c(0,1)){
           these <- d$vjust == vj
           if(any(these)){
             one.side <- d[these,]
             solved <- solver(one.side)
             solution <- rbind(solution,solved)
           }
         }
         solution
       })
})


move.right <- dl.trans(x=x+0.1)
reduce.cex <- dl.trans(cex=0.7)
methods <- list(Naïve=dl.combine(list(reduce.cex,lasso.naive),
                  list(reduce.cex,last.points,move.right)),
                QP=dl.combine(list(reduce.cex,lasso.labels),
                  list(reduce.cex, last.qp, move.right)))

##pdf("figure-lasso-labels.pdf",h=7,w=9)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-lasso-labels.tex",h=5,w=6)
dlcompare(list(p),methods,rects=FALSE,row.items="posfuns")
dev.off()
