## plot the coefficient regularization path.

load("data/overfit.df.RData")
load("data/overfitting.RData")
source("scripts/display.coefs.R")
load("data/min.test.df.RData")

## cv selected coefs
cv.coefs <- overfitting$fit$coefs[,min.test.df[1,"j"]]
print(names(cv.coefs[cv.coefs != 0]))

## build df to plot reg path
f <- overfitting$fit
vars.gammas <- f$coefs[-1,]
intercepts <- f$coefs[1,]
coef.df <- do.call(rbind,lapply(1:ncol(vars.gammas),function(j){
  data.frame(display.coefs(intercepts[j],vars.gammas[,j],f$mu,f$sigma),
             gamma=overfitting$fit$gamma.seq[j])
}))
per <- "percent\nannotation error"
error.df <- with(overfit.df,{
  data.frame(variable=set,coef=value,gamma,
             what=ifelse(stat=="annotation.error.percent",
               per,"surrogate loss"))
})
lines.df <- rbind(error.df,
                  data.frame(coef.df,what="coefficients"))
lines.df$what <- factor(lines.df$what,
            c("surrogate loss",per,"coefficients"))
                  
pts.df <- data.frame(subset(coef.df,coef!=0),what="coefficients")
library(ggplot2)
blank <- with(error.df,{
  data.frame(gamma=min(gamma)/2.5,coef=c(max(coef)*1.05,0.28),
             what=c(per,"surrogate loss"))
})
library(grid)
p <- ggplot(,aes(-log10(gamma),coef))+
  geom_line(aes(colour=variable),data=lines.df)+
  geom_point(aes(colour=variable),data=pts.df)+
  geom_vline(aes(xintercept=-log10(gamma)),
             size=4,alpha=1/4,
             data=subset(min.test.df,stat=="annotation.error.percent"))+
  geom_blank(data=blank)+
  ylab("")+
  xlab("model complexity $-\\log_{10}(\\gamma)$")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  facet_grid(what~.,scales="free_y")
## install directlabels for nice lasso plot with variable names.
library(directlabels)

mylasso <- 
  list(cex=0.8,
       rot=0,
       gapply.fun({ ## figure out where the path hits 0
         d <- d[order(d$x),]
         zero <- d$y[1]
         i <- which(d$y!=zero)[1]
         just <- as.integer(d[i,"y"]>zero)
         transform(d[i-1,],hjust=0.5,#just,
                   vjust=just,y=y-(just-1/2)/5,
                   x=ifelse(groups=="log.hall",x+1.2,x))
       }),
       "calc.boxes",
       ## calculate how wide the tilted box is
       ##dl.trans(hyp=h/sin(2*pi*rot/360)),
       ##dl.trans(left=x-hyp/2,right=x+hyp/2),
       dl.trans(left=x-w/2,right=x+w/2),
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
         is.int <- solution$group=="intercept"
         solution[is.int,"x"] <- solution[is.int,"x"]+0.1
         solution
       })

dl <- direct.label(p,dl.combine("mylasso",
                                list("last.qp",dl.trans(x=x+0.1))))

##pdf("figure-ireg-overfitting-path.pdf",h=5.5)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-ireg-overfitting-path.tex",h=4.5,w=6)
print(dl)
dev.off()#;system("evince figure-ireg-overfitting-path.pdf")
