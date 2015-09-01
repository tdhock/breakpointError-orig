source("interval-regression.R")
phi.list$hinge <- function(x)ifelse(x<1,1-x,0)
deriv.list$hinge <- function(x)ifelse(x<1,-1,0)
x <- seq(-1,3,l=8*200+1)
loss.df.list <- lapply(names(phi.list),function(loss){
  phi <- phi.list[[loss]]
  phi.deriv <- deriv.list[[loss]]
  rbind(data.frame(loss,x,fun="loss",value=phi(x)),
        data.frame(loss,x,fun="subdifferential",value=phi.deriv(x)))
})
loss.df.list$zero.one <-
  data.frame(loss="zero-one",x,fun="loss",value=ifelse(x<0,1,0))
loss.df <- do.call(rbind,loss.df.list)
library(ggplot2)
p <- ggplot(loss.df,aes(x,value))+
  geom_line(aes(colour=loss,linetype=loss),lwd=1.1)+
  facet_grid(fun~.,scales="free",space="free")+
  scale_y_continuous(minor_breaks=NULL)+
  xlab("L")+
  opts(title="Losses for interval regression and their derivatives")
library(directlabels)
dl <- direct.label(p)+
  guides(linetype="none",colour="none")+
  coord_cartesian(xlim=c(min(x)-0.6,max(x)))
pdf("figure-ireg-phi-deriv.pdf",h=4)
print(dl)
dev.off()

