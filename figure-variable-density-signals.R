load("data/variable.density.show.RData")
signal.df <- do.call(rbind,lapply(variable.density.show,function(sig){
  with(sig,data.frame(locations,signal,mu,bases.per.probe))
}))
library(ggplot2)
source("scripts/signal.colors.R")
p <- ggplot(signal.df,aes(locations,signal))+
  geom_point(pch=21)+
  geom_line(aes(y=mu),colour=signal.colors["latent"],lwd=2)+
  facet_grid(bases.per.probe~.,
             labeller=function(var,val)sprintf("bases/probe = %s",val))+
  xlab("position in base pairs")+
  ylab("simulated copy number signal")+
  theme_grey()

png("figure-variable-density-signals.png",res=280,w=2000,h=960)
print(p)
dev.off()
##system(paste("display",f))
