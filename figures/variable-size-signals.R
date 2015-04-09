load("data/variable.size.show.RData")
signal.df <- do.call(rbind,lapply(variable.size.show, with, {
  data.frame(locations,signal,mu,length=size, data=length(mu))
}))
library(ggplot2)
source("scripts/signal.colors.R")
p <- ggplot(signal.df,aes(locations,signal))+
  geom_point(pch=21)+
  geom_line(aes(y=mu),colour=signal.colors["latent"],lwd=1.1)+
  facet_grid(length + data ~.,labeller=label_both)+
  xlab("position in base pairs")+
  theme_grey()
print(p)

png("figures/variable-size-signals.png",res=140,w=1000)
print(p)
dev.off()
