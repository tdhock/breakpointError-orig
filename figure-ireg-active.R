load("data/active.results.RData")
library(ggplot2)
levs <- levels(active.results$ranker.name)
not.random <- levs[levs!="random"]
random.df <- subset(active.results,ranker.name == "random")
not.random.df <- do.call(rbind,lapply(not.random,function(ranker.name){
  ranker.df <- active.results[active.results$ranker.name == ranker.name,]
  random.df$ranker.name <- ranker.name
  rbind(data.frame(random.df,method="random"),
        data.frame(ranker.df,method="ranker"))
}))
stats.plot <- ggplot(not.random.df)+
  geom_ribbon(aes(train.chroms,ymin=mean-sd,ymax=mean+sd,
                  group=method,fill=method),alpha=1/4)+
  geom_line(aes(train.chroms,mean,group=method,colour=method),
            lwd=1.5)+
  facet_grid(ranker.name~mat.name)
print(stats.plot)

zoomed <- stats.plot+coord_cartesian(ylim=c(4,20))
print(zoomed)


pdf("figure-ireg-active.pdf",7,9)
print(stats.plot)
dev.off()

pdf("figure-ireg-active-zoomed.pdf",7,9)
print(zoomed)
dev.off()
