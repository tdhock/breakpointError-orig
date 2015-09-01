load("data/optimization.results.RData")
library(ggplot2)
p <- ggplot(subset(optimization.results,step.size.type=="bb"),
            aes(iterate,log10(grad.l1norm)))+
  geom_line(aes(group=interaction(data.set,loss),colour=loss))+
  facet_grid(acceleration~problem.size)+
  coord_cartesian(xlim=c(0,100),ylim=c(-6,0))
print(p)
library(plyr)
it.max <- ddply(optimization.results,
                .(acceleration,problem.size,data.set,loss,step.size.type),
                summarize,iterations=max(iterate))
p <- ggplot(it.max)+
  geom_point(aes(log10(iterations),jitter(log10(problem.size)),
                 colour=acceleration),pch=1)+
  facet_grid(step.size.type~loss)+
  opts(title="acceleration does not help BB step")
## TODO: add second row of panels w/o BB step.
pdf("figure-ireg-optimization-results.pdf")
print(p)
dev.off()
