x <- c(-3,-2,0,3,5)
library(clusterpath)
df <- clusterpath.l1.id(x)
head(df)
mean(x)
plot(df)
## check agreement with cvx
cres <- cvxcheck(df,seq(0,max(df$lambda),l=8),verbose=TRUE)
xbar <- data.frame(alpha=mean(x),lambda=max(df$lambda))

library(ggplot2)
p <- ggplot(df,aes(lambda,alpha))+
  geom_line(aes(group=row))+
  geom_point(aes(y=alpha.1),data=cres,pch=21)+
  xlab("$\\lambda$")+ylab("$\\alpha$")+
  geom_text(data=xbar,label="$\\bar X$",hjust=-0.10)+
  xlim(0,5)+
  scale_y_continuous(breaks=x,minor=min(x):max(x))
print(p)

library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-clusterpath-l1path.tex",width=3.7,height=2)
print(p)
dev.off()
