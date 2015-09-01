load("data/sim.cvx.RData")
means <- data.frame(alpha=t(colMeans(sim$mat)))
library(ggplot2)
library(grid)
normweights <- function(var,val){
  val <- as.character(val)
  if(var=="gamma")var <- "\\textrm{weights } \\gamma"
  else var <- sprintf("\\textrm{%s}",var)
  val[val=="inf"] <- "\\infty"
  sprintf("$%s=%s$",var,val)
}
plot.one <- function(S){
  path <- data.frame(subset(cvx,s>=S),points="$\\alpha$")
  alpha <- subset(path,s==min(s))
  xx <- alpha
  xx[,1] <- sim$mat[,1]
  xx[,2] <- sim$mat[,2]
  xx$points <- "$X$"
  xalpha <- rbind(alpha,xx)
ggplot(path,aes(alpha.2,alpha.1))+
  geom_path(aes(group=row),lwd=1)+
  geom_point(data=means,label="$\\bar X$",col="grey")+
  facet_grid(gamma~norm,labeller=normweights)+
  coord_equal()+
  geom_point(data=xx,fill="white",pch=21)+
  scale_x_continuous("",breaks=-10)+
  scale_y_continuous("",breaks=-10)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))
}
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
library(directlabels)

slides <- ""
for(S in seq(0,1,by=0.1)){
  fn <- sprintf("figure-clusterpath-normweights-%s.tex",
                sub("[.]","-",S))
  info <- list(lim=as.character(S),tex=fn)
  tex <- filltemplate(info,"figure-clusterpath-normweights-template.tex")
  slides <- paste(slides,tex)

  tikz(fn,width=4,height=3.8)
  print(plot.one(S))
  dev.off()
}

writeLines(slides,"figure-clusterpath-normweights.tex")
