## Show the annotations for 2 samples from the same signal.

load("data/variable.density.show.RData")
source("scripts/geom_tallrect.R")
source("scripts/breakpoint.colors.R")
source("scripts/signal.colors.R")

offset.by <- 100
## show the 2 signals.
points.df <- do.call(rbind,lapply(variable.density.show,with,{
  data.frame(bases.per.probe,
             signal=signal+offset.by,
             mu=mu+offset.by,
             locations,
             what="signal")
}))
regions.df <- do.call(rbind,lapply(variable.density.show,with,{
  d <- data.frame(regions[,c("min","max","annotation")])
  d <- d[order(d$min),]
  neg <- with(d,{
    data.frame(min=c(1,max+1),
               max=c(min-1,locations[length(locations)]),
               annotation="0breakpoints")
  })
            
  res <- rbind(data.frame(d,what="signal"),
               ##data.frame(d,what="augmented"),
               data.frame(neg,what="negative"))
  data.frame(res,bases.per.probe)
}))
  

## show the 2 signals.
cost.df <- do.call(rbind,lapply(variable.density.show,with,{
  last <- locations[length(locations)]
  B <- mu.break.after
  R <- rep(last-1,length(B))
  R[-length(R)] <- floor((B[-1]+B[-length(B)])/2)
  L <- rep(1,length(B))
  L[-1] <- R[-1]+1
  df <- function(base,cost)data.frame(base,cost)
  dfs <-
    list(breakpoint=rbind(df(L,1),df(B,0),df(R,1)),
         annotation=with(regions,{
           rbind(df(min,0),df(max,0),df(min-1,1),df(max+1,1))
         }))
  ##err <- err[order(err$base),]
  inf.df <- data.frame(base=c(-Inf,Inf),cost=1)
  for(i in seq_along(dfs)){
    d <- rbind(inf.df,dfs[[i]])
    dfs[[i]] <- data.frame(d,error=names(dfs)[i],
                           bases.per.probe,what="cost")
  }
  do.call(rbind,dfs)
}))

error.colors <- c(breakpoint=signal.colors[["latent"]],
                  annotation=breakpoint.colors[["1breakpoint"]])
library(ggplot2)
library(grid)
p <- ggplot()+
  geom_tallrect(aes(xmin=min,xmax=max,fill=annotation),
                data=regions.df)+
  geom_point(aes(locations,signal),pch=21,data=points.df)+
  geom_line(aes(locations,mu),lwd=2,
            colour=signal.colors["latent"],data=points.df)+
  geom_line(aes(base,cost,group=error,colour=error,size=error),
            data=cost.df)+
  facet_grid(what~bases.per.probe,scales="free",
             labeller=function(var,val){
               if(var=="bases.per.probe"){
                 sprintf("bases/probe = %s",val)
               }else{
                 as.character(val)
               }
               })+
  theme_bw()+
  theme(panel.margin=unit(0,"lines"))+
  xlab("position in base pairs")+
  scale_colour_manual("imprecision",values=error.colors)+
  scale_fill_manual(values=breakpoint.colors)+
  scale_size_manual("imprecision",values=c(breakpoint=2,annotation=1)/3)+
  scale_x_continuous(breaks=NULL,minor_breaks=NULL)+
  scale_y_continuous("",breaks=c(0,1),minor_breaks=NULL)+
  ## this is a new feature! to install, do
  ## install_github("gtable")
  ## install_github("ggplot2")
  guides(fill=guide_legend(order=1),
         colour=guide_legend(order=2),
         size=guide_legend(order=2))
         
png("figures/variable-density-annotation-cost.png",h=600,w=1200,res=200)
##library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
##tikz("figures/variable-density-annotation-cost.tex")
print(p)
dev.off()#;system("firefox figures/variable-density-annotation-cost.png&")
