works_with_R("2.15.1",grid="2.15.1",bams="1.2",ggplot2="0.9.2.1")
load("data/annotation.sets.RData")
load("data/signal.list.RData")

pids <- c("4","8")

suffix <- c(original="systematic",detailed.low.density="any")

ann.map <- c(breakpoint=">0breakpoints",
             normal="0breakpoints",
             "1breakpoint"="1breakpoint",
             ">0breakpoints"=">0breakpoints")

ann.df <- data.frame()
for(set.name in names(suffix)){
  set <- annotation.sets[[set.name]]
  these <- subset(set,profile.id%in%pids)
  these$annotation <- ann.map[as.character(these$annotation)]
  these$set <- set.name
  ann.df <- rbind(ann.df,these)
}

sig.df <- do.call(rbind,lapply(pids,function(pid){
  pat <- sprintf("^%s[.]",pid)
  sig.dfs <- signal.list[grep(pat,names(signal.list))]
  do.call(rbind,lapply(sig.dfs,subset,chromosome%in%unique(ann.df$chr)))
}))

breakpoint.colors <-
  c(">0breakpoints"="#a445ee",
    "$>$0breakpoints"="#a445ee",
    "1breakpoint"="#ff7d7d",
    "breakpoint"="#a445ee", ## used for >0 bkpts in ch3.
    "0breakpoints"="#f6f4bf",
    normal="skyblue")
lfun <- function(var,val){
  if(var=="profile.id"){
    sprintf("profile %s",val)
  }else as.character(val)
}
for(set.name in unique(ann.df$set)){
  plot.anns <- subset(ann.df,set==set.name)
p <- ggplot()+
  geom_blank(aes(min/1e6),y=0,data=ann.df)+
  geom_blank(aes(max/1e6),y=0,data=ann.df)+
  geom_tallrect(aes(xmin=min/1e6,xmax=max/1e6,fill=annotation),data=plot.anns)+
  geom_point(aes(position/1e6,logratio),data=sig.df)+
  facet_grid(profile.id~chromosome,scales="free",space="free_x",
             labeller=lfun)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  scale_x_continuous("position on chromosome (mega base pairs)",
                     breaks=c(100,200))+
  scale_fill_manual(values=breakpoint.colors)+
  ylab("logratio = approximate copy number")
  f <- sprintf("figure-ireg-%s.png",suffix[[set.name]])
png(f,2800,1600,res=400)
  print(p)
  dev.off()
}
