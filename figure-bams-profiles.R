works_with_R("2.15.1",grid="2.15.1",ggplot2="0.9.2.1")
clinical <- read.csv("data/clinical-limited.csv")
profiles <- read.csv("data/profiles.csv")
anns <- read.csv("data/anns.csv")
ann.map <- c(breakpoint=">0breakpoints",normal="0breakpoints")
anns$annotation <- ann.map[as.character(anns$annotation)]
source("scripts/geom_tallrect.R")
source("scripts/breakpoint.colors.R")
source("scripts/run.cghseg.R")
source("scripts/signal.colors.R")


str_match_perl <- function(string,pattern){
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- do.call(rbind,lapply(seq_along(string),function(i){
    st <- attr(parsed,"capture.start")[i,]
    if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
    substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
  }))
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}


lambda <- 10^c(breaks=-3,flat=-1.5,good=-2.2)

clinical$display.id <-
  sprintf("%d %s",clinical$profile.id,clinical$relapse)
clinical <- clinical[order(clinical$relapse,clinical$profile.id),]
clinical$display.id <- factor(clinical$display.id,clinical$display.id)

cvars <- clinical[,c("profile.id","relapse","display.id")]
merged <- merge(cvars,profiles)
amerge <- merge(cvars,anns)

toplot <- merged
toplot$chromosome <- factor(toplot$chromosome,c(1:22,"X"))
amerge$chromosome <- factor(amerge$chromosome,c(1:22,"X"))

signals <- split(profiles,list(profiles$pro,profiles$chr))
models <- lapply(signals,with,run.cghseg(logratio,position))

seg.df <- data.frame()
brk.df <- data.frame()
for(pid.chr in names(models)){
  info <-
    data.frame(str_match_perl(pid.chr,"(?<pid>[^.]+)[.](?<chromosome>.*)"))
  info$profile.id <- info$pid
  info <- merge(info,cvars)
  model <- models[[pid.chr]]
  J <- model$J.est
  Kseq <- seq_along(J)
  N <- nrow(signals[[pid.chr]])
  for(lname in names(lambda)){
    l <- lambda[[lname]]
    crit <- J/N + l*Kseq
    kstar <- which.min(crit)
    segs <- subset(model$segments,segments==kstar)
    brks <- subset(model$break.df,segments==kstar)
    seg.df <- rbind(seg.df,data.frame(info,lname,segs))
    if(kstar>1){ ## 1 segment means no breaks and data.frame concat err
      brk.df <- rbind(brk.df,data.frame(info,lname,brks))
    }
  }
}


makeplot <- function(a=NULL,suffix="noanns",lambda.name=NULL){
p <- ggplot()
if(!is.null(a)){
p <- p+geom_tallrect(aes(xmin=min/1e6,xmax=max/1e6,fill=annotation),data=amerge,
              alpha=a)
}
p <- p+
  scale_fill_manual(values=breakpoint.colors)+
  geom_point(aes(position/1e6,logratio),pch=1,colour="black",data=toplot)+
  scale_x_continuous("position on chromosome (mega base pairs)",
                     breaks=c(100,200))+
  ylab("logratio = approximate copy number")+
  facet_grid(display.id~chromosome,scales="free_x",space="free")+
  theme_bw()+
  theme(panel.margin=unit(0,"lines"))
if(!is.null(lambda.name)){
  segs <- subset(seg.df,lname==lambda.name)
  brks <- subset(brk.df,lname==lambda.name)
  suffix <- sprintf("%s-%s",suffix,lambda.name)
  p <- p+
    geom_segment(aes(first.base/1e6,mean,xend=last.base/1e6,yend=mean),
                 data=segs,lwd=1,colour=signal.colors[["estimate"]])+
    geom_vline(aes(xintercept=base/1e6),data=brks,lwd=1,
               colour=signal.colors[["estimate"]],linetype="dashed")
}
f <- sprintf("figure-bams-profiles-%s.png",suffix)
print(f)
##return(p)
png(f,2000,1600,res=200)
print(p)
dev.off()
}

makeplot()
makeplot(0,"transparent")
makeplot(1,"withann")

for(lname in names(lambda)){
  makeplot(1,"model",lname)
}
