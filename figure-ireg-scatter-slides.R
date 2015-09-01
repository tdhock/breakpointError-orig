works_with_R("2.15.1",grid="2.15.1",ggplot2="0.9.2.1")
source("scripts/display.coefs.R")
source("scripts/interval-regression.R")
calcFit <- regression.funs$square
## the lambda matrices which quantify the annotation error for each
## signal.
for(item in c("L.min.max","signal.features")){
  if(!item%in%ls())load(sprintf("data/%s.RData",item))
}

limit.df <- do.call(rbind,lapply(names(L.min.max),function(ann.set){
  log.lambda.limits <- L.min.max[[ann.set]]

  do.call(rbind,lapply(c("min","max"),function(limit){
    colname <- sprintf("%s.L",limit)
    pid.chr <- rownames(log.lambda.limits)
    log.lambda <- log.lambda.limits[,colname]
    do.call(rbind,lapply(c("log.hall","log.n"),function(variable){
      data.frame(ann.set,pid.chr,limit,log.lambda,
                 feature=signal.features[pid.chr,variable],
                 variable)
    }))
  }))
}))

ann.set.names <- c("original","detailed.low.density")

toplot <- subset(limit.df,is.finite(log.lambda) &
       ann.set%in%ann.set.names)
latex <- c(log.n="log(number of points sampled)",
           log.hall="feature x = log(variance estimate)")
vars <- levels(toplot$variable)


for(v in vars){
  line.df <- data.frame()
  for(ann.set in ann.set.names){
    limits <- L.min.max[[ann.set]]
    x <- signal.features[rownames(limits),v,drop=FALSE]
    fit <- calcFit(x,limits)
    coef.df <- with(fit,display.coefs(intercept,weights,mu,sigma))
    line.df <- rbind(line.df,{
      data.frame(ann.set,slope=coef.df$coef[1],intercept=coef.df$coef[2])
    })
  }
  
  sub.df <- subset(toplot,variable==v)
p <- ggplot(sub.df)+
  geom_point(aes(log.lambda,feature,fill=limit),pch=21,size=2)+
  scale_fill_manual(values=c(min="white",max="black"))+
  facet_grid(.~ann.set,scales="free_y")+
  ylab(latex[[v]])+
  xlab("penalty exponent L")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))
  regline <- geom_abline(aes(slope=1/slope,intercept=-intercept/slope),
                         data=line.df,colour="green",lwd=2)
  plots <- list(noline=p,line=p+regline)

  for(N in names(plots)){
    f <- sprintf("figure-ireg-scatter-slide-%s-%s.png",sub("[.]","-",v),N)
    png(f,2400,1600,res=400)
    print(plots[[N]])
    dev.off()
  }
  
}

