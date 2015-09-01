load("data/variable.density.show.RData")

regions <- read.csv("data/variable-density-signals-regions.csv")


library(ggplot2)
library(proto)
geom_tallrect <- function
### ggplot2 geom with xmin and xmax aesthetics that covers the entire
### y range.
(mapping=NULL,
 data=NULL,
 stat="identity",
 position="identity",
 ...){
  GeomTallRect <- proto:::proto(ggplot2:::GeomRect,{
    required_aes <- c("xmin", "xmax")
    draw <- draw_groups <- function(.,data,scales,coordinates,
                                    ymin=0,ymax=1,...){
      ymin <- unit(ymin,"npc")
      ymax <- unit(ymax,"npc")
      with(ggplot2:::coord_transform(coordinates, data, scales),
           ggname(.$my_name(), {
        rectGrob(xmin, ymin, xmax - xmin, ymax-ymin,
                 default.units = "native", just = c("left", "bottom"), 
                 gp=gpar(
                   col=colour, fill=alpha(fill, alpha), 
                   lwd=size * .pt, lty=linetype, lineend="butt"
                   )
                 )
      }))
    }
  })
  GeomTallRect$new(mapping = mapping, data = data, stat = stat,
                   position = position, ...)
}

breakpoint.colors <-
  c(">0breakpoints"="#a445ee",
    "$>$0breakpoints"="#a445ee",
    "1breakpoint"="#ff7d7d",
    "breakpoint"="#a445ee", ## used for >0 bkpts in ch3.
    "0breakpoints"="#f6f4bf",
    normal="skyblue")



leg <- "bases/probe"

fp.fn.colors <- c(false.positive="skyblue",
                  false.negative="#E41A1C",
                  errors="black",
                  imprecision="black")
fp.fn.linetypes <- c(errors="solid",
                     false.positive="solid",
                     false.negative="solid",
                     imprecision="dashed")
fp.fn.sizes <- c(errors=1,
                 false.positive=3,
                 false.negative=3,
                 imprecision=1)/1.2
synonyms <- list(false.negative=c("FN","false\nnegative"),
                 false.positive=c("FP","false\npositive"),
                 errors=c("error","cost","E"),
                 imprecision="I")
for(from in names(synonyms)){
  for(to in synonyms[[from]]){
    fp.fn.colors[to] <- fp.fn.colors[from]
    fp.fn.linetypes[to] <- fp.fn.linetypes[from]
    fp.fn.sizes[to] <- fp.fn.sizes[from]
  }
}

library(ggplot2)
library(grid)
library(directlabels)

doPlot <- function(p,r){
  pushViewport(viewport(layout.pos.row=r))
  print(p,newpage=FALSE)
  popViewport()
}

for(sig.i in seq_along(variable.density.show)){

  sig <- variable.density.show[[sig.i]]
  sig$E <- sig$cost
  bases.per.probe <- factor(round(max(sig$locations)/length(sig$signal)))
  error.df <- do.call(rbind,lapply(c("FP","FN","I","E"),function(component){
    value <- sig[[component]]
    data.frame(bases.per.probe,segments=seq_along(value),value,component)
  }))


  positive <- subset(regions,chromosome==2,select=c(min,max,annotation))
  positive <- positive[order(positive$min),]
  last <- max(sig$locations)
  neg <- with(positive,{
    data.frame(min=c(1,max+1),
               max=c(min-1,last),
               annotation="0breakpoints")
  })
  all.anns <- rbind(positive,neg)
  ann.sets <- list(all=all.anns,
                   positive=positive,
                   half=positive[c(1,3,5),])
  positive.part <- function(x)ifelse(x>0,x,0)
  thresh <- function(x)ifelse(x==0,0,1)
  errDetails <- function(guess,anns,trans=identity){
    if(is.null(guess))guess <- integer()
    within(anns,{
      breaks <- sapply(seq_along(min),function(i){
        sum(min[i] < guess & guess < max[i])
      })
      FP <- trans(positive.part({
        ifelse(annotation=="1breakpoint",breaks-1,breaks)
      }))
      FN <- trans(positive.part({
        ifelse(annotation=="1breakpoint",1-breaks,0)
      }))
    })
  }
  lapply(sig$breaks,errDetails,all.anns)
  annErr <- function(guess,anns,trans=identity){
    df <- errDetails(guess,anns,trans)
    df$E <- df$FN + df$FP
    err.df <- data.frame()
    for(component in c("FP","FN","E")){
      err.df <- rbind(err.df,{
        data.frame(value=sum(df[,component]),component)
      })
    }
    err.df
  }
  errDF <- function(anns,trans=identity){
    do.call(rbind,lapply(seq_along(sig$breaks),function(segments){
      data.frame(annErr(sig$breaks[[segments]],anns,trans),segments)
    }))
  }
  

  signal.colors <- c(estimate="#0adb0a",
                     latent="#0098ef")

  plot2 <- function(fig.name,top,bottom=NULL){
    f <- sprintf("figure-variable-density-slides-sig%d-%s.png",sig.i,fig.name)
    png(f,2000,1600,res=400)

    grid.newpage()
    h <- unit(c(1,1),"null")
    pushViewport(viewport(layout=grid.layout(nrow=2,heights=h)))
    doPlot(top,1)
    if(!is.null(bottom)){
      doPlot(bottom,2)
    }

    dev.off()
  }

  get.df <- function(signal){
    logratio <- sig[[signal]]
    data.frame(position=sig$locations,logratio,
               signal,bases.per.probe)
  }

  signal.df <- get.df("signal")

  annPlot <- function(anns){
    x <- c("1breakpoint","0breakpoints")
    anns <- rbind(anns,data.frame(min=-2,max=-1,
       annotation=factor(x,x)))
    ggplot()+
      geom_tallrect(aes(xmin=min,xmax=max,fill=annotation),alpha=1/2,data=anns)+
      geom_point(aes(position,logratio),data=signal.df,pch=1)+
      scale_fill_manual(values=breakpoint.colors)+
      coord_cartesian(xlim=c(0,70000))+
      theme_bw()
  }

  blank <- ggplot(,aes(position,logratio))+
    geom_blank(data=signal.df)+
    scale_colour_manual(values=signal.colors)+
    theme_bw()

  sig$latent <- sig$mu
  sig$estimate <- sig$mu
  latent.df <- rbind(get.df("estimate"),
                     get.df("latent"))
  line.size <- 1
  latent <- geom_line(aes(colour=signal),data=latent.df,size=line.size)

  latent.breaks.df <-
    rbind(data.frame(position=sig$mu.break.after,signal="estimate"),
          data.frame(position=sig$mu.break.after,signal="latent"))
  latent.breaks <-
    geom_vline(aes(xintercept=position,colour=signal),
               size=line.size,linetype="dashed",data=latent.breaks.df)

  signal <- geom_point(data=signal.df,pch=1)

  details <- sig$cost.details[[1]]
  get.imp <- function(position,logratio){
    rbind(data.frame(position,logratio,signal="estimate"),
          data.frame(position,logratio,signal="latent"))
  }
  imp.df <- with(details,{
    rbind(get.imp(left,max(signal.df$logratio)),
          get.imp(right,max(signal.df$logratio)),
          get.imp(breaks,min(signal.df$logratio)))
  })
  imp <- geom_line(aes(colour=signal),data=imp.df,size=line.size)

  plot2("signals",blank+latent)

  ##plot2("points-ann",annPlot(all.anns))

  ##plot2("points-noann",annPlot(data.frame()))

  plot2("signals-breaks",blank+latent+latent.breaks)

  plot2("signals-points",blank+signal+latent+latent.breaks)

  plot2("points-breaks",blank+latent+signal+latent.breaks)

  plot2("points-breaks-imp",blank+signal+latent.breaks+imp)

  plot2("points-imp",blank+signal+imp)

    adj <- 0.05
  ## draw all the models with their breakpoint error.
  for(k in seq_along(sig$cost)){
    p.name <- sprintf("model-%s",k)

    sig$estimate <- sig$smooth[,k]
    est.df <- get.df("estimate")
    estimated.signal <-
      geom_line(aes(colour=signal),data=est.df,size=line.size)
    ## need to make a NA df, otherwise error.
    position <- if(k>1)sig$breaks[[k]] else as.integer(NA)
    est.breaks.df <- data.frame(position,signal="estimate")
    estimated.breaks <-
      geom_vline(aes(xintercept=position,colour=signal),
                 data=est.breaks.df,size=line.size,linetype="dashed")

    ## bottom plot of breakpoint error.
    errPlot <- function(this.df,k=NULL,what="annotation"){
    unlab <- ggplot(,aes(segments,value))
    if(!is.null(k)){
      unlab <- unlab+geom_vline(xintercept=k,lwd=5,colour="grey")
    }
    unlab <- unlab+
      geom_blank(data=error.df)+
      geom_line(aes(group=component,colour=component,
                    size=component,linetype=component),
                data=this.df)+
      scale_linetype_manual(values=fp.fn.linetypes)+
      scale_colour_manual(values=fp.fn.colors)+
      scale_size_manual(values=fp.fn.sizes)+
      theme_bw()+
      ylab(sprintf("%s error",what))+
      scale_x_continuous("segments in estimated model",
                         breaks=c(1,k,7,20),minor_breaks=NULL)

    mymethod <-
      dl.combine(list(dl.trans(x=x-adj),"first.qp",fontfamily="serif"),
                 list(dl.trans(x=x+adj),"last.qp",fontfamily="serif"))
    direct.label(unlab,mymethod)+
      guides(linetype="none",size="none")
    }

    bottom <- errPlot(error.df,k,"breakpoint")

    top <- blank+signal+imp+estimated.signal+estimated.breaks

    plot2(p.name,top,bottom)

    old <- c(1,5,6,7,8,13,20)
    old <- c() ## comment to make more plots.
    if(k %in% old && sig.i == 1){

      for(ann.set in names(ann.sets)){
        ann.plot.name <- sprintf("%s-%s",p.name,ann.set)
        ann.df <- ann.sets[[ann.set]]
        top <- annPlot(ann.df)+
      geom_vline(aes(xintercept=position),colour=signal.colors[["estimate"]],
                 data=est.breaks.df,size=line.size,linetype="dashed")+
      geom_line(aes(position,logratio),colour=signal.colors[["estimate"]],
                data=est.df,size=line.size)
        plot2(ann.plot.name,top,errPlot(errDF(ann.df),k,"breakpoint"))
        plot2(sprintf("%s-thresh",ann.plot.name),
              top,errPlot(errDF(ann.df,thresh),k,"annotation"))

      }
      
    }
  }
}


