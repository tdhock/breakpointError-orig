works_with_R("3.1.0",
             RColorBrewer="1.0.5",
             directlabels="2014.4.25",
             breakpointError="1.0",
             Segmentor3IsBack="1.8",
             "tdhock/ggplot2@98cefe4d653ce8f214177b66dc030c2f3c725ffb",
             changepoint="1.1.1")

params <-
  list(mean=data.frame(mean=c(-1, 1/2, -1/2),
         sd=c(1, 1, 1)),
       var=data.frame(mean=c(0, 0, 0),
         sd=c(1, 3, 1/2)))
segment.last <- c(300, 400, 500)
LAST.BASE <- as.integer(max(segment.last))
change.after <- segment.last[-length(segment.last)]
segment.first <- c(1, change.after+1)
segments <- NULL
signals <- NULL
changes <- data.frame(change.after, what="truth")
model.segments <- NULL
err.df <- NULL
model.changes <- NULL
set.seed(1)
for(change.param in names(params)){
  param.df <- params[[change.param]]
  facet <- if(change.param=="var"){
    "change in variance"
  }else{
    "change in mean"
  }
  these.segments <-
    data.frame(param.df, last=segment.last, first=segment.first,
               size=segment.last-segment.first+1, facet,
               change.param, what="truth")
  segments <- rbind(segments, these.segments)
  mean.vec <- NULL
  sd.vec <- NULL
  sampled.signal <- NULL
  for(segment.i in 1:nrow(these.segments)){
    seg <- these.segments[segment.i,]
    mean.vec <- c(mean.vec, rep(seg$mean, seg$size))
    sd.vec <- c(sd.vec, rep(seg$sd, seg$size))
    base <- with(seg, first:last)
    signal <- rnorm(seg$size, seg$mean, seg$sd)
    full.segment <- data.frame(base, signal, segment.i, change.param,
                               what="data", facet)
    is.sampled <- sample(c(TRUE, FALSE),
                         size=nrow(full.segment),
                         replace=TRUE,
                         prob=c(1, 4))
    sampled.segment <- full.segment[is.sampled, ]
    signals <- rbind(signals, sampled.segment)
    sampled.signal <- rbind(sampled.signal, sampled.segment)
  }
  max.segments <- 5
  for(model.type in c("mean", "var")){
    fun.name <- paste0("cpt.", model.type)
    fun <- get(fun.name)
    model.ints <- c(mean=2L, var=4L)
    sfit <- Segmentor(sampled.signal$signal,
                      model.ints[[model.type]], max.segments)
    fit <- fun(sampled.signal$signal,
               method="SegNeigh", Q=max.segments, class=FALSE)
    guesses <- list()
    log.lik <- rep(NA, max.segments)
    for(model.i in 1:max.segments){
      ## Check that I am interpreting the output of the changepoint
      ## package correctly. The change-points occur after the
      ## locations given in fit$cps.
      if(model.i == 2 && model.type == "mean"){
        change.indices <- 1:(nrow(sampled.signal)-1)
        rss <- rep(NA, nrow(sampled.signal))
        for(last.i in change.indices){
          est.mean <- rep(NA, nrow(sampled.signal))
          for(indices in list(1:last.i, (last.i+1):nrow(sampled.signal))){
            est.mean[indices] <- mean(sampled.signal$signal[indices])
          }
          residual <- est.mean-sampled.signal$signal
          rss[last.i] <- sum(residual * residual)
        }
        stopifnot(which.min(rss) == fit$cps[2,1])
        stopifnot(which.min(rss) == sfit@breaks[2,1])
      }
      point.mean <- point.sd <- rep(NA, nrow(sampled.signal))
      seg.mean <- seg.sd <- rep(NA, model.i)
      if(model.i == 1){
        first.i <- 1
        last.i <- nrow(sampled.signal)
        change.i <- change.base <- base.after <- base.before <- NULL
      }else{
        change.i <- sort(fit$cps[model.i, 1:(model.i-1)])
        stopifnot(change.i == sfit@breaks[model.i, 1:(model.i-1)])
        first.i <- c(1, change.i+1)
        last.i <- c(change.i, nrow(sampled.signal))
        base.after <- sampled.signal$base[change.i+1]
        base.before <- sampled.signal$base[change.i]
        change.base <- floor((base.after+base.before)/2)
        model.changes <- rbind(model.changes, {
          data.frame(change.i, change.base,
                     segments=model.i, model.type, change.param)
        })
      }
      first.base <- c(1, change.base+1)
      last.base <- c(change.base, LAST.BASE)
      guesses[[model.i]] <- as.integer(change.base)
      if(model.type == "var"){
        seg.mean <- point.mean <- mean(sampled.signal$signal)
      }
      for(seg.i in 1:model.i){
        seg.indices <- (first.i[seg.i]):(last.i[seg.i])
        if(model.type == "mean"){
          point.mean[seg.indices] <- seg.mean[seg.i] <-
            mean(sampled.signal$signal[seg.indices])
        }else{
          residual <- sampled.signal$signal[seg.indices] - seg.mean
          est.var <- mean(residual * residual)
          stopifnot(all.equal(est.var,
                              sfit@parameters[model.i, seg.i]))
          point.sd[seg.indices] <- seg.sd[seg.i] <- sqrt(est.var)
        }
      }
      if(model.type=="mean"){
        stopifnot(all.equal(as.numeric(sfit@parameters[model.i, 1:model.i]),
                            seg.mean))
        residual <- sampled.signal$signal-point.mean
        seg.sd <- point.sd <- sqrt(mean(residual * residual))
      }
      model.segments <- rbind(model.segments, {
        data.frame(first.i, last.i,
                   first.base, last.base,
                   segments=model.i,
                   seg.sd, seg.mean,
                   model.type, change.param)
      })
      log.lik[[model.i]] <-
        sum(dnorm(sampled.signal$signal, point.mean, point.sd, log=TRUE))
    }
    ## I guess the reason why my likelihood is not exactly equal to
    ## Segmentor likelihood is numerical issues...
    print(log.lik + sfit@likelihood)
    this.err <- errorComponents(guesses,
                                as.integer(change.after),
                                LAST.BASE)
    err.df <- rbind(err.df, data.frame(this.err, model.type, change.param))
  }
}

model.colors <- brewer.pal(3, "Dark2")
names(model.colors) <- c("truth", "mean", "var")

p <- ggplot(err.df, aes(segments,error))+
  geom_line(aes(size=type,colour=type,linetype=type))+
  scale_linetype_manual(values=fp.fn.linetypes)+
  scale_colour_manual(values=fp.fn.colors)+
  scale_size_manual(values=fp.fn.sizes)+
  facet_grid(model.type ~ change.param)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
dl <- direct.label(p+guides(linetype="none",colour="none",size="none"),
                   dl.combine("first.polygons","last.polygons"))

sig.model.breaks <- 
ggplot()+
  geom_point(aes(base, signal), data=signals, pch=1)+
  geom_vline(aes(xintercept=change.base+1/2,
               color=model.type, linetype=model.type),
           data=model.changes, show_guide=TRUE)+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  facet_grid(segments ~ change.param)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(sig.model.breaks)

sig.model.breaks <- 
ggplot()+
  geom_point(aes(base, signal), data=signals, pch=1)+
  geom_vline(aes(xintercept=change.base+1/2,
               color=model.type, linetype=model.type),
           data=model.changes, show_guide=TRUE)+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  facet_grid(segments ~ change.param)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(sig.model.breaks)

ffactor <- function(x)factor(x, c("truth", paste(2:9, "segments")))
matching <-
  list(segments=model.segments,
       changes=model.changes)
for(data.name in names(matching)){
  df <- subset(matching[[data.name]],
               change.param == model.type &
               segments > 1)
  df$facet <- ifelse(df$change.param=="mean",
                     "change in mean", "change in variance")
  df$facet2 <-
    ffactor(paste(df$segments, "segments"))
  matching[[data.name]] <- df
    
}

matchPlot <- 
ggplot()+
  geom_point(aes(base, signal), data=signals, pch=1)+
  geom_rect(aes(xmin=first.base-1/2, xmax=last.base+1/2,
                ymin=seg.mean-seg.sd, ymax=seg.mean+seg.sd,
                fill=model.type),
            data=matching$segments, alpha=3/10, color=NA)+
  geom_segment(aes(first.base-1/2, seg.mean, xend=last.base+1/2, yend=seg.mean,
                   color=model.type),
               data=matching$segments)+
  geom_vline(aes(xintercept=change.base+1/2,
               color=model.type, linetype=model.type),
           data=matching$changes, show_guide=TRUE)+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  facet_grid(segments ~ change.param)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(matchPlot)

alpha.rect <- 5/10
segments$facet2 <- ffactor("truth")
changes$facet2 <- ffactor("truth")
mod.ord <- c("truth", "mean", "var")
modelOnly <- 
ggplot()+
  geom_point(aes(base, signal), data=signals, pch=1)+
  geom_rect(aes(xmin=first.base-1/2, xmax=last.base+1/2,
                ymin=seg.mean-seg.sd, ymax=seg.mean+seg.sd,
                fill=model.type),
            data=matching$segments, alpha=alpha.rect, color=NA)+
  geom_segment(aes(first.base-1/2, seg.mean, xend=last.base+1/2, yend=seg.mean,
                   color=model.type),
               data=matching$segments)+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  facet_grid(facet2 ~ facet)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  geom_rect(aes(xmin=first-1/2, xmax=last+1/2,
                ymin=mean-sd, ymax=mean+sd,
                fill=what),
            data=segments, alpha=alpha.rect, color=NA)+
  geom_segment(aes(first-1/2, mean, xend=last+1/2, yend=mean,
                   color=what),
               data=segments)+
  scale_fill_manual(values=model.colors)+
  scale_color_manual(values=model.colors)
w <- 7
h <- 5
pdf("figure-motivation-modelOnly.pdf",w=w,h=h)
print(modelOnly)
dev.off()

vline.size <- 1
breaksOnly <- 
ggplot()+
  geom_point(aes(base, signal), data=signals, pch=1)+
  geom_vline(aes(xintercept=change.base+1/2,
               color=model.type),
           data=matching$changes, show_guide=TRUE, linetype="dashed")+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  facet_grid(facet2 ~ facet)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  geom_vline(aes(xintercept=change.after+1/2, color=what),
             data=changes, linetype="dashed")+
  scale_fill_manual(values=model.colors)+
  scale_color_manual(values=model.colors)
pdf("figure-motivation-breaksOnly.pdf",w=w,h=h)
print(breaksOnly)
dev.off()


two.sigs <- 
ggplot()+
  geom_point(aes(base, signal, color=what), data=signals, pch=1)+
  geom_vline(aes(xintercept=change.after+1/2, color=what),
             data=changes, lty="dashed")+
  scale_color_manual(values=c(data="black", model="green"))+
  scale_fill_manual(values=c(data="black", model="green"))+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  geom_rect(aes(xmin=first-1/2, xmax=last+1/2,
                ymin=mean-sd, ymax=mean+sd,
                fill=what),
            data=segments, alpha=2/10, color=NA)+
  geom_segment(aes(first-1/2, mean, xend=last+1/2, yend=mean, color=what),
               data=segments)+
  facet_grid(change.param ~ .)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  guides(fill="none")

## Also construct some breakpoints to illustrate differnt kinds of bad
## guesses.
guesses <-
  rbind(data.frame(base=c(400, 500, 750, 860), facet="false positive"),
        data.frame(base=c(400, 500), facet="false negative"),
        data.frame(base=c(370, 520, 745), facet="imprecise"))
guesses$what <- "estimate"
two.bad <- 
ggplot()+
  geom_point(aes(base, signal), data=signals, pch=1)+
  ## geom_vline(aes(xintercept=base+1/2, color=what),
  ##            data=guesses, lty="dashed", size=2, show_guide=TRUE)+
  geom_vline(aes(xintercept=change.after+1/2),
             data=changes, lty="dashed", color=model.colors[["truth"]])+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  geom_rect(aes(xmin=first-1/2, xmax=last+1/2,
                ymin=mean-sd, ymax=mean+sd),
            data=segments, alpha=2/10, color=NA, fill=model.colors[["truth"]])+
  geom_segment(aes(first-1/2, mean, xend=last+1/2, yend=mean),
               data=segments, color=model.colors[["truth"]])+
  facet_grid(facet ~ .)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  guides(fill="none")

pdf("figure-motivation.pdf",w=7,h=3.3)
print(two.bad)
dev.off()
