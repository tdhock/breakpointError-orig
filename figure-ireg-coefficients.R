load("data/model.comparison.RData")

coef.df <- data.frame()

for(ann.set in names(model.comparison$fit)){
  folds <- model.comparison$fit[[ann.set]]
  for(i in seq_along(folds)){
    fold <- folds[[i]]
    models <- names(fold)
    models <- models[models!="cghseg.k"]
    for(model in models){
      f <- fold[[model]]$fit
      nu <- with(f,intercept-sum(weights*mu/sigma))
      v <- with(f,weights/sigma)
      d <- rbind(data.frame(variable=names(v),coef=v),
                 data.frame(variable="intercept",coef=nu))
      both <- data.frame(d,fold=i,ann.set,model)
      coef.df <- rbind(coef.df,both)
    }
  }
}

library(plyr)
stats.df <- ddply(coef.df,.(model,ann.set,variable),summarize,
                  mean=mean(coef),
                  sd=sd(coef))
library(ggplot2)
### geoms useful for plotting these data using ggplot2
geom_hrange <- function
(mapping=NULL,
 data=NULL,
 stat="identity",
 position="identity",
 ...){
  require(ggplot2)
  require(proto)
  GeomHorizontalRange <- proto(ggplot2:::GeomLinerange,{
    required_aes <- c("xmin","y","xmax")
    draw <- function(., data, scales, coordinates, ...) {
      munched <- ggplot2:::coord_transform(coordinates, data, scales)
      ggname(.$my_name(),
             GeomSegment$draw(transform(data, yend=y, x=xmin, xend=xmax),
                              scales, coordinates, ...))
    }
  })
  GeomHorizontalRange$new(mapping = mapping, data = data, stat = stat,
                          position = position, ...)
}
plot.df <- subset(stats.df,ann.set!="detailed.high.density")
p <- ggplot(plot.df,aes(mean,ann.set))+
  geom_point()+
  geom_hrange(aes(xmin=mean-sd,xmax=mean+sd))+
  facet_grid(model~variable,scales="free")

pdf("figure-ireg-coefficients.pdf")
print(p)
dev.off()
