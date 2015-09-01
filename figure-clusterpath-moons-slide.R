## Figure 5. comparing methods. a lot of code for doing the
## clusterings is in clusterpath/R/compare.R

library(clusterpath)
load("data/moon.results.RData")

methods <- c("clusterpath","spectral kmeans",
             "average linkage","kmeans","gaussian mixture")
guessed.points <- subset(moon.results,seed==1 & method%in%methods)
guessed.points$guess <- factor(guessed.points$guess)
guessed.points$method <- factor(guessed.points$method,methods,
                                sub("old ","",methods))
m <- as.matrix(subset(guessed.points,method=="clusterpath",select=c(x,y)))
tree <- clusterpath.l2.general(m,check.splits=0,join.thresh=0.05,opt.thresh=0.1,
                       lambda.factor=1.05,gamma=2,verbose=1)
each.lambda <- split(tree,tree$lambda)
clusters <- sapply(each.lambda,function(x)nrow(unique(x[,c(1,2)])))
up2 <- data.frame(do.call(rbind,each.lambda[1:which(clusters==2)[1]]),
                  method="clusterpath")
p <- ggplot(guessed.points,aes(y,x))+
  geom_point(aes(shape=guess,colour=guess))+
  coord_equal()+
  facet_grid(.~method)+
  scale_colour_manual(values=c("blue","red"),guide="none")+
  scale_shape_manual(values=c(21,20),guide="none")+
  scale_x_continuous("",breaks=NULL)+
  scale_y_continuous("",breaks=NULL)+
  geom_path(aes(group=row),data=up2)
png("figure-clusterpath-moons-slide.png",h=3,w=7.8,units="in",res=600)
print(p)
dev.off()#;system(paste("display",f))
##maketikz(p,"moons",standAlone=TRUE,h=5,w=3)
