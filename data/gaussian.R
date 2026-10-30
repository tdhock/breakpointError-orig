## the point of this is to show that kmeans fails for the grid of
## gaussian clusters, and the clusterpath and hcluster works ok
library(plyr)
library(directlabels)

centers <- data.frame(scale(expand.grid(x=1:5,y=1:5)))
centers$cluster <- factor(1:nrow(centers))
N <- 10
SD <- 0.05
set.seed(1)
d <- ddply(centers,.(cluster),with,data.frame(x=rnorm(N,x,SD),y=rnorm(N,y,SD)))
##xyplot(y~x,d,group=cluster,aspect="iso")
dmat <- as.matrix(d[,c("x","y")])

## k-means fails
clust <- kmeans(dmat,nlevels(centers$cluster))
d$kmeans <- clust$cluster
##direct.label(xyplot(y~x,d,group=kmeans,aspect="iso"))

## hclust works
tree <- hclust(dist(dmat))
d$hclust <- cutree(tree,nlevels(centers$cluster))
##direct.label(xyplot(y~x,d,group=hclust,aspect="iso"))

## l2path works
library(clusterpath)
res <- clusterpath.l2(dmat,gamma=10,lambda.factor=1.025,verbose=1,
                      join.thresh=0.01)
each.lambda <- split(res,res$lambda)
nclusters <- sapply(each.lambda,function(x)nrow(unique(x[,c("x","y")])))
tree <- do.call(rbind,each.lambda[1:which(nclusters==nrow(centers))[1]])
last <- subset(tree,lambda==max(lambda))
d$clusterpath <- assign.labels(last[,c("x","y")])
##xyplot(y~x,tree,group=row,type="l",aspect="iso")

#xyplot(y~x,res,group=row,type="l",aspect="iso")

save(d,res,tree,file="data/gaussian.RData")
