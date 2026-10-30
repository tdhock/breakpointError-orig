library(clusterpath)
library(multicore)
agreement.matrix <- function(l){
  1-dist(sapply(unique(l),function(x)l==x),"max")
}
calc.curves <- function(x,true.labels,kmax=12,gamma.vals=c(0.5,2,10)){
  names(gamma.vals) <- sprintf("clusterpath $\\gamma=%.1f$",gamma.vals)
  FUNS <- c(mclapply(gamma.vals,function(gamma){
    path <-
      clusterpath.l2(x,gamma=gamma,join.thresh=0.05,opt.thresh=0.1,
                             verbose=1,lambda.factor=1.01)
    stats <- calcstats(path)
    substats <- subset(stats,k<=kmax)
    function(kval){
      l <- subset(substats,k==kval)[1,"lambda"]
      if(is.na(l)){
        rep(NA,length(true.labels))
      }else{
        alpha <- subset(path,lambda==l)[,attr(path,"alphacolnames")]
        assign.labels(alpha)
      }
    }
  }),list("gaussian mixture"=function(k){
    require(mclust)
    Mclust(x,k)$classification
  },"average linkage"=function(k){
    tree <- hclust(dist(x),"average")
    cutree(tree,k)
  },kmeans=function(k){
    sapply(1:100,function(seed){
      set.seed(seed)
      kmeans(x,k,100)$cluster
    })
  }))
  agree <- agreement.matrix(true.labels)
  do.call(rbind,lapply(names(FUNS),function(method){
    FUN <- FUNS[[method]]
    do.call(rbind,lapply(1:kmax,function(k){
      res <- as.matrix(FUN(k))
      rand <- apply(res,2,function(klass){
        norm.rand.index(klass,true.labels)
      })
      agreement <- apply(res,2,function(klass){
        sum((agreement.matrix(klass)-agree)^2)
      })
      data.frame(method,k,
                 mean.rand=mean(rand),sd.rand=sd(rand),
                 mean.agree=mean(agreement),sd.agree=sd(agreement))
    }))
  }))
}
halfcircle <- function(r,center=c(0,0),class,sign,N=150,noise=0.5){
  angle <- runif(N,0,pi)
  rad <- rnorm(N,r,noise)
  data.frame(x=rad*cos(angle)+center[1],y=sign*rad*sin(angle)+center[2],
             class=factor(class))
}
set.seed(1)
pts <- rbind(halfcircle(4,c(0,0),1,1),
             halfcircle(4,c(4,2),2,-1))
print(xyplot(y~x,pts,groups=class,aspect="iso"))
m <- pts[,c("x","y")]
moons.results <- calc.curves(m,pts$class)

## iris
irisSc <- scale(iris[,-5])
iris.results <- calc.curves(irisSc,iris[,5])

moons.iris <- rbind(
                 data.frame(iris.results,data="iris"),
                 data.frame(moons.results,data="moons"))
save(moons.iris,file="data/moons.iris.RData")
