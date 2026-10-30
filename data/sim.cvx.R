set.seed(19)
library(clusterpath)
sim <- gendata(N=5,D=2,K=2,SD=0.3)
plot(sim$mat,asp=1)
cvx <- data.frame()
for(norm in c(1,2,"inf"))for(gamma in c(0,1)){
  cvx <- rbind(cvx,cvxmod.cluster(sim$mat,norm=norm,gamma=gamma,
                                  regularization.points=50))
}
save(sim,cvx,file="data/sim.cvx.RData")
