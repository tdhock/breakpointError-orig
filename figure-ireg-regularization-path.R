## plot the coefficient regularization path.

load("data/overfitting.RData")
load("data/min.test.df.RData")

## cv selected coefs
cv.coefs <- overfitting$fit$coefs[,min.test.df[1,"j"]]
print(names(cv.coefs[cv.coefs != 0]))

## build df to plot reg path
vars.gammas <- overfitting$fit$coefs[-1,]
coef.df <- do.call(rbind,lapply(1:ncol(vars.gammas),function(j){
  data.frame(variable=rownames(vars.gammas),
             gamma=overfitting$fit$gamma.seq[j],
             coefficient=vars.gammas[,j])
}))
library(ggplot2)
p <- ggplot(coef.df,aes(-log10(gamma),coefficient,colour=variable))+
  geom_line()+
  geom_vline(aes(xintercept=-log10(gamma)),data=min.test.df)+
  opts(title=paste("L1-regularization path of coefficients"))
## install directlabels for nice lasso plot with variable names.
desc <- packageDescription("directlabels")
do.install <- if(!is.list(desc)){
  TRUE
}else{
  version <- desc[["Version"]]
  compareVersion(version,"2.7") < 0
}
if(do.install){
  install.packages("directlabels",repos="http://r-forge.r-project.org")
}
library(directlabels)
dl <- direct.label(p,"lasso.labels")
print(dl)

pdf("figure-ireg-regularization-path.pdf",h=14,w=14)
print(dl)
dev.off()
