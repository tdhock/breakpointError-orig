load("data/lambda.matrices.RData")
library(reshape2)
img.df <- do.call(rbind,lapply(c("narrower","web","wider"),function(grid.name){
  mat.name <- sprintf("%s.original",grid.name)
  omat <- lambda.matrices[[mat.name]]$optimal.seg.mat
  molt <- melt(omat)
  names(molt) <- c("lambda","chr","k")
  data.frame(molt,grid.name)
}))
library(ggplot2)
img.df$limit <- ifelse(img.df$k %in% range(img.df$k),"limit","no")
small.df <- subset(img.df,as.integer(chr)<200)
lambda.ranges <- ggplot(small.df,aes(lambda,chr))+
  geom_tile(aes(fill=k))+
  geom_tile(data=subset(small.df,limit=="limit"),fill="red")+
  facet_wrap("grid.name")+
  scale_fill_gradient(low="white",high="black")
print(lambda.ranges)

## verify that cost curves for each lambda are calculated correctly.
pattern <- paste("(?<grid>[a-z]+)",
                 "[.]",
                 "(?<annotations>.*)",
                 sep="")
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
diag.df <- do.call(rbind,lapply(names(lambda.matrices),function(grid.anns){
  match <- str_match_perl(grid.anns,pattern)
  L <- lambda.matrices[[grid.anns]]
  data.frame(lambda=L$lambda.grid,
             cost=apply(L$cost,1,sum),
             grid.anns,
             grid=match[,"grid"],
             annotations=match[,"annotations"],
             row.names=NULL)
}))
library(ggplot2)
diagnostic <- ggplot(diag.df,aes(log10(lambda),log10(cost)))+
  geom_point(pch=1,size=5)+
  geom_line()+
  facet_grid(annotations~grid)+
  opts(title="training error curves for all parameter grids and annotations")
pdf("figure-ireg-lambda-diagnostics.pdf")
print(diagnostic)
dev.off()

