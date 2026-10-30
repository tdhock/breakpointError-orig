load("data/overfit.df.RData")

library(plyr)
min.test.df <- ddply(subset(overfit.df,set=="test"),.(stat),function(test.df){
  test.df[which.min(test.df$value),]
})
save(min.test.df, file="data/min.test.df.RData")

