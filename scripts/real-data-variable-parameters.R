data(neuroblastoma,package="neuroblastoma")
profile.list <- split(neuroblastoma$pro,neuroblastoma$pro$profile.id)
chrom.list <- lapply(profile.list,function(df){
  split(df,df$chr)
})
chroms <- do.call(c,chrom.list)
chrom.df <- do.call(rbind,lapply(chroms,with,{
  data.frame(chr=chromosome[1],
             base.pairs=position[length(position)]-position[1],
             probes=length(chromosome))
}))
notY <- subset(chrom.df,chr!="Y")

## Signal size:
range(notY$base.pairs)/1e6

## Sampling density:
with(notY,range(base.pairs/probes))/1e3

## Scale:
s.est <- sapply(profile.list,function(df){
  df <- subset(df,chromosome!="Y")
  diffs <- lapply(split(df,df$chr,drop=TRUE),with,abs(diff(logratio)))
  median(do.call(c,diffs))
})
range(s.est)
