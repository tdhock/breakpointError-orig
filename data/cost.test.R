### save the agreement for each k so we can easily quantify the
### generalization ability of several model selection criteria.

load("data/segmentation.list.RData")
## there are 2 sets of annotations that are interesting.
data(neuroblastoma,package="neuroblastoma")
new.anns <- read.csv("annotations.csv")
names(new.anns)[names(new.anns)=="profile_id"] <- "profile.id"
annotation.sets <- list(original=neuroblastoma$annotations,
                        detailed=subset(new.anns,type=="breakpoints"))

## translate into standard notation, and filter out annotations for
## which we have no profiles
str(segmentation.list[[1]])
global.kmax <- max(sapply(segmentation.list,function(L)length(L$J.est)))
lapply(annotation.sets,nrow)
for(set in names(annotation.sets)){
  pid.chr <- with(annotation.sets[[set]],sprintf("%s.%s",profile.id,chromosome))
  have.profiles <- pid.chr %in% names(segmentation.list)
  if(any(!have.profiles)){
    cat(set,": removing annotations with no profiles:\n")
    print(annotation.sets[[set]][!have.profiles,])
    annotation.sets[[set]] <- annotation.sets[[set]][have.profiles,]
  }
  anns <- annotation.sets[[set]]$annotation
  annotation.sets[[set]]$annotation <- as.character(anns)
  annotation.sets[[set]]$annotation[anns=="breakpoint"] <- ">0breakpoints"
}
lapply(annotation.sets,function(df)table(df$ann))
lapply(annotation.sets,nrow)

## calculate if each annotation agrees with each model, returning
## list[[set]]list[[pid.chr]]matrix[annotation,k]
cost.matrices <- lapply(annotation.sets,function(all.annotations){
  anns.by.pid.chr <-
    split(all.annotations,
          with(all.annotations,list(profile.id,chromosome)),
          drop=TRUE)
  lapply(anns.by.pid.chr,function(annotations){
    cost.mat <- matrix(NA,nrow(annotations),global.kmax)
    for(ann.i in 1:nrow(annotations)){
      region <- annotations[ann.i,]
      pid <- as.character(region$profile.id)
      chromosome <- as.character(region$chromosome)
      pid.chr <- sprintf("%s.%s",pid,chromosome)
      results <- segmentation.list[[pid.chr]]
      if(is.null(results)){ ## profile data not available
        print(region)
        stop("profile data not available for ",pid.chr)
      }
      breakpoints.in.region <- sapply(results$breakpoints,function(positions){
        sum(region$min < positions & positions < region$max)
      })
      ann <- as.character(region$annotation)
      cost.mat[ann.i,] <-
        switch(ann,
               normal = ifelse(breakpoints.in.region==0,0,1),
               ">0breakpoints" =  ifelse(breakpoints.in.region==0,1,0),
               "1breakpoint" = ifelse(breakpoints.in.region==1,0,1),
               stop("cost undefined for ",ann))
      ## seems like this calculation is working.
      ##print(ann)
      ##print(cost.mat[ann.i,])
    }
    cost.mat
  })
})

save(cost.matrices,file="data/cost.matrices.RData")
