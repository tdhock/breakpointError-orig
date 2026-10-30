new.anns <- read.csv("data/annotations.csv")

## figure out high density arrays for testing separately.
data(neuroblastoma,package="neuroblastoma")
profile.list <- split(neuroblastoma$profiles,neuroblastoma$profiles$profile.id)
profile.probes <- sapply(profile.list,nrow)
large <- profile.probes[profile.probes > 10000]
large.count <- table(large)
stopifnot( length(large.count) == 1 )

names(new.anns)[names(new.anns)=="profile_id"] <- "profile.id"
all.new <- subset(new.anns,type=="breakpoints")

## Here we define the sets of annotation data.
annotation.sets <-
  list(original=neuroblastoma$annotations,
       detailed.high.density=subset(all.new,profile.id%in%names(large)),
       detailed.low.density=subset(all.new,!profile.id%in%names(large)))
sim.ann.files <-
  c(complete="SimuProfilesTRUEFormattedAnnotation_3_n-125-.csv",
    trimmed="SimuProfilesTRUE_Trimmed_FormattedAnnotation_3_n-125-.csv",
    manual="SimuProfilesEDITEDAnnotation.csv")
## simulated data sets.
for(ann.name in names(sim.ann.files)){
  set.name <- sprintf("simulation.%s",ann.name)
  fn <- sim.ann.files[ann.name]
  if(!file.exists(fn)){
    u <- sprintf("http://cbio.ensmp.fr/~thocking/neuroblastoma/%s",fn)
    download.file(u,fn)
  }
  sim.anns <- read.csv(fn)
  sim.anns$annotation <-
    ifelse(sim.anns$breakpoints==0,"normal","1breakpoint")
  annotation.sets[[set.name]] <- sim.anns
}

target.names <- c("profile.id","chromosome","min","max","annotation")

for(set.name in names(annotation.sets)){
  annotation.sets[[set.name]] <- annotation.sets[[set.name]][,target.names]
}

check.anns <- function(df){
  str(df)
  stopifnot(is.data.frame(df))
  stopifnot(nrow(df) > 0)
  stopifnot(length(target.names) == length(names(df)))
  stopifnot(all(target.names == names(df)))
  TRUE
}

ok <- sapply(annotation.sets,check.anns)

save(annotation.sets,file="data/annotation.sets.RData")
