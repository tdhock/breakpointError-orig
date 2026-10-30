if(getOption("repos")=="@CRAN@")options(repos=c("http://cran.r-project.org"))
if(!require(neuroblastoma)){
  install.packages("neuroblastoma")
  require(neuroblastoma)
}
data(neuroblastoma,package="neuroblastoma")
pros <- neuroblastoma$profiles
nb.list <- split(pros,list(pros$profile.id,pros$chromosome))

simulated <- read.csv("data/SimuProfiles.csv")
sim.list <- split(simulated,
                  list(simulated$profile.id,simulated$chromosome),
                  drop=TRUE)

signal.list <- c(nb.list,sim.list)

check.signal <- function(df){
  stopifnot(is.data.frame(df))
  target.names <- c("profile.id","chromosome","position","logratio")
  stopifnot(length(names(df)) == length(target.names))
  stopifnot(all(names(df) == target.names))
  stopifnot(nrow(df) > 0)
  TRUE
}

ok <- sapply(signal.list,check.signal)

save(signal.list,file="data/signal.list.RData")
