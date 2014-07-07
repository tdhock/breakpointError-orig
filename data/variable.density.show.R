## download regions we have annotated by eye.
regions <- read.csv("data/variable-density-signals-regions.csv")
load("data/variable.density.signals.RData")

region.list <- split(regions,regions$chromosome)

to.use <- c(2, 6)
variable.density.show <- list()
for(i in seq_along(to.use)){
  list.index <- to.use[i]
  info <- variable.density.signals[[list.index]]

  region.id <- as.character(list.index)
  info$regions <- region.list[[region.id]]
  
  variable.density.show[[i]] <- info
}
save(variable.density.show,file="data/variable.density.show.RData")

