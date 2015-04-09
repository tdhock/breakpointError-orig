load("data/variable.size.signals.RData")
sigs <- c("200 4", "800 1")
variable.size.show <-
  variable.size.signals[sigs]
save(variable.size.show,file="data/variable.size.show.RData")

