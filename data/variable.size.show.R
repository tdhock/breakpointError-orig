load("data/variable.size.signals.RData")
sigs <- c("5 1", "8 10")
variable.size.show <-
  variable.size.signals[sigs]
save(variable.size.show,file="data/variable.size.show.RData")

