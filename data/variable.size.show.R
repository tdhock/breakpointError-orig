load("data/variable.size.signals.RData")
variable.size.show <-
  variable.size.signals[c(1,length(variable.size.signals))]
save(variable.size.show,file="data/variable.size.show.RData")

