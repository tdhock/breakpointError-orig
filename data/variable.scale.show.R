load("data/variable.scale.signals.RData")
variable.scale.show <-
  variable.scale.signals[c(1,length(variable.scale.signals))]
save(variable.scale.show,file="data/variable.scale.show.RData")
