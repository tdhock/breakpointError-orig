load("data/variable.breaks.constant.size.RData")
variable.breaks.constant.size.show <-
  variable.breaks.constant.size[c(1,length(variable.breaks.constant.size))]
save(variable.breaks.constant.size.show,
     file="data/variable.breaks.constant.size.show.RData")
