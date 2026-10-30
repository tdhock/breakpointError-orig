source("scripts/algo.colors.R")
latex.colors <-
  sprintf("\\definecolor{%s.color}{HTML}{%s}",
          names(algo.colors),
          sub("#","",algo.colors))
writeLines(latex.colors,"scripts/algo.colors.tex")
