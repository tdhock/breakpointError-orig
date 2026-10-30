fp.fn.colors <- c(false.positive="skyblue",
                  false.negative="#E41A1C",
                  errors="black",
                  imprecision="black")
fp.fn.linetypes <- c(errors="solid",
                     false.positive="solid",
                     false.negative="solid",
                     imprecision="dashed")
fp.fn.sizes <- c(errors=1,
                 false.positive=3,
                 false.negative=3,
                 imprecision=1)/1.2
synonyms <- list(false.negative=c("FN","false\nnegative"),
                 false.positive=c("FP","false\npositive"),
                 errors=c("error","cost"),
                 imprecision="I")
for(from in names(synonyms)){
  for(to in synonyms[[from]]){
    fp.fn.colors[to] <- fp.fn.colors[from]
    fp.fn.linetypes[to] <- fp.fn.linetypes[from]
    fp.fn.sizes[to] <- fp.fn.sizes[from]
  }
}
