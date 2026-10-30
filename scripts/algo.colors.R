algo.colors <- c(cghseg.k="#F8766D",
                 flsa.norm="#619CFF",
                 flsa="#00BA38",
                 gada="#361eed",
                 dnacopy.alpha="#D39200",
                 dnacopy.prune="#00C19F",
                 dnacopy.sd="#DB72FB",
                 glad.haarseg="#93AA00",
                 glad.lambdabreak="#00B9E3",
                 glad.MinBkpWeight="#FF61C3",
                 pelt.default="#FFFFFF",
                 gada.default="#FFFFFF",
                 glad.default="#FFFFFF",
                 dnacopy.default="#FFFFFF",
                 cghseg.mBIC="#FFFFFF",
                 cghFLasso="#FFFFFF")
synonyms <- list(cghseg.k=c("pelt.n","cghseg.k, pelt.n"))
for(algo in names(synonyms)){
  for(new.algo in synonyms[[algo]]){
    algo.colors[[new.algo]] <- algo.colors[[algo]]
  }
}
