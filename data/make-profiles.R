works_with_R("2.15.1",bams="1.2",neuroblastoma="1.0")
if(getOption("repos")=="@CRAN@"){
  options(repos=c("http://mirror.ibcp.fr/pub/CRAN/",
            "http://cran.r-project.org"))
}
data(neuroblastoma)
clinical <- read.csv("data/clinical-limited.csv")
pids <- clinical$profile.id
pro <- subset(neuroblastoma$profiles,profile.id%in%pids & chromosome != "Y")
write.csv(pro,"data/profiles.csv",row.names=FALSE,quote=FALSE)
anns <- subset(neuroblastoma$annotations,profile.id%in%pids)
write.csv(anns,"data/anns.csv",row.names=FALSE,quote=FALSE)
