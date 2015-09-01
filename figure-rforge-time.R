pat <- paste("(?<date>[0-9]{4}-[0-9]{2}-[0-9]{2})",
             " ",
             "(?<time>[0-9]{2}:[0-9]{2})",
             sep="")
  str_match_perl <- function(string,pattern){
    parsed <- regexpr(pattern,string,perl=TRUE)
    captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
    captured.text[captured.text==""] <- NA
    captured.groups <- do.call(rbind,lapply(seq_along(string),function(i){
      st <- attr(parsed,"capture.start")[i,]
      if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
      substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
    }))
    result <- cbind(captured.text,captured.groups)
    colnames(result) <- c("",attr(parsed,"capture.names"))
    result
  }

getHTML <- function(proj,tmp="http://r-forge.r-project.org/projects/%s/"){
  u <- sprintf(tmp,proj)
  paste(readLines(url(u)),collapse="\n")
}
html <- sapply(c("inlinedocs","directlabels","clusterpath"),getHTML)
pat <- paste("(?<year>(\\d\\d)?\\d\\d)",
             "-",
             "(?<month>\\d\\d)",
             "-",
             "(?<day>\\d\\d)",
             sep="")
str_match_perl(html,pat)



if(!file.exists("data/project.stats.csv"))download.file("http://r-forge.r-project.org/scm/viewvc.php/*checkout*/tex/2011-06-09-ibl/project.stats.csv?root=inlinedocs","data/project.stats.csv")
sorted.projects <- read.csv("data/project.stats.csv",header=TRUE, 
                            colClasses=c("POSIXct","factor","integer"))
sorted.projects$count <- 1:nrow(sorted.projects)

##pdf("figure-rforge-time.pdf",h=3.5)
library(tikzDevice)#;options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
tikz("figure-rforge-time.tex",h=3,w=6)

par(mar=c(5,5,2,3))
plot(count~registered,sorted.projects,type="s",las=1,
     xlab="Date of project registration",
     ylab="Total number of projects\non R-Forge")
last.line <- tail(sorted.projects,1)
last.date <- last.line$registered
user2008 <- as.POSIXct("2008-08-13")
yhat <- with(sorted.projects,approx(registered,count,user2008))$y
points(user2008,yhat,pch=20)
text(user2008,yhat,"R-Forge announcement\nat useR 2008",adj=c(-0.1,1))
axis(3,last.date,format(last.date,"%e %B %Y"))
axis(4,last.line$count,las=1)

dev.off()
