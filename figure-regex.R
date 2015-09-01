## Example taken from PCRE docs.
date.pat <- paste('(?<date>',
                 '(?<year>(\\d\\d)?\\d\\d)',
                 '-',
                 '(?<month>\\d\\d)',
                 '-',
                 '(?<day>\\d\\d)',
                 ')',
                 sep="")
subject <- "Name: Maude, Born: 1983-03-17, Sex: F"
nchar(subject)
match <- gregexpr(date.pat,subject,perl=TRUE)[[1]]
groups <- data.frame(name=c("",attr(match,"capture.names")))
groups$number <- seq_along(groups$name)-1
groups$address <- groups$number*2+1/2
groups$number[1] <- NA
groups$y <- 1
n <- nrow(groups)
library(RColorBrewer)
data.colors <- brewer.pal(3,"Accent")

content <- c(match,match+attr(match,"match.length"))-1
g.start <- attr(match,"capture.start")
g.len <- attr(match,"capture.length")
for(i in seq_along(g.start)){
  content <- c(content,c(g.start[i],g.start[i]+g.len[i])-1)
}
content <- c(content,rep("??",n))
boxes <- data.frame(address=(1:(n*3))-1,
                    data=c(rep(c("start","end"),n),rep("workspace",n)),
                    content)
names(data.colors) <- unique(boxes$data)
withspace <- function(x,n)paste(x,paste(rep(" ",n),collapse=""))

##pdf("figure-regex.pdf",h=3.5,w=7)
library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-regex.tex",h=3.5,w=6)
layout(rbind(1,2),heights=c(1,2))
par(mar=c(3,0,0,0))
subject.i <- 1:nchar(subject)
plot(c(0,nchar(subject)),c(0,2),bty="n",ylab="",xaxt="n",yaxt="n",type="n",
     xlab="")
title(xlab=withspace("subject character offset",70),line=2)
axis(1,c(0,content,max(subject.i)-1),cex.axis=1)
to.label <- subset(boxes,data!="workspace")
to.label$content <- as.integer(as.character(to.label$content))
with(to.label,rect(content-1/2,5/4,content+1/2,2-1/8,
                   col=data.colors[as.character(data)]))
par(family="mono",font=2)
subject.chars <- strsplit(subject,split="")[[1]]
text(subject.i-1,1/2,subject.chars,adj=c(0.5,0))
par(family="sans",font=1)

par(mar=c(4,0,1,0))
plot(c(-1,n*4),c(-1,4),type="n",yaxt="n",ylab="",xaxt="n",xlab="")
title(xlab=withspace("memory address relative to ovector",50),line=2)
axis(1,c(0,2,4,6,8,10,12,17))
par(lwd=2)
note <- function(from,to,note){
  data.frame(from,to,note)
}
notes <- rbind(note(0,1,"entire pattern"),
               note(6,7,"un-named subpattern"),
               note(12,17,"workspace"))
leg <- data.frame(y=c(3,2,1),
                  label=c("notes","subpattern number","subpattern name"))
with(leg,text(18,y,label,adj=0))
with(notes,{
  segments(from-1/2,2.5,to+1/2,2.5)
  text((from+to)/2,3,note)
})
down <- -1
with(boxes,{
  rect(address-1/2,down,address+1/2,1/2,border="grey",
       col=data.colors[as.character(data)])
  text(address,-1/4,content)
})
with(groups,{
  rect(address-1,down,address+1,1/2)
  text(address,1,name,adj=c(0.5,0))
  text(address,2,number)
})
## the table is printed in the text but here is the data if we ever
## want to plot it.
##table.names <- sort(group.names[group.names!=""])
##table.indices <- sapply(table.names,function(x)which(group.names==x))-1

par(xpd=NA)
legend("bottomright",legend=names(data.colors),fill=data.colors,
       inset=c(0,-0.55),title="data type",bg="white")
dev.off()
##system("xpdf figure-regex.pdf")
