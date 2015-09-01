library(lattice)
set.seed(1)
p <- xyplot(jitter(Petal.Length)~jitter(Sepal.Length),iris,groups=Species)
library(directlabels)
egrid <- function(step){
  force(step)
  function (d, ...) {
    d <- apply.method("big.boxes",d)
    NREP <- 10
    orig <- attr(d,"orig.data")
    all.points <- orig[, c("x", "y")]
    if (any(table(d$groups) > 1)) 
      d <- get.means(d)
    label.targets <- d
    ranges <- list(x = convertX(unit(c(0, 1), "npc"), "cm", valueOnly = TRUE), 
                   y = convertY(unit(c(0, 1), "npc"), "cm", valueOnly = TRUE))
    gl <- function(v) {
      s <- seq(min(all.points[, v]), max(all.points[, v]), 
               l = NREP)
      if (expand) {
        dif <- s[2] - s[1]
        s <- seq(min(ranges[[v]]) - expand * dif, max(ranges[[v]]) + 
                 expand * dif, l = NREP + 2 * expand)
      }
      list(centers = s, diff = s[2] - s[1])
    }
    hgrid <- function(x, w) {
      hboxes <- floor(diff(ranges[[x]])/r[, w])
      (-expand:(hboxes + expand - 1)) * r[, w] + r[, w]/2 + 
        min(ranges[[x]])
    }
    
    draw <- function(g) {
      gridlines <- with(g, list(x = unique(c(left, right)), 
                                y = unique(c(top, bottom))))
      drawlines <- function(a, b, c, d) {
        grid.segments(a, b, c, d, "cm", gp = gpar(col = "grey"))
      }
      with(gridlines, drawlines(min(x), y, max(x), y))
      with(gridlines, drawlines(x, min(y), x, max(y)))
    }
    res <- data.frame()
    label.targets <-
      label.targets[order(nchar(as.character(label.targets$groups))),]
    for (v in label.targets$groups) {
      r <- subset(label.targets, groups == v)
      no.points <- data.frame()
      expand <- 0
      while (nrow(no.points) == 0) {
        boxes <- if ("left" %in% names(label.targets)) {
          list(x = hgrid("x", "w"), y = hgrid("y", "h"), 
               w = r$w, h = r$h)
        }
        else {
          L <- sapply(c("x", "y"), gl, simplify = FALSE)
          list(x = L$x$centers, y = L$y$centers, w = L$x$diff, 
               h = L$y$diff)
        }
        boxes <- calc.borders(do.call(expand.grid, boxes))
        boxes <- cbind(boxes, data = inside(boxes, all.points))
        no.points <- transform(subset(boxes, data == 0))
        expand <- expand + 1
      }
      ## for every box, figure out the class of the points which is
      ## its nearest neighbor.
      no.points$nearest <- NA
      for(i in 1:nrow(no.points)){
        b <- no.points[i,]
        d.orig <- with(orig,(b$x-x)^2+(b$y-y)^2)
        no.points[i,"nearest"] <- as.character(orig$groups[which.min(d.orig)])
      }
      closest <- subset(no.points,nearest == rownames(r))
      if(nrow(closest) == 0){
        closest <- no.points
      }
      closest$len <- with(closest,(r$x-x)^2+(r$y-y)^2)
      best <- subset(closest, len == min(len))[1, ]
      res <- rbind(res, transform(r, x = best$x, y = best$y))
      newpts <- with(best, {
        expand.grid(x = seq(left, right, l = 3), y = seq(top, 
                                                   bottom, l = 3))
      })
      
      if (step == nrow(res)){
        draw(boxes)
        ##with(no.points,grid.points(x,y,1,default.units="cm"))

        ## show which classes are closest to each box.
        ##with(no.points,grid.text(no.points$nearest,x,y,default.units="cm"))
        ##with(closest,grid.points(x,y,20,default.units="cm"))

        ## draw black boxes around the boxes closest to this class.
        for(i in 1:nrow(closest)){
          with(closest[i,],{
            grid.polygon(c(left,right,right,left),
                         c(top,top,bottom,bottom),
                         default.units="cm",
                         gp=gpar(lwd=2))
          })
        }

        ## show points used for collision detection.
        with(all.points, grid.points(x, y, default.units = "cm"))

        last <- rownames(res)[step]
        with(label.targets[last,], {
        ##with(label.targets, {
          grid.points(x, y, 20, default.units = "cm",
                      gp = gpar(col = "green"))
        })
        return(res)
      }

      all.points <- rbind(all.points, newpts)
    }

    res
  }
}

##direct.label(p, egrid(2))

methods <- lapply(1:3,egrid)
names(methods) <- sprintf("$k=%d$",seq_along(methods))

library(tikzDevice);options(tikzDocumentDeclaration="\\documentclass[11pt]{memoir}",tikzMetricsDictionary="tikzMetrics")
options(tikzDocumentDeclaration=("\\documentclass[11pt]{article}"))
tikz("figure-iris-grid.tex",h=7,w=6)
dlcompare(list(p), methods, rects=FALSE, row.items="posfuns")
dev.off()
