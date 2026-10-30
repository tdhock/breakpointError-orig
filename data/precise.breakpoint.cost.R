library(inline)

### Match guessed breaks to actual breaks, to calculate a cost that
### varies from 0 to the number of incorrect breaks.
precise.breakpoint.cost <- function(target,guess,size){
  one.at <- c(1,target[-1]-diff(target)/2,size)
  ##   zero.at <- target
  ##   fun.df <- rbind(data.frame(i=one.at,cost=1),
  ##                   data.frame(i=zero.at,cost=0))
  ##   fun <- with(fun.df,approxfun(i,cost))
  ##   curve(fun(x),1,d)
  distances <- data.frame()
  names(guess) <- seq_along(guess)
  names(target) <- seq_along(target)
  for(target.i in names(target)){
    if(length(guess)){
      distances <- rbind(distances,{
        data.frame(target.i,guess.i=names(guess),
                   dist=abs(target[target.i]-guess))
      })
    }
  }
  cost <- 0
  while(length(target) && length(guess)){
    closest <- which.min(distances$dist)
    target.i <- as.integer(distances$target.i[closest])
    guess.i <- distances$guess.i[closest]
    ones.at <- one.at[c(target.i,target.i+1)]
    zero.at <- target[as.character(target.i)]
    fun <- approxfun(c(ones.at,zero.at),c(1,1,0),rule=2)
    at <- guess[as.character(guess.i)]
    this.cost <- fun(at)
    ##curve(fun(x),1,d)
    ##points(at,this.cost)
    cost <- cost + this.cost
    ## remove stuff for next pass.
    not.chosen <-
      (distances$guess.i!=as.character(guess.i)) & (
         distances$target.i!=as.character(target.i))
    distances <- distances[not.chosen,]
    guess <- guess[names(guess)!=guess.i]
    target <- target[names(target)!=target.i]
  }
  cost + length(guess) + length(target)
}
    
bkpt.err.details <- function(breaks, guess, last){
  breakpointError::errorDetails(guess, breaks, last)
}
##  cfunction(signature(breaks="integer",guess="integer",last="integer"),'
bk <- c(1L,3L,6L)
g <- c(4L,7L,10L,15L)
res <- bkpt.err.details(bk,g,20L)
str(res)
with(res,cbind(left,breaks,right))

calcError <- function(res){
  with(res,sum(false.positive,false.negative,imprecision,guess.unidentified))
}
bkpt.err <- function(breaks,guess,d){
  calcError(bkpt.err.details(as.integer(breaks),as.integer(guess),as.integer(d)))
}

## check that guessing exact right results in zero error.
should.be.perfect <- list(bkpt.err.details(bk,bk,20L),
                          bkpt.err.details(integer(),integer(),20L))
for(L in should.be.perfect){
  err <- calcError(L)
  stopifnot(err == 0)
}

## check that guessing farther away is worse.
stopifnot(bkpt.err(5L,6L,11L) < bkpt.err(5L,7L,11L))

## check that false positives are more costly.
stopifnot(bkpt.err(integer(),integer(),11L) <
          bkpt.err(integer(),5L,11L))

## check that false negatives are more costly.
stopifnot(bkpt.err(5L,5L,11L) <
          bkpt.err(5L,integer(),11L))

## check empty breaks and guess.
checkEmpty <- function(L,should.be.empty){
  for(N in should.be.empty){
    stopifnot(length(L[[N]])==0)
  }
}
checkEmpty(bkpt.err.details(bk,integer(),20L),c("guess","guess.unidentified"))
checkEmpty(bkpt.err.details(integer(),bk,20L),
           c("breaks","false.positive","false.negative","imprecision",
             "left","right"))

checkError <- function(expr,should.be){
  msg <- tryCatch(expr,error=function(L)L[["message"]])
  stopifnot(is.character(msg))
  stopifnot(length(msg)==1)
  stopifnot(msg == should.be)
}
## these should be errors.
checkError(bkpt.err.details(0L,2L,10L),"break out of range")
checkError(bkpt.err.details(10L,20L,10L),"guess out of range")
checkError(bkpt.err.details(9L,20L,10L),"guess out of range")
checkError(bkpt.err.details(c(9L,9L),integer(),10L),"duplicate break")
checkError(bkpt.err.details(integer(),c(9L,9L),10L),"duplicate guess")
