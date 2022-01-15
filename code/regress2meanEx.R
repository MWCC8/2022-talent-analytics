##### setup -----
library(tidyverse)
data_path = "~/Dropbox/McGill/teaching/2021-2022/2022_winter/ORGB671/data/"

##### functions -----
performance <- function(strategy=NULL){ 
  # strategy is A, B, or NULL (NULL is default)
  xbar <- 0
  if (!is.null(strategy)){
    xbar <- match(strategy,LETTERS[1:2])-1.5}
   # A reduces mean performance by 0.5 SD
   # B increases mean performance by 0.5 SD
  return(rnorm(n=1,mean=xbar))
}

performance()
performance("A")
performance("B")

indiv <- function(obs=1000,strategy=NULL){ # strategy is A, B, or NULL (NULL is default)
  prev <- performance()
  obsPerf <- rep(NA,obs)
  obsPerf[1] <- as.character(prev)
  nextObs <- NA
  for (i in 2:obs){
    if (is.null(strategy)) { 
      nextObs <- as.character(performance())}
    else if (strategy=="A" & prev <= -1.3){
      nextObs <- c(strategy,performance("A"))}
    else if (strategy=="B" & prev >= 1.3){
      nextObs <- c(strategy,performance("B"))}
    else {
      nextObs <- performance()
    }
    obsPerf[i] <- paste0(nextObs,collapse=",") 
    prev <- sum(as.numeric(nextObs),na.rm=T)
  }
  return(obsPerf)
}

##### simulate 3 individuals with different interventions -----
indiv0 <- indiv()
indivA <- indiv(strategy="A")
indivB <- indiv(strategy="B")

evalPerf <- function(perfVec){
  perfVecSplit <- as.character(
    unlist(
      sapply(perfVec,function(e){unlist(strsplit(e,split=","))})))
  if (length(grep("A",perfVec))>0){
    aIndices <- grep("A",perfVecSplit)
    perfChangeA <- sapply(aIndices,function(a){
      beforeA <- as.numeric(perfVecSplit[a-1])
      afterA <- as.numeric(perfVecSplit[a+1])
      return(afterA - beforeA)
    })
    print(paste0("Strategy A yielded an increase in performance ",
                 round(100*mean(as.numeric(perfChangeA > 0)),0),
                 " percent of the time, and a decrease in performance ",
                 round(100*mean(as.numeric(perfChangeA < 0)),0),
                 " percent of the time."))
  }
  if (length(grep("B",perfVec))>0){
    bIndices <- grep("B",perfVecSplit)
    perfChangeB <- sapply(bIndices,function(b){
      beforeB <- as.numeric(perfVecSplit[b-1])
      afterB <- as.numeric(perfVecSplit[b+1])
      return(afterB - beforeB)
    })
    print(paste0("Strategy B yielded an increase in performance ",
                 round(100*mean(as.numeric(perfChangeB > 0)),0),
                 " percent of the time, and a decrease in performance ",
                 round(100*mean(as.numeric(perfChangeB < 0)),0),
                 " percent of the time."))
  }
  xIndices <- sort(union(grep("A",perfVecSplit),
                         grep("B",perfVecSplit)))
  if (length(xIndices)>0) {
    oIndices <- (1:length(perfVecSplit))[-1*xIndices]
    perfChangeO <- diff(as.numeric(perfVecSplit[oIndices]))[-1*
                                                              sort(union(
                                                                grep("A",perfVec),
                                                                grep("B",perfVec)))]
  }
  else {
    oIndices <- (1:length(perfVecSplit))
    perfChangeO <- diff(as.numeric(perfVecSplit[oIndices]))
  }
  print(paste0("No intervention yielded an increase in performance ",
               round(100*mean(as.numeric(perfChangeO > 0)),0),
               " percent of the time, and a decrease in performance ",
               round(100*mean(as.numeric(perfChangeO < 0)),0),
               " percent of the time."))
}

evalPerf(indiv0)
evalPerf(indivA)
evalPerf(indivB)

##### write out data -----
perf_data <- tibble(indiv0,indivA,indivB)
write_csv(perf_data, paste0(data_path,"performance_data.csv"))
