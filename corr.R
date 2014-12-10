corr <- function(directory="specdata", threshold=0){
  ## Return numeric vector of correlations  
  
  ## 'directory' is where to find the data, 'threshold' is min # of pairs to accept for cor

  corVec <- numeric(0)
  obsCnt <- numeric(0)
  x <- 1
  while (x < 333) {  
    ##  get the file name  (there's probably a better way to do this)
    if (x < 10) {
      fileName <- paste(directory,"/","00",x,".csv",sep="") 
      } else { 
      if (x < 100) {
        fileName <- paste(directory,"/","0",x,".csv",sep="")
      } else {
        fileName <- paste(directory,"/",x,".csv",sep="")
      }}
    
    ##  read the csv file
    pData <- read.csv(fileName)
    
    ##  count complete cases (keeping vector for debugging)
    obsCnt[x] <- sum(complete.cases(pData))
    
    ##  if nobCnt is over the threshold perform the corr
    if (obsCnt[x] >= threshold & obsCnt[x] > 0) {
      ##  add the correlation to the vector
      corVec[x] <- cor(pData$sulfate,pData$nitrate,use="complete.obs") 
    }
    x<-x+1
  }
  corVec
  corVecNoNA <- corVec[!is.na(corVec)]
  corVecNoNA
}