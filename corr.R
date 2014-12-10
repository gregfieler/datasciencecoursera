corr <- function(directory="specdata", threshold=0){
  ## Return numeric vector of correlations  

  corVec <- numeric(0)
  obsCnt <- numeric(0)
  x <- 1
  while (x < 333) {  
    ##  get the file name  
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
    
    ##  count complete cases
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