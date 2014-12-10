complete <- function(directory, id = 1:332) {
  ##return a dataframe in the form
  ##   id   nobs
  ##    1   117
  ##    ...  ...
  ##  where id is the monitor ID number and 'nobs' is complete cases
  
  ##  initialize a dataframe with names "id" and "nobs"
  completeObs <- data.frame(id=NA,nobs=0)
 
  ##  loop thru the files identified in the parm

  for (x in 1:length(id)){
    
    ##  get the file name  
    if (id[x] < 10) {
      fileName <- paste(directory,"/","00",id[x],".csv",sep="")
    } else { 
      if (id[x] < 100) {
        fileName <- paste(directory,"/","0",id[x],".csv",sep="")
      } else {
        fileName <- paste(directory,"/",id[x],".csv",sep="")
      }}
    ##  read the csv file
    pData <- read.csv(fileName) 
    
    ##  count complete cases
    nobCnt <- sum(complete.cases(pData))
    ##  update the dataframe with ID and complete cases
    
    completeObs[x,1] <- id[x]
    completeObs[x,2] <- nobCnt

  }
  ##  return the dataframe
  completeObs
  
}