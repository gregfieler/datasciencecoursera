pollutantmean <- function(directory = "specdata",pollutant,id=1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  ## 'pollutant' is a charactor vector of length 1 indicating the name of the pollutant for which we calculate the mean; either "sulfate" or "nitrate"
  ##    sulfate is the second column, nitrate is the third
  ##  for the purpose of this exercise I will default to "sulfate"
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  
  pAllData <- vector()
  ##loop to get the data
  ##how many files is length of id vector - NOT WHICH FILES!!
  for (x in 1:length(id)){    
  ##get file name
    if (id[x] < 10) {
      fileName <- paste(directory,"/","00",id[x],".csv",sep="")
    } else { 
      if (id[x] < 100) {
        fileName <- paste(directory,"/","0",id[x],".csv",sep="")
      } else {
        fileName <- paste(directory,"/",id[x],".csv",sep="")
      }}
  ##load the data 
  pData <- read.csv(fileName) 
  if (pollutant == "nitrate") {
    pSample <- pData[,3]
  } else {
    pSample <- pData[,2]
  }
  pAllData <- append(pAllData, pSample, after=length(pAllData))
  }
  
  ##calculate the mean
  
  pMean <- mean(pAllData,na.rm=TRUE)
 
  pMean
  
  ## Return the mean of the pollutant across all monitors listed in the 'id' vector
}