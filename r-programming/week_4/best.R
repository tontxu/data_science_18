
best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate

  options(warn=-1)
  
  ## FORMATTING THE NAME OF THE COLUMN
  s <- strsplit(outcome, " ")[[1]]
  outcomeName<- paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=".")
  outcomeName <- paste(c("Hospital.30.Day.Death..Mortality..Rates.from.",outcomeName),collapse = "")
 
  ##LOADING DATA
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## VALIDATION
  if (nrow(outcome[outcome$State==state,]) == 0)
  {
    stop("invalid state")
  }
  
  if (!outcomeName %in% colnames(outcome))
  {
    stop("invalid outcome")
  }
  
  outcomeState <- outcome[outcome$State == state,c("State","Hospital.Name",outcomeName)]
  outcomeState[,outcomeName] <- as.numeric(outcomeState[,outcomeName] )
  outcomeStateClean <- outcomeState[!is.na(outcomeState[,outcomeName]),]
  minim <- min(outcomeStateClean[,outcomeName])

  options(warn=0)
  
  
  bestHospitals <- outcomeStateClean[outcomeStateClean[,outcomeName] == minim,]
  bestHospitals <- bestHospitals[order("Hospital.Name"),]
  bestHospitals[1,"Hospital.Name"]
}


## YOU CAN TEST IT WITH THE FOLLOWING CODE
## source("best.R")
## best("MD","heart attack")
## best("TX","heart attack")
## best("MD","pneumonia")
## best("MD","heattack")
## best("XX","heart attack")


