
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

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
  
  outcomeStateClean$rank <- NA
  outcomeStateClean$rank[order(outcomeStateClean[,3],outcomeStateClean[,2] )] <- 1:nrow(outcomeStateClean)

  # outcomeStateClean <- outcomeStateClean[outcomeStateClean$rank,]
  options(warn=0)

  #outcomeStateClean
  hospitalName <- NA

  if (!is.numeric(num))
  {
    if (num == "best")
    {
      print(num)
      hospitalName<- outcomeStateClean [outcomeStateClean$rank == 1,"Hospital.Name"]
    }
    if (num == "worst")
    {
      hospitalName<- outcomeStateClean [outcomeStateClean$rank == nrow(outcomeStateClean),"Hospital.Name"]

    }

  }
  else
  {
    if ((num > 0) & (num <= nrow(outcomeStateClean)))
    {
      hospitalName<- outcomeStateClean [outcomeStateClean$rank == num,"Hospital.Name"]
      
    }
  }

  hospitalName
}

## YOU CAN TEST IT WITH THE FOLLOWING CODE
## source("rankhospital.R")
## rankhospital("MD","heart attack")
## rankhospital("TX", "heart failure", 4)
## rankhospital("MD", "heart attack", "worst")



