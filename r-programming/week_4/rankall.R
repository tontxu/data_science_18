
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  options(warn=-1)
  
  ## FORMATTING THE NAME OF THE COLUMN
  s <- strsplit(outcome, " ")[[1]]
  outcomeName<- paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=".")
  outcomeName <- paste(c("Hospital.30.Day.Death..Mortality..Rates.from.",outcomeName),collapse = "")
 
  ##LOADING DATA
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

 
  if (!outcomeName %in% colnames(outcome))
  {
    stop("invalid outcome")
  }
  

  outcomeState <- outcome[,c("Hospital.Name","State",outcomeName)]
  outcomeState[,outcomeName] <- as.numeric(outcomeState[,outcomeName] )

  outcomeStateClean <- outcomeState
# outcomeStateClean <- outcomeState[!is.na(outcomeState[,outcomeName]),]
  
  outcomeStateClean$rank <- NA
  
  finaloutcome <- NA
  

  # create a vector with all different states
  diffStates <- unique(outcomeStateClean[,2])
  
  for (x in diffStates)
  {
    newoutcome <- outcomeStateClean[outcomeStateClean[,2] == x,]
    newoutcome$rank[order(newoutcome[,3],newoutcome[,1] )] <- 1:nrow(newoutcome)
    finaloutcome <- rbind(finaloutcome,newoutcome )
  }

#  finaloutcome

  # outcomeStateClean <- outcomeStateClean[outcomeStateClean$rank,]
  options(warn=0)

  finaloutcome <- finaloutcome [!is.na(finaloutcome[,2]),]
  
  # to test stuff
  # finaloutcome[order(finaloutcome[,4]),]



  hospitals <- NA



  if (!is.numeric(num))
  {
    if (num == "best")
    {
      hospitals<- finaloutcome [finaloutcome$rank == 1,]

    }
    if (num == "worst")
    {

      finaloutcomeClean <- finaloutcome[!is.na(finaloutcome[,3]),]
      maxRank <- aggregate(finaloutcomeClean$rank ~ finaloutcomeClean$State, data = finaloutcomeClean, max)

      hospitals<- merge(x = maxRank, y = finaloutcomeClean, by.x = c("finaloutcomeClean$State","finaloutcomeClean$rank"), by.y = c("State","rank"), all.x = TRUE)
      hospitals<- hospitals[,c(1,3,4)]
      colnames(hospitals)[1] <- "State"

    }

  }
  else
  {
    if ((num > 0) & (num <= nrow(finaloutcome)))
    {
      hospitals<- finaloutcome [finaloutcome$rank == num,]

    }
  }

   hospitals <- hospitals[order(hospitals[,2]),]

   statesDF <- data.frame("State" = diffStates)

   finalResult <-    merge(x = statesDF, y = hospitals, by = "State", all.x = TRUE)
   finalResult <- finalResult[,c(2,1)]
   colnames(finalResult)[1] <- "hospital"
   colnames(finalResult)[2] <- "state"
   finalResult
   

  
}

## YOU CAN TEST IT WITH THE FOLLOWING CODE
## source("rankall.R")
## head(rankall("heart attack", 20),10)
## tail(rankall("pneumonia", "worst"), 3)
##  tail(rankall("heart failure"), 10)



