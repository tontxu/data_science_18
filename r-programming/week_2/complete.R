
# rm(list = ls())
# setwd("C:/coursera/data_science_18/r-programming/week_2")

complete <- function (directory,  id=1:332)
{
  
  #initialize running total
  complete_cases <- data.frame(id = id, nobs = 0 )
  
  row.names(complete_cases) <- id

  #print(complete_cases)
  
  row <- 1
  
  for (file in id)
  {
    
    #print(file)
    
    #complexity is concatenating leading zeros to the file name and put it all together
    filename <- paste(paste(rep(0,times=(3-nchar(as.character(file)))),collapse=""), file,".csv",sep="")
    
    #For debugging
    #print(filename)
    
    
    
    if (file.exists(file.path(directory, filename)))
    {
      #intially i used nrows thinking 2000 rows would suffice but it didnt and I was only using partial data
      temp_data <- read.csv(file.path(directory, filename) 
                            , sep=","
                            #, nrows=2000
                            , header=TRUE 
                            , stringsAsFactors = FALSE
                            , comment.char = ""
                            , colClasses = c("Date", "numeric", "numeric", "numeric") )

      #remove incomplete rows
      clean_data <- temp_data[complete.cases(temp_data),]
      
      # add an entry 
      
      complete_cases[as.character(file),2] <- length (clean_data[,1])
      
            
    }
    

  }
  
  complete_cases
}