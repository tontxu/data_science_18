
# rm(list = ls())
# setwd("C:/coursera/data_science_18/r-programming/week_2")

corr <- function (directory, threshold = 0)
{
  
  #initialize running total
  total_correlation <- vector("numeric")
  
  id <- 1:332 
  
  for (file in id)
  {
    
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
      
      number_complete_cases <- length (clean_data[,1])
      
      
      #print (number_complete_cases)
      
      if ( number_complete_cases > threshold)
      {
        
        
        # we put all the data together in the same vector for a running total
        
        total_correlation <- c(total_correlation,cor(clean_data[,"sulfate"], clean_data[,"nitrate"] ))
        
        
      }
      
            
    }
    


  }
  
  total_correlation
}