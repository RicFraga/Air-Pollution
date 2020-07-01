# Function to compute the correlation between the 2 pollutants
# (sulfate and nitrate) in the files where the complete
# observations are greater than the threshold
corr <- function(directory, threshold = 0) {
  
  values <- c()
  
  # Going through all of the files from the directory
  for(index in 1:323){
    
    # Reading the data from the current file
    data <- read_file(build_path(directory, index))
    
    # Getting the amount of complete observations in the file linked
    # to the index we are currently looping through
    amount_complete <- complete('Data/specdata/', index)$nobs
    
    # Checking if we are going to compute the correlation or not
    if(amount_complete > threshold) {
      
      # Getting the data from sulfate and nitrate
      sulfate <- data$sulfate
      nitrate <- data$nitrate
      
      # Appending that value to the vector
      values <- c(values, cor(sulfate, y = nitrate, use = 'complete.obs'))
    }
    # If we didn't compute the correlation we return a vector of length 0
    else {
      
      # Appending an empty value to the vector
      values <- c(values, c())
    }
  }
  
  return(values)
}

a <- corr('Data/specdata/', 400)
print(head(a))