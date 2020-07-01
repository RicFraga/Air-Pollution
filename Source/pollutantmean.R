# Function to compute the mean of a specific pollutant in specific files
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # Initialize these values to keep record of which values have we seen
  # and how many have we seen
  sum <- 0
  total_samples <- 0
  
  # Going through each index
  for(index in id) {
    
    # Building a complete path to a file
    path <- build_path(directory, index)
    data <- read.csv(path)
    
    # If it is sulfate you want to compute the mean
    if(pollutant == 'sulfate') {
      
      # Getting the values where there are no NA
      values_sulfate <- which(!(is.na(data[['sulfate']])))
      
      # Adding how many values are we going to take
      total_samples <- total_samples + length(values_sulfate)
      
      # Summing all the values of sulfate
      for(index in values_sulfate) {
        sum <- sum + data[index, 'sulfate']
      }
    }
    
    # If it is nitrate you want to compute the mean
    if(pollutant == 'nitrate') {
      
      # Getting the values where there are no NA
      values_nitrate <- which(!(is.na(data[['nitrate']])))
      
      # Adding how many values are we going to take
      total_samples <- total_samples + length(values_nitrate)
      
      # Summing all the values of sulfate
      for(index in values_nitrate) {
        sum <- sum + data[index, 'nitrate']
      }
    }
  }
  
  return(sum / total_samples)
}