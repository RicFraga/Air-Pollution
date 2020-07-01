# Function to read the data from a csv file
read_file <- function(file_path) {
  data <- read.csv(file_path)
  return(data)
}

# Function to build paths to specific files
build_path <- function(base_file_path, id) {
  
  # Building a path for each index
  for(index in id) {
    
    # If this is true, then the path will look something like 00x.csv
    if(index < 10) {
      full_file_path <- paste0(base_file_path, paste0('00', index))
      full_file_path <- paste0(full_file_path, '.csv')
    }
    
    # If this is true, then the path will look something like 0xx.csv
    else if(index >= 10 && index < 100) {
      full_file_path <- paste0(base_file_path, paste0('0', index))
      full_file_path <- paste0(full_file_path, '.csv')
    }
    
    # If this is true, then the path will look something like xxx.csv
    else if(index >= 100) {
      full_file_path <- paste0(base_file_path, index)
      full_file_path <- paste0(full_file_path, '.csv')
    }
  }
  
  return(full_file_path)
}

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

answer <- pollutantmean(directory = 'Data/specdata/', pollutant = 'nitrate', id = 23)
print(answer)
