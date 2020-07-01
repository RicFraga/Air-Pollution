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