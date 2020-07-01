# Function to generate a data frame of the form:

#     id  nob
# 1   1    x1
# 2   5    x2
# 3   7    x2

# Where id is the number of the file we are analyzing and nob
# is the number of complete cases in the observations

complete <- function(directory, id = 1:332) {
  
  base_file_path <- directory
  
  # Creating a matrix to generate the data frame
  
  # nrow = length(id) because thats the amount of observations we are
  # going to analyze
  # ncol = 2 because we are only going to store 2 values (id and nobs)
  matrix_data <- matrix(nrow = length(id), ncol = 2)
  
  # Variable to keep track of the vectors of the matrix
  matrix_index <- 1
  
  # Going through each index
  for(index in id) {
    
    # Generating each path to access it
    full_file_path <- build_path(base_file_path, index)

    # Reading the data in the full file path
    data <- read_file(full_file_path)
    
    # Getting the complete cases in the file
    complete_cases <- length(which(complete.cases(data[, 'nitrate'], data[, 'sulfate'])))
    
    # Saving the data to the matrix
    matrix_data[matrix_index, 1] <- index
    matrix_data[matrix_index, 2] <- complete_cases
    
    # Incrementing the matrix index to move to the next position
    matrix_index <- matrix_index + 1
  }
  
  colnames(matrix_data) <- list('id', 'nobs')
  return(data.frame(matrix_data))
}


print(complete('Data/specdata/', 30:25))