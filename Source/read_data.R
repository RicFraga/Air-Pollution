# This file has the purpose of reading the data needed to generate the plots 
# used to answer the questions

summary_scc <- function() {
     data <- readRDS('Data/exdata_data_NEI_data/summarySCC_PM25.rds')
     
     return(data)
}

source_classification <- function() {
     data <- readRDS('Data/exdata_data_NEI_data/Source_Classification_Code.rds')
     
     return(data)
}