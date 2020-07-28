# Have total emissions of PM25 decreased in the United States from 1999 to 2008?
total_emissions <- function(data) {
     
     png('Graphs/plot1-1.png')
     par(mfrow = c(2, 2))
     
     # Getting the data for 1999
     pm25_1999 <- subset(data, year == 1999)$Emissions
     plot(1:length(pm25_1999), pm25_1999, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'PM25 Emissions from 1999',
          col = rgb(1, 0, 0, 0.3), ylim = c(0, 150000))
     
     # Getting the data for 2002
     pm25_2002 <- subset(data, year == 2002)$Emissions
     plot(1:length(pm25_2002), pm25_2002, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'PM25 Emissions from 2002',
          col = rgb(1, 0, 0, 0.3), ylim = c(0, 150000))
     
     # Getting the data for 2005
     pm25_2005 <- subset(data, year == 2005)$Emissions
     plot(1:length(pm25_2005), pm25_2005, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'PM25 Emissions from 2005',
          col = rgb(1, 0, 0, 0.3), ylim = c(0, 150000))
     
     # Getting the data for 2008
     pm25_2008 <- subset(data, year == 2008)$Emissions
     plot(1:length(pm25_2008), pm25_2008, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'PM25 Emissions from 2008',
          col = rgb(1, 0, 0, 0.3), ylim = c(0, 150000))
     
     dev.off()
     
     png('Graphs/plot1-2.png')
     
     # Getting the mean of each year
     pm25_1999_mean <- mean(pm25_1999, na.rm = TRUE)
     pm25_2002_mean <- mean(pm25_2002, na.rm = TRUE)
     pm25_2005_mean <- mean(pm25_2005, na.rm = TRUE)
     pm25_2008_mean <- mean(pm25_2008, na.rm = TRUE)
     aux <- 1:4
     
     plot(c(pm25_1999_mean, pm25_2002_mean, pm25_2005_mean,
                 pm25_2008_mean) ~ aux, type = 'b', col = rgb(1, 0, 0), 
          pch = 19, xlab = 'Year', ylab = 'Mean'
          , main = 'PM2.5 Mean by Year')
     
     dev.off()
}