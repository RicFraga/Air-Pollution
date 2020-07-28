# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland?

baltimore_city <- function(data) {
     
     baltimore <- subset(data, fips == '24510')
     
     # Baltimore data from 1999
     baltimore_1999 <- subset(baltimore, year == 1999)$Emissions
     
     # Baltimore data from 2002
     baltimore_2002 <- subset(baltimore, year == 2002)$Emissions
     
     # Baltimore data from 2005
     baltimore_2005 <- subset(baltimore, year == 2005)$Emissions
     
     # Baltimore data from 2008
     baltimore_2008 <- subset(baltimore, year == 2008)$Emissions
     
     png('Graphs/plot2-1.png')
     par(mfrow = c(2, 2))
     
     plot(1:length(baltimore_1999), baltimore_1999, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'Baltimore PM25 Emissions from 1999',
          col = rgb(0.5, 0, 0.4, 0.3), ylim = c(0, 400))
     
     plot(1:length(baltimore_2002), baltimore_2002, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'Baltimore PM25 Emissions from 2002',
          col = rgb(0.5, 0, 0.4, 0.3), ylim = c(0, 400))
     
     plot(1:length(baltimore_2005), baltimore_2005, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'Baltimore PM25 Emissions from 2005',
          col = rgb(0.5, 0, 0.4, 0.3), ylim = c(0, 400))
     
     plot(1:length(baltimore_2008), baltimore_2008, xlab = 'Observation',
          ylab = 'Emissions (ton)', main = 'Baltimore PM25 Emissions from 2008',
          col = rgb(0.5, 0, 0.4, 0.3), ylim = c(0, 400))
     
     dev.off()
     
     png('Graphs/plot2-2.png')
     
     baltimore_1999_mean <- mean(baltimore_1999)
     baltimore_2002_mean <- mean(baltimore_2002)
     baltimore_2005_mean <- mean(baltimore_2005)
     baltimore_2008_mean <- mean(baltimore_2008)
     aux <- 1:4
     
     plot(c(baltimore_1999_mean, baltimore_2002_mean, baltimore_2005_mean, 
            baltimore_2008_mean) ~ aux, type = 'b', col = rgb(0.5, 0, 0.4), 
          pch = 19, xlab = 'Year', ylab = 'Mean', main = 
               'Baltimore PM2.5 Mean by Year')
     
     dev.off()
}