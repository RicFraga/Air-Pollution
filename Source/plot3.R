# Of the four types, which of these four sources have seen decreases in 
# emissions from 1999-2008 for baltimore city? Which have seen increases in
# emissions from 1999 - 2008?

library(ggplot2)

baltimore_type <- function(data) {
     baltimore <- subset(data, fips == '24510')
     
     # Point data
     point_baltimore_mean_1999 <- mean(subset(baltimore, type == 'POINT'
                                              & year == 1999)$Emissions,
                                       na.rm = TRUE)
     
     point_baltimore_mean_2002 <- mean(subset(baltimore, type == 'POINT'
                                              & year == 2002)$Emissions,
                                       na.rm = TRUE)
     
     point_baltimore_mean_2005 <- mean(subset(baltimore, type == 'POINT'
                                              & year == 2005)$Emissions,
                                       na.rm = TRUE)
     
     point_baltimore_mean_2008 <- mean(subset(baltimore, type == 'POINT'
                                              & year == 2008)$Emissions,
                                       na.rm = TRUE)
     
     # Non point data
     nonpoint_baltimore_mean_1999 <- mean(subset(baltimore, type == 'NONPOINT'
                                              & year == 1999)$Emissions,
                                          na.rm = TRUE)
     
     nonpoint_baltimore_mean_2002 <- mean(subset(baltimore, type == 'NONPOINT'
                                              & year == 2002)$Emissions,
                                          na.rm = TRUE)
     
     nonpoint_baltimore_mean_2005 <- mean(subset(baltimore, type == 'NONPOINT'
                                              & year == 2005)$Emissions,
                                          na.rm = TRUE)
     
     nonpoint_baltimore_mean_2008 <- mean(subset(baltimore, type == 'NONPOINT'
                                              & year == 2008)$Emissions,
                                          na.rm = TRUE)
     
     # On road data
     onroad_baltimore_mean_1999 <- mean(subset(baltimore, type == 'ON-ROAD'
                                              & year == 1999)$Emissions,
                                        na.rm = TRUE)
     
     onroad_baltimore_mean_2002 <- mean(subset(baltimore, type == 'ON-ROAD'
                                              & year == 2002)$Emissions,
                                        na.rm = TRUE)
     
     onroad_baltimore_mean_2005 <- mean(subset(baltimore, type == 'ON-ROAD'
                                              & year == 2005)$Emissions,
                                        na.rm = TRUE)
     
     onroad_baltimore_mean_2008 <- mean(subset(baltimore, type == 'ON-ROAD'
                                              & year == 2008)$Emissions,
                                        na.rm = TRUE)
     
     # Non road data
     nonroad_baltimore_mean_1999 <- mean(subset(baltimore, type == 'NON-ROAD'
                                               & year == 1999)$Emissions,
                                         na.rm = TRUE)
     
     nonroad_baltimore_mean_2002 <- mean(subset(baltimore, type == 'NON-ROAD'
                                               & year == 2002)$Emissions,
                                         na.rm = TRUE)
     
     nonroad_baltimore_mean_2005 <- mean(subset(baltimore, type == 'NON-ROAD'
                                               & year == 2005)$Emissions,
                                         na.rm = TRUE)
     
     nonroad_baltimore_mean_2008 <- mean(subset(baltimore, type == 'NON-ROAD'
                                               & year == 2008)$Emissions,
                                         na.rm = TRUE)
     
     years <- rep(c('1999', '2002', '2005', '2008'), each = 4)
     types <- rep(c('Point', 'Nonpoint', 'Onroad', 'Nonroad'), 4)
     means <- c(point_baltimore_mean_1999, nonpoint_baltimore_mean_1999, 
                onroad_baltimore_mean_1999, nonroad_baltimore_mean_1999,
                
                point_baltimore_mean_2002, nonpoint_baltimore_mean_2002,
                onroad_baltimore_mean_2002, nonroad_baltimore_mean_2002,
                
                point_baltimore_mean_2005, nonpoint_baltimore_mean_2005,
                onroad_baltimore_mean_2005, nonroad_baltimore_mean_2005,
                
                point_baltimore_mean_2008, nonpoint_baltimore_mean_2008,
                onroad_baltimore_mean_2008, nonroad_baltimore_mean_2008)
     
     all <- data.frame(year = years, type = types, mean = means)
     
     png('Graphs/plot3.png')
     print(ggplot(all, aes(year, mean, group = 4)) +
          geom_point(colour = rgb(0.5, 0, 0.4)) +
          facet_wrap(~ type, scale = 'free_y') +
          geom_line() + ggtitle('Mean by Type Across Years'))
     
     dev.off()
     
}