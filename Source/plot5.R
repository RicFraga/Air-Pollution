# How have emissions from motor vehicle sources changed from 1999-2008 in
# Baltimore City?

library(ggplot2)

motor_vehicle_bm <- function(summary, source) {
     
     # Get the scc for the gasoline emissions
     sources <- as.character(source[, 3])
     index <- c()
     
     for(i in 1:length(sources)) {
          if('Gasoline' %in% strsplit(sources[i], ' ')[[1]]) {
               index <- c(index, i)
          }
     }
     
     scc <- source[index, 1]
     
     gasoline <- subset(summary, SCC == scc & fips == '24510')
     
     gasoline_1999 <- subset(gasoline, year == 1999)$Emissions
     
     gasoline_2002 <- subset(gasoline, year == 2002)$Emissions
     
     gasoline_2005 <- subset(gasoline, year == 2005)$Emissions
     
     gasoline_2008 <- subset(gasoline, year == 2008)$Emissions
     
     emissions <- c(gasoline_1999, gasoline_2002, gasoline_2005,
                    gasoline_2008)
     
     years = c(
          rep('1999', length(gasoline_1999)),
          rep('2002', length(gasoline_2002)),
          rep('2005', length(gasoline_2005)),
          rep('2008', length(gasoline_2008))
     )
     
     frame <- data.frame(year = years, emission = emissions)
     
     png('Graphs/plot5.png')
     print(ggplot(frame, aes(year, log10(emission), colour = year)) +
                geom_boxplot() + facet_wrap(~ year) + 
                labs(y = 'Emission') + 
                ggtitle('Emissions (log10) of Motor Vehicle Sources by Year in 
                        Baltimore City'))
     
     dev.off()
}