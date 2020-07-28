# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == 06037),
# which has seen greater changes over time in motor vehicle emissions?

library(ggplot2)

baltimore_vs_la <- function(summary, source) {
     
     # Get the scc for the gasoline emissions
     sources <- as.character(source[, 8])
     index <- c()
     
     for(i in 1:length(sources)) {
          if('Gasoline' %in% strsplit(sources[i], ' ')[[1]]) {
               index <- c(index, TRUE)
          }
          
          else {
               index <- c(index, FALSE)
          }
     }
     
     scc <- source[index, 1]
     
     matched <- summary[scc, ]
     
     baltimore <- subset(matched, fips == '09001' | fips == '09005')
     la <- subset(matched, fips == '09003' | fips == '09015')
     
     baltimore_1999 <- subset(baltimore, year == 1999)$Emissions
     baltimore_2002 <- subset(baltimore, year == 2002)$Emissions
     baltimore_2005 <- subset(baltimore, year == 2005)$Emissions
     baltimore_2008 <- subset(baltimore, year == 2008)$Emissions
     
     la_1999 <- subset(la, year == 1999)$Emissions
     la_2002 <- subset(la, year == 2002)$Emissions
     la_2005 <- subset(la, year == 2005)$Emissions
     la_2008 <- subset(la, year == 2008)$Emissions
     
     emissions <- c(baltimore_1999, la_1999)
     id <- c(
          rep('Baltimore', length(baltimore_1999)),
          rep('Los Angeles', length(la_1999))
     )
     
     frame <- data.frame(emission = emissions, city = id)
     
     png('Graphs/plot6.png')
     print(ggplot(frame, aes(city, log10(emission), colour = city)) + 
          geom_boxplot() + 
          facet_wrap(~ city)) +
          ggtitle('Motor Vehicle Based Emissions from Baltimore City and
                  Los Angeles across 1999 - 2008')
     
     dev.off()
}