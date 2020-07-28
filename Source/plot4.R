# Across the USA, how have emissions from coal combustion - related sources
# changed from 1999 - 2008?

library(ggplot2)

change_1999_2008 <- function(summary, source) {
     
     # First we need to 'link' the emissions data with the source data
     
     sources <- as.character(source[, 3])
     index <- c()
     
     for(i in 1:length(sources)) {
          if('Coal' %in% strsplit(sources[i], ' ')[[1]]) {
               index <- c(index, i)
          }
     }

     print(index)
     scc <- source[index, 1]
     
     # Now that we have the scc we get the summary data that matches it
     
     coal <- subset(summary, SCC == scc)
     
     coal_1999 <- subset(coal, year == 1999)$Emissions
     coal_2002 <- subset(coal, year == 2002)$Emissions
     coal_2005 <- subset(coal, year == 2005)$Emissions
     coal_2008 <- subset(coal, year == 2008)$Emissions
     
     emissions <- c(coal_1999, coal_2002, coal_2005, coal_2008)
     
     years = c(
          rep('1999', length(coal_1999)),
          rep('2002', length(coal_2002)),
          rep('2005', length(coal_2005)),
          rep('2008', length(coal_2008))
     )
     
     frame <- data.frame(year = years, emission = emissions)
     
     png('Graphs/plot4.png')
     print(ggplot(frame, aes(year, log10(emission), colour = year)) +
          geom_boxplot() + facet_wrap(~ year) + 
          labs(y = 'Emission') + 
          ggtitle('Emissions (log10) of Coal Combustion Sources by Year'))
     
     dev.off()
}