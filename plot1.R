library(graphics)
plot1 <- function() {
        # Read in data
        NEI <- readRDS("summarySCC_PM25.rds")
        
        # Find total emissions for each year of interest
        s99 <- sum(NEI$Emissions[NEI$year == 1999])
        s02 <- sum(NEI$Emissions[NEI$year == 2002])
        s05 <- sum(NEI$Emissions[NEI$year == 2005])
        s08 <- sum(NEI$Emissions[NEI$year == 2008])
        
        # Create vectors for each year
        NEI99 <- c("Total.Emissions" = as.numeric(s99),
                   "year" = as.integer("1999"))
        NEI02 <- c("Total.Emissions" = as.numeric(s02),
                   "year" = as.integer("2002"))
        NEI05 <- c("Total.Emissions" = as.numeric(s05),
                   "year" = as.integer("2005"))
        NEI08 <- c("Total.Emissions" = as.numeric(s08),
                   "year" = as.integer("2008"))
        
        # Create a table with these four vectors
        NEIsummary <- rbind(NEI99, NEI02, NEI05, NEI08)
        
        # Make a barplot to a PNG file
        png(filename = "plot1.png")
        options(scipen=10)
        barplot(NEIsummary[,"Total.Emissions"],
                names=NEIsummary[,"year"],
                xlab="Year",
                ylab="Emissions (Tons)",
                main="Total PM2.5 Emissions from All Sources by Year")
        dev.off()
}
