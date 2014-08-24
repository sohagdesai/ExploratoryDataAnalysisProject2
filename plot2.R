library(graphics)
plot2 <- function() {
        # Read in data
        NEI <- readRDS("summarySCC_PM25.rds")
        
        # Find total emissions for each year of interest for city of interest (Baltimore City)
        sBaltCity99 <- sum(NEI$Emissions[NEI$year == 1999 & NEI$fips == 24510])
        sBaltCity02 <- sum(NEI$Emissions[NEI$year == 2002 & NEI$fips == 24510])
        sBaltCity05 <- sum(NEI$Emissions[NEI$year == 2005 & NEI$fips == 24510])
        sBaltCity08 <- sum(NEI$Emissions[NEI$year == 2008 & NEI$fips == 24510])
        
        # Create vectors for each year
        NEIBaltCity99 <- c("Total.Emissions" = as.numeric(sBaltCity99),
                           "year" = as.integer("1999"))
        NEIBaltCity02 <- c("Total.Emissions" = as.numeric(sBaltCity02),
                           "year" = as.integer("2002"))
        NEIBaltCity05 <- c("Total.Emissions" = as.numeric(sBaltCity05),
                           "year" = as.integer("2005"))
        NEIBaltCity08 <- c("Total.Emissions" = as.numeric(sBaltCity08),
                           "year" = as.integer("2008"))
        
        # Create a table with these four vectors
        NEIBaltCitySummary <- rbind(NEIBaltCity99,
                                    NEIBaltCity02,
                                    NEIBaltCity05,
                                    NEIBaltCity08)
        
        # Make a barplot to a PNG file
        png(filename = "plot2.png")
        options(scipen=10)
        barplot(NEIBaltCitySummary[,"Total.Emissions"],
                names=NEIBaltCitySummary[,"year"],
                xlab="Year",
                ylab="Emissions (Tons)",
                main="Total PM2.5 Emissions from All Sources for Baltimore City by Year")
        dev.off()
}
