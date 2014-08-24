library(ggplot2)
plot5 <- function() {
        # Read in data and subset it to the city of interest (Baltimore City)
        NEI <- readRDS("summarySCC_PM25.rds")
        NEIBaltCity <- NEI[NEI$fips == 24510,]

        # Sum the emissions for source = "ON-ROAD" for each year of interest
        sBaltCityOnRoad99 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 1999])
        sBaltCityOnRoad02 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 2002])
        sBaltCityOnRoad05 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 2005])
        sBaltCityOnRoad08 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 2008])

        
        # Create vectors for each year
        NEIBaltCityOnRoad99 <- c("Total.Emissions" = as.numeric(sBaltCityOnRoad99),
                              "year" = as.integer("1999"))
        NEIBaltCityOnRoad02 <- c("Total.Emissions" = as.numeric(sBaltCityOnRoad02),
                              "year" = as.integer("2002"))
        NEIBaltCityOnRoad05 <- c("Total.Emissions" = as.numeric(sBaltCityOnRoad05),
                              "year" = as.integer("2005"))
        NEIBaltCityOnRoad08 <- c("Total.Emissions" = as.numeric(sBaltCityOnRoad08),
                              "year" = as.integer("2008"))        
        
        
        # Construct a table with the sums 
        NEIBaltCityOnRoadSummary <- rbind(NEIBaltCityOnRoad99,
                                          NEIBaltCityOnRoad02,
                                          NEIBaltCityOnRoad05,
                                          NEIBaltCityOnRoad08)
        
        
        # Make a plot to a PNG file
        png(filename = "plot5.png", width = 640)        
        options(scipen=10)
        barplot(NEIBaltCityOnRoadSummary[,"Total.Emissions"],
                names=NEIBaltCityOnRoadSummary[,"year"],
                xlab="Year",
                ylab="Total Emissions (ON-ROAD sources, Baltimore City)",
                main="Year to Year Emission Trend for Motor Vehicle Sources in Baltimore City"
        )
        dev.off()
}
