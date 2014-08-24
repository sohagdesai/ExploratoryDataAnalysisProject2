library(graphics)
plot6 <- function() {
        # Read in data and subset it to the city of interest (Baltimore City)
        NEI <- readRDS("summarySCC_PM25.rds")
        NEIBaltCity <- NEI[NEI$fips %in% "24510",]
        NEILACounty <- NEI[NEI$fips %in% "06037",]

        # Sum the emissions for source = "ON-ROAD" for Baltimore City 
        # for each year of interest
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
        
        # Sum the emissions for source = "ON-ROAD" for LA County 
        # for each year of interest
        sLACountyOnRoad99 <- sum(NEILACounty$Emissions
                                 [NEILACounty$type == "ON-ROAD" & 
                                          NEILACounty$year == 1999])
        sLACountyOnRoad02 <- sum(NEILACounty$Emissions
                                 [NEILACounty$type == "ON-ROAD" & 
                                          NEILACounty$year == 2002])
        sLACountyOnRoad05 <- sum(NEILACounty$Emissions
                                 [NEILACounty$type == "ON-ROAD" & 
                                          NEILACounty$year == 2005])
        sLACountyOnRoad08 <- sum(NEILACounty$Emissions
                                 [NEILACounty$type == "ON-ROAD" & 
                                          NEILACounty$year == 2008])        
        
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
        
        # Create vectors for each year
        NEILACountyOnRoad99 <- c("Total.Emissions" = as.numeric(sLACountyOnRoad99),
                              "year" = as.integer("1999"))
        NEILACountyOnRoad02 <- c("Total.Emissions" = as.numeric(sLACountyOnRoad02),
                              "year" = as.integer("2002"))
        NEILACountyOnRoad05 <- c("Total.Emissions" = as.numeric(sLACountyOnRoad05),
                              "year" = as.integer("2005"))
        NEILACountyOnRoad08 <- c("Total.Emissions" = as.numeric(sLACountyOnRoad08),
                              "year" = as.integer("2008"))        
        
        
        # Construct a table with the sums 
        NEILACountyOnRoadSummary <- rbind(NEILACountyOnRoad99,
                                          NEILACountyOnRoad02,
                                          NEILACountyOnRoad05,
                                          NEILACountyOnRoad08)
        
        # Make a plot to a PNG file
        png(filename = "plot6.png", width = 640)        
        options(scipen=10)
        
        # Set up the array of plots: 1 x 2
        par(mfrow = c(1,2))
        
        barplot(NEIBaltCityOnRoadSummary[,"Total.Emissions"],
                names=NEIBaltCityOnRoadSummary[,"year"],
                xlab="Year",
                ylab="Total Emissions (ON-ROAD sources)",
                main="Baltimore City"
        )

        barplot(NEILACountyOnRoadSummary[,"Total.Emissions"],
                names=NEILACountyOnRoadSummary[,"year"],
                xlab="Year",
                ylab="Total Emissions (ON-ROAD sources)",
                main="LA County"
        )
        dev.off()
}
