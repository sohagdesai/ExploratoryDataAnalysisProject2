library(ggplot2)
plot3 <- function() {
        # Read in data and subset it to the city of interest (Baltimore City)
        NEI <- readRDS("summarySCC_PM25.rds")
        NEIBaltCity <- NEI[NEI$fips == 24510,]

        # Sum the emissions for source = "POINT" for each year of interest
        NEIBaltCityPoint99 <- sum(NEIBaltCity$Emissions
                                  [NEIBaltCity$type == "POINT" & 
                                           NEIBaltCity$year == 1999])
        NEIBaltCityPoint02 <- sum(NEIBaltCity$Emissions
                                  [NEIBaltCity$type == "POINT" & 
                                           NEIBaltCity$year == 2002])
        NEIBaltCityPoint05 <- sum(NEIBaltCity$Emissions
                                  [NEIBaltCity$type == "POINT" & 
                                           NEIBaltCity$year == 2005])
        NEIBaltCityPoint08 <- sum(NEIBaltCity$Emissions
                                  [NEIBaltCity$type == "POINT" & 
                                           NEIBaltCity$year == 2008])

        # Sum the emissions for source = "NONPOINT" for each year of interest
        NEIBaltCityNonPoint99 <- sum(NEIBaltCity$Emissions
                                     [NEIBaltCity$type == "NONPOINT" & 
                                           NEIBaltCity$year == 1999])
        NEIBaltCityNonPoint02 <- sum(NEIBaltCity$Emissions
                                     [NEIBaltCity$type == "NONPOINT" & 
                                           NEIBaltCity$year == 2002])
        NEIBaltCityNonPoint05 <- sum(NEIBaltCity$Emissions
                                     [NEIBaltCity$type == "NONPOINT" & 
                                           NEIBaltCity$year == 2005])
        NEIBaltCityNonPoint08 <- sum(NEIBaltCity$Emissions
                                     [NEIBaltCity$type == "NONPOINT" & 
                                           NEIBaltCity$year == 2008])

        # Sum the emissions for source = "ON-ROAD" for each year of interest
        NEIBaltCityOnRoad99 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 1999])
        NEIBaltCityOnRoad02 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 2002])
        NEIBaltCityOnRoad05 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 2005])
        NEIBaltCityOnRoad08 <- sum(NEIBaltCity$Emissions
                                   [NEIBaltCity$type == "ON-ROAD" & 
                                            NEIBaltCity$year == 2008])
        
        # Sum the emissions for source = "NON-ROAD" for each year of interest
        NEIBaltCityNonRoad99 <- sum(NEIBaltCity$Emissions
                                    [NEIBaltCity$type == "NON-ROAD" & 
                                             NEIBaltCity$year == 1999])
        NEIBaltCityNonRoad02 <- sum(NEIBaltCity$Emissions
                                    [NEIBaltCity$type == "NON-ROAD" & 
                                             NEIBaltCity$year == 2002])
        NEIBaltCityNonRoad05 <- sum(NEIBaltCity$Emissions
                                    [NEIBaltCity$type == "NON-ROAD" & 
                                             NEIBaltCity$year == 2005])
        NEIBaltCityNonRoad08 <- sum(NEIBaltCity$Emissions
                                    [NEIBaltCity$type == "NON-ROAD" & 
                                             NEIBaltCity$year == 2008])

        # Construct a table with the sums for source type = "POINT"
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityPoint99,
                                     "type" = "POINT", 
                                     "year" = 1999))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityPoint02,
                                     "type" = "POINT", 
                                     "year" = 2002))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityPoint05,
                                     "type" = "POINT", 
                                     "year" = 2005))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityPoint08,
                                     "type" = "POINT", 
                                     "year" = 2008))
        
        # Construct a table with the sums for source type = "NONPOINT"
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonPoint99,
                                     "type" = "NONPOINT", 
                                     "year" = 1999))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonPoint02,
                                     "type" = "NONPOINT", 
                                     "year" = 2002))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonPoint05,
                                     "type" = "NONPOINT", 
                                     "year" = 2005))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonPoint08,
                                     "type" = "NONPOINT", 
                                     "year" = 2008))
        
        # Construct a table with the sums for source type = "NON-ROAD"
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonRoad99,
                                     "type" = "NON-ROAD", 
                                     "year" = 1999))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonRoad02,
                                     "type" = "NON-ROAD", 
                                     "year" = 2002))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonRoad05,
                                     "type" = "NON-ROAD", 
                                     "year" = 2005))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityNonRoad08,
                                     "type" = "NON-ROAD", 
                                     "year" = 2008))
        
        # Construct a table with the sums for source type = "ON-ROAD"
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityOnRoad99,
                                     "type" = "ON-ROAD", 
                                     "year" = 1999))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityOnRoad02,
                                     "type" = "ON-ROAD", 
                                     "year" = 2002))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityOnRoad05,
                                     "type" = "ON-ROAD", 
                                     "year" = 2005))
        NEIBaltCitySummary = rbind(NEIBaltCitySummary,
                                   c("Emission" = NEIBaltCityOnRoad08,
                                     "type" = "ON-ROAD", 
                                     "year" = 2008))

        # Make a plot to a PNG file
        png(filename = "plot3.png", width = 640)
        options(scipen=10)
        print(
            qplot(as.factor(year),
              as.numeric(Emission),
              data   =  as.data.frame(NEIBaltCitySummary), 
              geom   =  c("point","smooth"), 
              group  =  1, 
              facets =  . ~ type, 
              method =  "lm", 
              xlab   =  "Year",
              ylab   =  "Total Emissions", 
              color  =  type, 
              main   =  "Year to Year Emission Trend for Baltimore City by Source Type")
        )
        dev.off()
}
