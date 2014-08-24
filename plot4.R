library(graphics)
plot4 <- function() {
        # Read in NEI data
        NEI <- readRDS("summarySCC_PM25.rds")
        
        # Read in Source Classification Codes
        SCC <- readRDS("Source_Classification_Code.rds")  
        
        # Subset SCC codes based on EI.Sector field having the string "Coal" in it
        # Note: there is a long thread in the Forum discussing whether this 
        # is truly accurate; I erred on the side of simplicity as I don't believe
        # the point of this exercise is to check our domain knowledge - I 
        # think it doesn't really matter for the purposes of this question
        SCCCoalSources <- SCC[grep("Coal", SCC$EI.Sector),]
        
        # Now subset the NEI data based on SCC codes relevant to Coal sources
        NEICoalSources <- NEI[which(NEI$SCC %in% SCCCoalSources$SCC),]
        
        # De-duplicate the data
        NEICoalSources <- NEICoalSources[!duplicated(NEICoalSources),]

        # Sum the emissions for each year of interest
        sCoalSources99 <- sum(NEICoalSources$Emissions
                                           [NEICoalSources$year == 1999])
        sCoalSources02 <- sum(NEICoalSources$Emissions
                                           [NEICoalSources$year == 2002])
        sCoalSources05 <- sum(NEICoalSources$Emissions
                                           [NEICoalSources$year == 2005])
        sCoalSources08 <- sum(NEICoalSources$Emissions
                                           [NEICoalSources$year == 2008])
        
        # Create vectors for each year
        NEICoalSources99 <- c("Total.Emissions" = as.numeric(sCoalSources99),
                           "year" = as.integer("1999"))
        NEICoalSources02 <- c("Total.Emissions" = as.numeric(sCoalSources02),
                           "year" = as.integer("2002"))
        NEICoalSources05 <- c("Total.Emissions" = as.numeric(sCoalSources05),
                           "year" = as.integer("2005"))
        NEICoalSources08 <- c("Total.Emissions" = as.numeric(sCoalSources08),
                           "year" = as.integer("2008"))        
        
        
        # Construct a table with the sums 
        NEICoalSourcesSummary <- rbind(NEICoalSources99,
                                       NEICoalSources02,
                                       NEICoalSources05,
                                       NEICoalSources08)
        
        # Make a plot to a PNG file
        png(filename = "plot4.png", width = 640)        
        options(scipen=10)
        barplot(NEICoalSourcesSummary[,"Total.Emissions"],
                names=NEICoalSourcesSummary[,"year"],
                xlab="Year",
                ylab="Year to Year Emission Trend for Coal Sources"
        )
        dev.off()
}
