rm(list = ls())
library(raster)

## We import the raw GNIP data
  rawGNIP <- read.csv("2016-10-31 Extract_ISORIX.csv")
  dim(rawGNIP)
  str(rawGNIP)
  head(rawGNIP)

## ID free to use German stations:
  free.stations <- c("ARKONA", "ARTERN", "BAD SALZUFLEN", "BERLIN", "BRAUNSCHWEIG", "CUXHAVEN", "DRESDEN", "EMMERICH",
                     "FEHMARN", "GARMISCH-PARTENKIRCHEN", "GOERLITZ", "HOF-SAALE", "HOHENPEISSENBERG", "KAHLER ASTEN",
                     "KARLSRUHE", "KOBLENZ", "KONSTANZ", "NEUBRANDENBURG", "NORDERNEY", "PASSAU-FUERSTENZELL",
                     "REGENSBURG", "SCHLESWIG", "SEEHAUSEN", "STUTTGART", "TRIER", "WASSERKUPPE RHOEN",
                     "WEIL AM RHEIN", "WUERZBURG")

## We extract time information
  rawGNIP$year.begin <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%Y"))
  rawGNIP$year.end <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%Y"))
  rawGNIP$year.span <- rawGNIP$year.begin - rawGNIP$year.end
  rawGNIP$month.begin <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%m"))
  rawGNIP$month.end <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%m"))
  rawGNIP$day.span <- as.Date(rawGNIP$Begin.Of.Period) - as.Date(rawGNIP$End.of.Period)
  rawGNIP$Year <- as.numeric(format(as.Date(rawGNIP$Date), "%Y"))
  rawGNIP$Month <- as.numeric(format(as.Date(rawGNIP$Date), "%m"))

## We select only rows with what we need
## and for which the span of the water collection was between 25 and 35 days
  out.index <- is.na(rawGNIP$H2) |
               is.na(rawGNIP$Latitude) | is.na(rawGNIP$Longitude) |
               is.na(rawGNIP$day.span) |
               rawGNIP$day.span > -25 | rawGNIP$day.span < -35 |
               !(rawGNIP$Name.of.Site %in% free.stations)

## We only keep the data we need
  keep <- c("Name.of.Site", "WMO.Code", "Latitude", "Longitude", "Altitude",
            "Year", "Month", "H2")
  smallrawGNIP <- rawGNIP[!out.index, keep]
  dim(smallrawGNIP)
  
  setdiff(free.stations, unique(smallrawGNIP$Name.of.Site)) ## No H2 data in HOF-SAALE!
  
## we turn the site into factors
  smallrawGNIP$WMO.Code <- as.factor(smallrawGNIP$WMO.Code)
  smallrawGNIP$Name.of.Site <- as.factor(smallrawGNIP$Name.of.Site)
  
## Export for version Isorix version 0.6
  GNIPDataDE <- smallrawGNIP[, c("Name.of.Site", "Latitude", "Longitude", "Altitude", "Year", "Month", "H2")]
  colnames(GNIPDataDE) <- c("stationID", "lat", "long", "elev", "year", "month", "isoscape.value")
  rownames(GNIPDataDE) <- NULL
  head(GNIPDataDE)
  str(GNIPDataDE)
  dim(GNIPDataDE)
  save(GNIPDataDE, file = "GNIPDataDE.rda")	
