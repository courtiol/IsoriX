queryGNIP <- function(
	data,
	month.min = 1,
	month.max = 12,
	year.min,
	year.max,
	long.min ,
	long.max,
	lat.min,
	lat.max,
	prop.random = 0,
	random.level = "station"
	)
{

	## Some checks
	if((month.min %% 1 != 0) | (month.max %% 1 != 0)){
		stop("Months must be provided as a round numeric values or as an integer.")
	}

	if(prop.random>1){
		stop("The value you entered for prop.random is > 1. It must be a proportion (so between 0 and 1)!")
	}

	## Handle missing data
	if(missing("year.min")) year.min <- min(data$year, na.rm=TRUE)
	if(missing("year.max")) year.max <- max(data$year, na.rm=TRUE)
	if(missing("long.min")) long.min <- -180
	if(missing("long.max")) long.max <- 180
	if(missing("lat.min")) lat.min <- -90
	if(missing("lat.max")) lat.max <- 90 

	## Prepare selection
	if (month.max < month.min)
		month.select <- data$month >= month.min | data$month <= month.max
	else 
		month.select <- data$month >= month.min & data$month <= month.max

	year.select <- data$year >= year.min & data$year >= year.min
	long.select <- data$long >= long.min & data$long <= long.max
	lat.select  <- data$lat  >= lat.min  & data$lat  <= lat.max
	all.select <-  month.select & year.select & long.select & lat.select
	
	## Apply selection
	query.data <- data[all.select, ]

	## Defining function returning unique values with test
	unique2 <- function(x, key) {
		u.x <- unique(x)
		if(length(u.x)==1) return(u.x)
		warning(paste(c("some", key, "values are not unique but should be, so the first element was taken among:", u.x), collapse=" "))
		return(u.x[1])
	}

	## Defining function for aggregation
	aggregate.data <- function(d) {
		d <- droplevels(d)
		df <- data.frame(
			stationID=levels(d$stationID),
			isoscape.value=c(tapply(d$isoscape.value, d$stationID, mean, na.rm=TRUE)),
			var.isoscape.value=c(tapply(d$isoscape.value, d$stationID, var, na.rm=TRUE)),
			n.isoscape.value=c(tapply(d$stationID, d$stationID, length)),
			lat=c(tapply(d$lat, d$stationID, unique2, key="latitude")),
			long=c(tapply(d$long, d$stationID, unique2, key="longitude")),
			elev=c(tapply(d$elev, d$stationID, unique2, key="elevation")))
		## Note that above the c() prevent the creation of 1d arrays that are troublesome in spaMM
		if(any(df$n.isoscape.value==1)){
			warnings("station presenting a single observation have been discarded during aggregation")
			df <- df[df$n.isoscape.value>1, ]
		}
		df <- droplevels(df)
		rownames(df) <- NULL
		return(df)
	}

	## Return aggregated data if no random selection is needed
	if(prop.random==0){
		return(aggregate.data(query.data))
	}
 
	## Random draw of observations
	if (random.level == "obs") {
		howmanylines <- round(prop.random * nrow(query.data))
		whichlines   <- sample(x=nrow(query.data), size=howmanylines, replace=FALSE)
		selected.data  <- query.data[whichlines, ] 
		remaining.data <- query.data[-whichlines, ]
		return(list(selected.data = aggregate.data(selected.data),
					 remaining.data = aggregate.data(remaining.data))
		)
	} 

	## Random draw of stationID
	if (random.level == "station") {
		howmanystations <- round(prop.random * length(unique(query.data$stationID)))
		whichsstations   <- sample(x=unique(query.data$stationID), size=howmanystations, replace=FALSE)
		whichlines <- query.data$stationID %in% whichsstations
		selected.data  <- query.data[whichlines, ] 
		remaining.data <- query.data[-whichlines, ]
		return(list(selected.data = aggregate.data(selected.data),
					 remaining.data = aggregate.data(remaining.data))
		)
	}

	## Display error if no return encountered before
	stop("the argument you chose for random.level is unknown.")

}
