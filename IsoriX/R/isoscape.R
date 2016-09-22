isoscape <- function(
	elevation.raster,
	isofit,
	verbose=interactive()
	) {

	if(verbose)
		print(paste("building the isoscape... (may take a while)"))

	if(isofit$mean.fit$spaMM.version != packageVersion(pkg="spaMM"))
		warning("The isofit has been fitted on a different version of spaMM than the one called by IsoriX. This may create troubles in paradize...")

	time <- system.time({

		## we extract lat/long from all cells of the elevation raster
		coord <- coordinates(elevation.raster)
		long.to.do <- coord[, 1]  # extract the longitude
		lat.to.do <-  coord[, 2]  # extract the lattitude
		rm(coord); gc()  ## remove coord as it can be a large object

		## size of chunks to split the job into smaller ones
		chunk.size.for.predict <- 150L
		
		## indexes of beginning of each chunk and of last position are being computed
		steps <- c(seq(1, length(long.to.do), by=chunk.size.for.predict), length(long.to.do))
		
		## a logical indicating if a progression bar must be used
		draw.pb <- interactive() & (length(steps)-1) > 2

		## create empty vectors to store predictions
		mean.pred <- disp.pred <- rep(NA, length(long.to.do))
		mean.predVar <- mean.residVar <- mean.respVar <- mean.pred
		disp.predVar <- disp.residVar <- disp.respVar <- disp.pred

		## initiate the progress bar
		if(draw.pb)
			pb <- txtProgressBar(min = 1,
				max = (length(steps)-1), style = 3) 
		
		## we loop on each chunk of 150 locations
		for(i in 1:(length(steps)-1)) {
			
			if(draw.pb) setTxtProgressBar(pb, i) ## update progress bar
			
			## compute indexes for covariate values matching the current chunk
			within.steps <- steps[i]:steps[i+1]
			
			## select coordinates for prediction within chunk
			long <- long.to.do[within.steps]
			lat <- lat.to.do[within.steps]

			## we build xs non-specifically using most complex model definition
			## (it may look ugly but it should not increase much the computation time
			## and it avoids a lot of uglier code)
			xs <- data.frame(
				long=long,
				long.2=long^2,
				lat=lat,
				lat.abs=abs(lat),
				lat.2=lat^2,
				elev=extract(elevation.raster, cbind(long, lat)),
				stationID=as.factor(paste("new", within.steps, sep="_"))
				)
			
			## predictions from disp.fit
			pred.disp.fit <- predict.HLfit(
				object=isofit$disp.fit,
				newdata=xs,
				variances=list(respVar=TRUE)
			)
			
			## transmission of phi to mean.fit
			xs$pred.disp <- pred.disp.fit[, 1]

			## predictions from mean.fit
			pred.mean.fit <- predict.HLfit(
				object=isofit$mean.fit,
				newdata=xs,
				variances=list(respVar=TRUE)
			)

			## we save the predictions
			mean.pred[within.steps] <- pred.mean.fit[, 1]
			mean.predVar[within.steps]  <- attr(pred.mean.fit, "predVar")
			mean.residVar[within.steps] <- attr(pred.mean.fit, "residVar") ## same as disp.pred (as it should be)
			mean.respVar[within.steps]  <- attr(pred.mean.fit, "respVar")

			disp.pred[within.steps] <- pred.disp.fit[, 1]
			disp.predVar[within.steps]  <- attr(pred.disp.fit, "predVar")  ## same as mean.residVar (as it should be)
			disp.residVar[within.steps] <- attr(pred.disp.fit, "residVar")  
			disp.respVar[within.steps]  <- attr(pred.disp.fit, "respVar")

		}  ## we leave the loop on chunks

		## the progress bar is being closed
		if(draw.pb) close(pb)
	})  ## end of system.time

	## display time
	time <- round(as.numeric((time)[3]))
	if(verbose) {
		print(paste("predictions for all", length(long.to.do), "locations have been computed in", time, "sec."))
	}

	## we store the predictions for mean isotopic values into a raster
	SaveRaster <- function(x){
		.CreateRaster(
			long=long.to.do,
			lat=lat.to.do,
			values=x,
			proj="+proj=longlat +datum=WGS84"
			)
	}

	mean.raster <- SaveRaster(mean.pred)
	mean.predVar.raster <- SaveRaster(mean.predVar)
	mean.residVar.raster <- SaveRaster(mean.residVar)
	mean.respVar.raster <- SaveRaster(mean.respVar)

	disp.raster <- SaveRaster(disp.pred)
	disp.predVar.raster <- SaveRaster(disp.predVar)
	disp.residVar.raster <- SaveRaster(disp.residVar)
	disp.respVar.raster <- SaveRaster(disp.respVar)


	## we create the spatial points for sources
	source.points  <- .CreateSpatialPoints(
		long=isofit$mean.fit$data$long,
		lat=isofit$mean.fit$data$lat,
		proj="+proj=longlat +datum=WGS84"
	)

	## we put all rasters in a stack
	isoscape <- stack(list(
		"mean"=mean.raster,
		"mean.predVar"=mean.predVar.raster,
		"mean.residVar"=mean.residVar.raster,
		"mean.respVar"=mean.respVar.raster,
		"disp"=disp.raster,
		"disp.predVar"=disp.predVar.raster,
		"disp.residVar"=disp.residVar.raster,
		"disp.respVar"=disp.respVar.raster
		)
	)

	## we put the stack in a list that also contains
	## the spatial points for the sources
	out <- list(isoscape=isoscape,
		sp.points=list(sources=source.points))

	## we define a new class
	class(out) <- c("isoscape", "isofit", "list")

	return(out)
}


print.isoscape <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}


summary.isoscape <- function(object, ...) {
  if("isosim" %in% class(object)) {
    cat("\n")
    cat("##############################################", "\n")
    cat("### Note: this isoscape has been simulated ###", "\n")
    cat("##############################################", "\n")
    cat("\n")
  }
  cat("### stack containing the isoscape")
  print(object[[1]])
  cat("\n")
  if(length(object) > 1) {
    cat("### first 5 locations of the dataset")
    print(head(object[[2]][[1]], 5L))
  }
  return(invisible(NULL))
}

