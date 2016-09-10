RElevate <-
	function(
		elevation.raster,
		isofit=NULL,
		aggregation.factor=0L,
		aggregation.fun=mean,
		manual.crop=NULL,
		verbose=interactive()
	) {
	time <- system.time({
		if(!is.null(isofit)) {  ## test if cropping is needed
			if(!is.null(manual.crop)) stop("cannot crop both according to sources and manually! Make up your choice.")
			if( ## test if the elevation raster is not smaller than the area covered by the weather sources.
				## If yes crop will not proceed!
				xmin(elevation.raster)>min(isofit$mean.fit$data$long) |
				xmax(elevation.raster)<max(isofit$mean.fit$data$long) |
				ymin(elevation.raster)>min(isofit$mean.fit$data$lat) |
				ymax(elevation.raster)<max(isofit$mean.fit$data$lat))
					stop("cropping not possible (sources located outside elevation raster)")
			if(verbose)
				print(paste("cropping..."))
			## crop is performed:
			elevation.raster <- crop(
				elevation.raster,
				extent(
					min(isofit$mean.fit$data$long),
					max(isofit$mean.fit$data$long),
					min(isofit$mean.fit$data$lat),
					max(isofit$mean.fit$data$lat)
					)
				)
		} else {
			if(length(manual.crop)==4)
				elevation.raster <- crop(elevation.raster, manual.crop)
		}
		if(aggregation.factor>1) {  ## test if aggregation is needed
			if(interactive())
				print(paste("aggregating..."))
			elevation.raster <- aggregate(elevation.raster, fact=aggregation.factor, fun=aggregation.fun)  ## aggregation
		}
	})

	if(verbose) {
		print(paste("done!"))
		print(time)
	}

	print(elevation.raster)
	return(elevation.raster)
}
