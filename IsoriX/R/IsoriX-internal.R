## Here are internal functions of the package IsoriX

.onAttach <-
function (
	libname,
	pkgname
	) {
	## This function should not be called by the user.
	## It display a message when the package is being loaded
  ## and sets the proxy method Earth
	packageStartupMessage(  # display message
		"\n ############################################",
		"\n #                                          #",
		"\n #    IsoriX (version ", packageDescription("IsoriX")$Version,") is loaded!     #",
		"\n #                                          #",
		"\n #  Type ?IsoriX for a short description    #",
		"\n #                                          #",
		"\n #  Type browseVignettes(package='IsoriX')  #",
		"\n #    for details about how this package    #",
		"\n #         works and how to use it          #",
		"\n #                                          #",
		"\n #  Type news(package='IsoriX') for news    #",
		"\n #                                          #",
		"\n ############################################", "\n")
	set_ll_warn(TRUE)  ##makes sp creating warning instead of errors when lat/long out of boundaries

	## Let us set the new proxy method
	pr_DB$set_entry(FUN = .Dist.earth.mat, names = c("Earth", "dist.earth"))
	pr_DB$modify_entry(
		names = "Earth",
		description = "Approximate distance in Km between points on earth surface.",
		loop = FALSE,
		distance = TRUE
		)

}


.onDetach <- function(libpath) {
  ## This function should not be called by the user.
  ## It remove the proxy method Earth when pacakge is being detached
  pr_DB$delete_entry("Earth")
}


.Dist.earth.mat <- function (x, y=NULL) {
	## This function should not be called by the user but is itself called by other functions.
	## It compute orthodromic distances in Km between locations.
	if(is.null(y)) {
		coslat <- cos(x[, 2]*pi/180)
		sinlat <- sin(x[, 2]*pi/180)
		coslon <- cos(x[, 1]*pi/180)
		sinlon <- sin(x[, 1]*pi/180)
		pp <- cbind(coslat * coslon, coslat * sinlon, sinlat) %*% 
				t(cbind(coslat * coslon, coslat * sinlon, sinlat))
	} else { 
		coslat1 <- cos(x[, 2]*pi/180)
		sinlat1 <- sin(x[, 2]*pi/180)
		coslon1 <- cos(x[, 1]*pi/180)
		sinlon1 <- sin(x[, 1]*pi/180)
		coslat2 <- cos(y[, 2]*pi/180)
		sinlat2 <- sin(y[, 2]*pi/180)
		coslon2 <- cos(y[, 1]*pi/180)
		sinlon2 <- sin(y[, 1]*pi/180)
		pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*% 
				t(cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2))
	}
	## Earth radius used for approximation = 6371.009 = 1/3*(2*6378.137+6356.752)  [details on https://en.wikipedia.org/wiki/Great-circle_distance]
	pp <- 6371.009 * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp))
	if (is.null(y)) pp <- as.dist(pp)  ## spaMM wants an half matrix in this case, not a full one
	return(pp)
}


.NiceRound <- function(x, digits) formatC(round(x, digits), digits=digits, format="f")


.CreateRaster <-
function(
	long,
	lat,
	values,
	proj
	) {
	## This function should not be called by the user but is itself called by other functions.
	## It creates a raster.
	##
	## Args:
	##   long: a vector of the longitudes of the raster cells
	##   lat: a vector of the latitudes of the raster cells
	##   values: a vector of the values of the raster cells
	##   proj: the projection system for the raster
	##   save.spatial.files: logical indicating if an hard copy of the raster should be saved (as ascii)
	##   filename: name of the file for the hard copy
	##   overwrite.spatial.files: logical indicating if an existing hard copy should be overwritten or not
	##
	## Returns:
	##   The raster.
	##
	data <- data.frame(long=long, lat=lat, values=values)
	coordinates(data) <- ~long+lat  ## coordonates are being set for the raster
	proj4string(data) <- CRS(proj)  ## projection is being set for the raster
	gridded(data) <- TRUE  ## a gridded structure is being set for the raster
	data.raster <- raster(data)  ## the raster is being created
	# if(save.spatial.files) writeRaster(
	# 	data.raster,
	# 	filename=paste(filename, ".asc", sep=""),
	# 	overwrite=overwrite.spatial.files
	# 	)  ## if save=TRUE the raster is exported as an ascii file
	return(data.raster) ## the raster is being returned
}


.CreateSpatialPoints <-
function(
	long,
	lat,
	values=-9999,
	proj
	) {
	##  This function should not be called by the user but is itself called by .CreateRasterFromAssignment().
	data.sp <- data.frame(long=long, lat=lat, values=values)
	coordinates(data.sp) <- ~long+lat
	proj4string(data.sp) <- CRS(proj)
	# THE FOLLOWING IS COMMENTED AS GDAL IS SOURCE OF TROUBLE
	# if(save.spatial.files)   writeOGR(
	# 	data.sp,
	# 	dsn=".",
	# 	layer=filename,
	# 	driver="ESRI Shapefile",
	# 	overwrite_layer=overwrite.spatial.files
	# 	)
	return(data.sp)
}


.HitReturn <- function() {
	## This function should not be called by the user but is itself called by other functions.
	## It ask the user to press return in RStudio (for plotting).
	if(interactive() & .Platform$GUI == "RStudio") {
		cat ("Hit <Return> for next plot")
		readline()
	}
	return(NULL)
}


.CompleteArgs <- function(fn) {
	## This function should not be called by the user but is itself called by other functions.
	## It keeps the default list elements when
	## a new list with fewer elements is provided
	env <- parent.frame()
	args <- formals(fn)
	for(arg.name in names(args)) {
		if(is.call(arg <- args[[arg.name]])) {
			if(arg[1]=="list()") {
				arg.input <- mget(names(args), envir=env)[[arg.name]]
				arg.full  <- eval(formals(fn)[[arg.name]])
				arg.full.updated <- modifyList(arg.full, arg.input)
				assign(arg.name, arg.full.updated, envir=env)
			}
		}
	}
	return(NULL)
}


.BuildAdditionalLayers <- function(x, sources, calib, borders, mask) {
	## This function should not be called by the user but is itself called by other functions.
	## It build the additional layers for plots

	## layer for sources
	if(!sources$draw)
		sources.layer <- layer()
	else
		sources.layer <- layer(sp.points(sources, col=pt$col,
			cex=pt$cex, pch=pt$pch, lwd=pt$lwd),
			data=list(sources=x$sp.points$sources,
				pt=sources, sp.points=sp.points))

	## layer for calibration points
	if(is.null(calib))
		calib.layer <- layer()
	else {
		if(!calib$draw)
			calib.layer <- layer()
		else
			calib.layer <- layer(sp.points(calib, col=pt$col,
				cex=pt$cex, pch=pt$pch, lwd=pt$lwd),
				data=list(calib=x$sp.points$calibs,
					pt=calib, sp.points=sp.points))
	}

	## layer for country borders
	if(is.null(borders$borders))
		borders.layer <- layer()
	else
		borders.layer <- layer(sp.polygons(b$borders, lwd=b$lwd,
			col=b$col, fill="transparent"),
			data=list(b=borders, sp.polygons=sp.polygons))

	## layer for mask
	if(is.null(mask$mask))
		mask.layer <- layer()
	else
		mask.layer <- layer(sp.polygons(m$mask, fill=m$col),
			data=list(m=mask, sp.polygons=sp.polygons))

	out <- list(
		sources.layer=sources.layer,
		calib.layer=calib.layer,
		borders.layer=borders.layer,
		mask.layer=mask.layer
		)
	
	## tweack to please code checking procedure
	b <- m <- pt <- NULL

	return(out)
}
