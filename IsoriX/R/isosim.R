isosim <- function(
	simu.data,
	mean.model.fix.coef=c(intercept=64, elev=-0.01, lat.abs=-2.3, lat.2=0, long=0, long.2=0),
	disp.model.fix.coef=c(intercept=5.8, elev=0, lat.abs=0, lat.2=0, long=0, long.2=0),
	mean.model.matern.coef=c(nu=0.35, rho=5e-5, lambda=899),
	disp.model.matern.coef=c(nu=3.2e-01, rho=1.5e-05, lambda=5),
	mean.model.uncorr.coef=c(nugget=0, lambda=0),
	disp.model.uncorr.coef=c(nugget=0, lambda=0),
	dist.method="Earth",
	seed=NULL,
	save.dataframe=FALSE,
	verbose=interactive()
	) {

	## if simu.data is a raster, we convert it as data.frame
	if(class(simu.data) == "RasterLayer") {
		elevation.raster <- simu.data
		coord <- coordinates(elevation.raster)
		simu.data <- data.frame(
			long=coord[, 1],
			long.2=coord[, 1]^2,
			lat=coord[, 2],
			lat.2=coord[, 2]^2,
			lat.abs=abs(coord[, 2]),
			elev=extract(elevation.raster, coord),
			n.isoscape.value=rep(1e6, nrow(coord)),
			stationID=as.factor(paste("simu", 1:nrow(coord), sep="_"))
			)
		rm(coord); gc() ## remove coord as it can be a large object
	}

	## test if inputs are correct
	if(!any(dist.method %in% c("Earth", "Euclidean")))
		stop("the argument you chose for dist.method is unknown")

	if(sum(mean.model.uncorr.coef>0)>1 | sum(disp.model.uncorr.coef>0)>1)
			stop("mean.model.uncorr.coef and disp.model.uncorr.coef must have at least one coeficient equals to zero each (considering two parameterizations of uncorrelated random effects does not make any sense.)")

	if(any(c(mean.model.uncorr.coef, disp.model.uncorr.coef)<0))
		stop("all nugget and lambda coefficients must be null or positive")	

	if(any(c(mean.model.matern.coef, disp.model.matern.coef)<0))
		stop("all spatial coefficients must be null or positive")

	data.needed <- names(c(mean.model.fix.coef, disp.model.fix.coef))
	data.needed <- data.needed[data.needed != "intercept"]

	if(!all(data.needed %in% names(simu.data)))
		stop(paste(c("you need to provide all the following covariates in simu.data (even if the coefficients associated to the missing ones are null):", data.needed), collapse=" "))

	## define the seeds (one for RandomFields, one for other R functions) and set other options for RandomFields
	set.seed(seed)
	RFoptions(
		seed=ifelse(is.null(seed), NA, seed),
		spConform=FALSE,  ##so that RFsimulte returns vector directly
		cPrintlevel=1)    ##cPrintlevel=3 for more details

	if(dist.method=="Earth") RFoptions(new_coord_sys="earth")
	if(dist.method=="Euclidean") RFoptions(new_coord_sys="cartesian")
	## see ?"coordinate system" for info about RF coordinate system


	### Simulate the dispersion

	if(verbose)
		print("Simulating the dispersion...")

	## compute the linear predictor
	linpred.disp <- .LinPred(
		fix.coef=disp.model.fix.coef,
		matern.coef=disp.model.matern.coef,
		uncorr.coef=disp.model.uncorr.coef,
		data=simu.data)

	simu.data$disp.logvar.fix <- linpred.disp$fix
	simu.data$disp.logvar.matern <- linpred.disp$matern
	simu.data$disp.logvar.uncorr <- linpred.disp$uncorr
	simu.data$disp.mean <- exp(linpred.disp$eta.sum)

	## add residual variance
	simu.data$var.isoscape.value <- rgamma(
		nrow(simu.data),
		shape=(simu.data$disp.mean^2)/2,
		scale=2/simu.data$disp.mean)

	
	### Simulate the mean
	if(verbose)
		print("Simulating the mean...")

	## compute the linear predictor
	linpred.mean <- .LinPred(
		fix.coef=mean.model.fix.coef,
		matern.coef=mean.model.matern.coef,
		uncorr.coef=mean.model.uncorr.coef,
		data=simu.data)

	simu.data$mean.var.fix <- linpred.mean$fix
	simu.data$mean.var.matern <- linpred.mean$matern
	simu.data$mean.var.uncorr <- linpred.mean$uncorr
	simu.data$isoscape.value <- linpred.mean$eta.sum


	### Building rasters
	if(verbose)
		print("Building rasters...")

	SaveRaster <- function(x){
		with(simu.data, {
		.CreateRaster(
			long=long,
			lat=lat,
			values=get(x),
			proj="+proj=longlat +datum=WGS84")
			})
	}

	mean.raster <- SaveRaster("isoscape.value")
	disp.raster <- SaveRaster("var.isoscape.value")

	### Buidling return object
	out <- list()

	out$isoscape <- stack(list(
		"mean"=mean.raster,
		"disp"=disp.raster
		))

	if(!save.dataframe & interactive())
		message(paste("Note: simulated data not saved as data.frame (save.dataframe is set to FALSE). Saving the simulated data as data.frame would require", format(object.size(simu.data), units="MB")))
	else
		out$data <- simu.data

	class(out) <- c("isoscape", "isosim", "list")

	return(out)
}


.LinPred <- function(fix.coef, matern.coef, uncorr.coef, data) {
	## This function should not be called by the user but is itself called by other functions.
	## It builds the linear predictor for the simulations

	## fixed effects
	fix <- with(as.list(fix.coef), intercept +
		elev*data$elev + lat.abs*data$lat.abs + lat.2*data$lat.2 +
		long*data$long + long.2*data$long.2)

	## spatial random effects
	matern <- 0
	if(matern.coef["lambda"] > 0) {
		model.matern <- with(as.list(matern.coef),
			RMwhittle(nu=nu, var=lambda, scale=1/rho))
		matern <- RFsimulate(model.matern,
			x=data$long, y=data$lat)
	}

	## uncorr random effects
	uncorr <- rnorm(nrow(data), mean=0, sd=sqrt(uncorr.coef["lambda"]))
	if(uncorr.coef["nugget"] > 0)
		uncorr <- uncorr + RFsimulate(RMnugget(var=uncorr.coef["nugget"]),
			x=data$long, y=data$lat)

	eta.sum <- fix + matern + uncorr
	return(list("fix"=fix, "matern"=matern, "uncorr"=uncorr, "eta.sum"=eta.sum))
}
