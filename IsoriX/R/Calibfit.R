Calibfit <- function(
		calib.data,
		isofit,
		verbose=interactive(),
		control.optim=list()
	) {

  time <- system.time({
    
  	## prepare the dataset
  	calib.data <- .PrepareDataCalib(calib.data)
  
  	## predict isoscape and associated prediction 
  	##   covariance matrix at animal locations
  
  	if(verbose)
  		print("predicting the isoscape value in each calibration site...")
  
  	calib.mean <- predict(isofit[["mean.fit"]], newdata=calib.data,
  		variances=list(predVar=TRUE, cov=TRUE))
  	
  	## store the mean prediction
  	calib.data$mean.iso <- c(calib.mean)
  
  	## extract the prediction covariance matrix
  	predcov.isofit.full <- attr(calib.mean, "predVar")
  
  	## extract the prediction variances
  	calib.data$mean.predVar.iso <- diag(predcov.isofit.full)
  	
  	## reshape the prediction covariance matrix to number of unique sites
  	firstoccurences <- match(levels(calib.data$siteID), calib.data$siteID)
  	predcov.isofit <- predcov.isofit.full[firstoccurences, firstoccurences]
  	rownames(predcov.isofit) <- levels(calib.data$siteID)
  	colnames(predcov.isofit) <- levels(calib.data$siteID)
  
  	### fitting the calibration function
  	if(verbose)
  		print("fitting the calibration function...")
  
  	## estimation of intercept and slope of the calibration function
  	opt.res <- optim(par=c(0, 1), fn=.ObjectiveFnCalib,
  		control=c(list(fnscale=-1), control.optim), data=calib.data, predcov=predcov.isofit,
  		lik.method="REML")
  
  	param.calibfit <- opt.res$par
  	names(param.calibfit) <- c("intercept", "slope")
  
  	## fit of the calibration function
  	calib.fit <- .ObjectiveFnCalib(param=param.calibfit,
  		data=calib.data, predcov=predcov.isofit, lik.method="REML", return.fit=TRUE)
  
  	## computing the covariance matrix of fixed effects
  	if(verbose)
  		print("computing the covariance matrix of fixed effects...")
  
  	fixefCov.calibfit <- solve(-hessian(.ObjectiveFnCalib, param.calibfit,
  		data=calib.data, predcov=predcov.isofit, lik.method="ML"))
  
  	rownames(fixefCov.calibfit) <- names(param.calibfit)
  	colnames(fixefCov.calibfit) <- names(param.calibfit)
  	
  }) ## end of system.time
  
  
  ## display time
  time <- round(as.numeric((time)[3]))
  if(verbose) {
    print(paste("the calibration procedure based on", nrow(calib.data), "calibration samples have been computed in", time, "sec."))
  }
  
	## we create the spatial points for calibration points
	calib.points  <- .CreateSpatialPoints(
		long=calib.data$long,
		lat=calib.data$lat,
		proj="+proj=longlat +datum=WGS84")

	## return
	out <- list(
		"param"=param.calibfit,
		"fixefCov"=fixefCov.calibfit,
		"calib.fit"=calib.fit,
		"calib.data"=calib.data,
		"sp.points"=list(calibs=calib.points))

	class(out) <- c("calibfit", "list")

	return(invisible(out))
}


.PrepareDataCalib <- function(data) {
	## This function should not be called by the user but is itself called by other functions.
	## It prepares data for the calibration procedure.
	if(!all(c("lat", "long") %in% colnames(data)))
		stop("the dataset does not seem to contain the required variable(s) lat and/or long")
	if(is.null(data$tissue.value))
		stop("the dataset does not seem to contain the required variable tissue.value")
	if(is.null(data$siteID))
		stop("the dataset does not seem to contain the required variable siteID")
	data$siteID <- factor(data$siteID)
	data$lat.abs <- abs(data$lat)
	data$lat.2 <- data$lat^2
	data$long.2 <- data$long^2
	data$stationID <- as.factor(paste("new", data$siteID, sep="_"))
	data <- droplevels(data)
	return(data)
}


.ObjectiveFnCalib <- function(param, data, predcov, return.fit=FALSE, lik.method="REML") {
	## This function should not be called by the user but is itself called by other functions.
	## It computes the likelihood of a given calibration function
	data$intercept <- param[1]
	data$slope <- param[2]
	calib.fit <- HLCor(
		formula=tissue.value ~ 0 + offset(intercept+slope*mean.iso) +
			corrMatrix(1|siteID) + (1|siteID),
		corrMatrix=predcov,
		ranPars=list(lambda=c(1e-6 + unique(data$slope)^2, NA)),
		data=data,
		method=lik.method)
	if(return.fit) return(calib.fit)
	return(calib.fit$APHLs$p_v)
}
