Isofit <- function(
  iso.data,
  mean.model.fix=list(elev=FALSE, lat.abs=FALSE, lat.2=FALSE, long=FALSE, long.2=FALSE),
  disp.model.fix=list(elev=FALSE, lat.abs=FALSE, lat.2=FALSE, long=FALSE, long.2=FALSE),
  mean.model.rand=list(uncorr=FALSE, spatial=TRUE),
  disp.model.rand=list(uncorr=FALSE, spatial=TRUE),
  uncorr.terms=list(mean.model="lambda", disp.model="lambda"), ## or: "nugget"
  spaMM.method=list(mean.model="fitme", disp.model="fitme"), ## or: "corrHLfit", "HLfit"
  dist.method="Earth", ## or: "Euclidean"
  control.mean=list(),
  control.disp=list(),
  verbose=interactive()
) {
  .Defunct("isofit")
}

isofit <- function(
		iso.data,
		mean.model.fix=list(elev=FALSE, lat.abs=FALSE, lat.2=FALSE, long=FALSE, long.2=FALSE),
		disp.model.fix=list(elev=FALSE, lat.abs=FALSE, lat.2=FALSE, long=FALSE, long.2=FALSE),
		mean.model.rand=list(uncorr=FALSE, spatial=TRUE),
		disp.model.rand=list(uncorr=FALSE, spatial=TRUE),
		uncorr.terms=list(mean.model="lambda", disp.model="lambda"), ## or: "nugget"
		spaMM.method=list(mean.model="fitme", disp.model="fitme"), ## or: "corrHLfit", "HLfit"
		dist.method="Earth", ## or: "Euclidean"
		control.mean=list(),
		control.disp=list(),
		verbose=interactive()
	) {

		## Complete the arguments
		.CompleteArgs(isofit)

		## Save the call information
		info.fit <- mget(names(formals()))
		info.fit$IsoriX_version <- packageDescription("IsoriX")$Version
		info.fit$verbose <- verbose

		## Check that arguments are correct and to some extent test that they make sense
		if(!dist.method %in% c("Euclidean", "Earth"))
			warnings("the argument you chose for dist.method may not be safe to use, please use 'Euclidean' or 'Earth' unless you really know how this package works")
		if(!all(unlist(spaMM.method) %in% c("fitme", "corrHLfit", "HLfit")))
			stop("the argument(s) you chose for spaMM.method are unknown")

		## Partially check that the different arguments are compatible between each others
		if(sum(unlist(mean.model.rand))==0 & (spaMM.method$mean.model=="corrHLfit" | spaMM.method$disp.model=="corrHLfit"))
			stop("your call does not make sense: the spaMM.method 'corrHLfit' should only be used when random effects are present")
		if (!mean.model.rand$spatial & !disp.model.rand$spatial &
			all(unlist(uncorr.terms) !=c("lambda", "lambda")))
			stop("in the absence of spatial random effects, only lambda can be used as uncorr.terms")


		## Prepare the dataset
		iso.data <- .PrepareDataIso(iso.data)

		## Define the formulas for each model   
		mean.formula <- .PrepareFormula("isoscape.value ~ 1",
			fix=mean.model.fix, rand=mean.model.rand, rand.p=uncorr.terms$mean.model)
		disp.formula <- .PrepareFormula("var.isoscape.value ~ 1",
			fix=disp.model.fix, rand=disp.model.rand, rand.p=uncorr.terms$disp.model)


		## Define weights
		iso.data$weights.mean <- as.numeric(iso.data$n.isoscape.value)
		iso.data$weights.disp <- as.numeric(iso.data$n.isoscape.value)-1
		if(any(iso.data$weights.disp<1))
			warning("some prior weights seem to be null")
		
		## Define the baseline argument lists for the models irrespective of the spaMM.method       
		args.disp.fit <- list(
			formula=formula(disp.formula),
			family=Gamma(log),
			prior.weights=iso.data$weights.disp,
			data=iso.data
		)

		args.mean.fit <- list(
			formula=formula(mean.formula),
			prior.weights=iso.data$weights.mean,
			resid.model=list(formula= ~ 0 + offset(pred.disp), family=Gamma(identity)),
			data=iso.data
		)

		## Inclusion of additional arguments for corrHLfit, if necessary
		if(spaMM.method[1]=="corrHLfit"){
			args.mean.fit[["control.corrHLfit"]] <- list(maxcorners=0)
			if(mean.model.rand$spatial){
				args.mean.fit[["control.dist"]] <- list(dist.method=dist.method)
				if(uncorr.terms$mean.model=="nugget") args.mean.fit[["init.corrHLfit"]] <- list(Nugget=0.01)
			}
		  }
		if(spaMM.method[2]=="corrHLfit"){
			if(disp.model.rand$spatial){
				args.disp.fit[["control.dist"]] <- list(dist.method=dist.method)
				if(uncorr.terms$disp.model=="nugget") args.disp.fit[["init.corrHLfit"]] <- list(Nugget=0.01)
			}
			args.disp.fit[["control.corrHLfit"]] <- list(maxcorners=0)
			args.disp.fit[["ranFix"]] <- list(phi=2)  
		}

		## Inclusion of additional arguments for fitme, if necessary
		if(spaMM.method[1]=="fitme") {
			args.mean.fit[["method"]] <- "REML"
			if(mean.model.rand$spatial){
				args.mean.fit[["control.dist"]] <- list(dist.method=dist.method)
				if(uncorr.terms$mean.model=="nugget") args.mean.fit[["init"]] <- list(Nugget=0.01)
			}
		  }
		if(spaMM.method[2]=="fitme"){
			args.disp.fit[["method"]] <- "REML"
			args.disp.fit[["fixed"]] <- list(phi=2)
			if(disp.model.rand$spatial){
				args.disp.fit[["control.dist"]] <- list(dist.method=dist.method)
				if(uncorr.terms$disp.model=="nugget") args.disp.fit[["init"]] <- list(Nugget=0.01)
			}
		  }

		## Interactive display
		if(verbose) {
			nug.string <- ifelse(uncorr.terms$disp.model=="nugget", "with a Nugget", "")
			print(paste("fitting the following residual dispersion model using spaMM", nug.string, ":"))
			print(disp.formula)
			if(sum(unlist(disp.model.rand))>0) print(paste("it may take a while..."))
		}
		
		## Fit disp.fit
		time.disp <- system.time(disp.fit <- do.call(spaMM.method$disp.model, c(args.disp.fit, control.disp)))

		## Predict the values for the residual variance
		args.mean.fit$data$pred.disp <- predict(disp.fit)[, 1]

		## Interactive display
		if(verbose) {
			nug.string <- ifelse(uncorr.terms$mean.model=="nugget", "with a Nugget", "")
			print(paste("fitting the following mean model using spaMM", nug.string, ":"))
			print(mean.formula)
			if(sum(unlist(mean.model.rand))>0) print(paste("it may take a while..."))
		}
		
		## Fit mean.fit
		time.mean <- system.time(mean.fit <- do.call(spaMM.method$mean.model, c(args.mean.fit, control.mean)))
	
		## Interactive display of fit time duration
		total.time <- round(as.numeric((time.mean+time.disp)[3]))
		if(verbose)
			print(paste("Done! Models were fitted in", total.time, "sec."))

		## Store the time
		info.fit$time.fit <- total.time

		## Create the return object
		out <- list("mean.fit"=mean.fit,
					"disp.fit"=disp.fit,
					"info.fit"=info.fit)
		class(out) <- c("isofit", "list")

		return(invisible(out))
	}


.PrepareDataIso <- function(data) {
	## This function should not be called by the user but is itself called by other functions.
	## It prepares data for the prediction procedures.
	if(!all(c("lat", "long") %in% colnames(data)))
		stop("the dataset does not seem to contain the required variable(s) lat and/or long")
	if(max(table(data$lat, data$long))>1)
		stop("the dataset does not seem to be aggregated, make sure you only have a single row per location in your dataset")     
	if(is.null(data$var.isoscape.value))
		stop("the dataset does not seem to contain the required variable var.isoscape.value")
	if(is.null(data$n.isoscape.value))
		stop("the dataset does not seem to contain the required variable n.isoscape.value")
	if(any(data$var.isoscape.value<=0))
		stop("the dataset seem to contain null or negative value for var.isoscape.value")
	if(!is.null(data$stationID))
		data$stationID <- factor(data$stationID)
	else
		data$stationID <- factor(1:nrow(data))
	data$lat.abs <- abs(data$lat)
	data$lat.2 <- data$lat^2
	data$long.2 <- data$long^2
	return(data)
}


.PrepareFormula <- function(base.formula, fix, rand, rand.p){
	## This function should not be called by the user but is itself called by other functions.
	## It prepares formulas for the fitting procedures.
	if(fix$elev)
		base.formula <- paste(base.formula, "+ elev")
	if(fix$lat.abs)
		base.formula <- paste(base.formula, "+ lat.abs")
	if(fix$lat.2)
		base.formula <- paste(base.formula, "+ lat.2")
	if(fix$long)
		base.formula <- paste(base.formula, "+ long")
	if(fix$long.2)
		base.formula <- paste(base.formula, "+ long.2")
	if(rand$uncorr & rand.p=="lambda") 	
		base.formula <- paste(base.formula, "+ (1|stationID)")
	if(rand$spatial)
		base.formula <- paste(base.formula, "+ Matern(1|long + lat)")

	return(base.formula)
}


print.isofit <- function(x, ...) {
  print(summary(x))
  return(invisible(NULL))
}


summary.isofit <- function(object, ...) {
  cat("\n")
  cat("##################################################", "\n")
  cat("### spaMM summary of the fit of the mean model ###", "\n")
  cat("##################################################", "\n")
  cat("\n")
  print(summary.HLfit(object$mean.fit))
  cat("\n")
  cat("\n")
  cat("#################################################################", "\n")
  cat("### spaMM summary of the fit of the residual dispersion model ###", "\n")
  cat("#################################################################", "\n")
  cat("\n")
  print(summary.HLfit(object$disp.fit))
  cat("\n")
  cat(paste("[models fitted with spaMM version ", object$mean.fit$spaMM.version, "]", sep=""), "\n")
  cat("\n")
  return(invisible(NULL))
}


