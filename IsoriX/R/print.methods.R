print.isofit <- function(x, ...) {
	print(summary(x))
	return(invisible(NULL))
}

print.isorix <- print.isoscape <- print.isofit

.NiceRound <- function(x, digits) formatC(round(x, digits), digits=digits, format="f")

print.calibfit <- function(x, ...) {
	cat("\n")
	cat("Fixed effect estimates of the calibration fit", "\n")
	cat("tissue.value = intercept + slope * predicted isoscape.value + SpatialRandom + Error", "\n")
	cat("\n")
	cat(paste("           intercept (+/- SE) =", .NiceRound(x$param["intercept"], 2),
		"+/-",  .NiceRound(sqrt(x$fixefCov["intercept", "intercept"]), 2)), "\n")
	cat(paste("           slope     (+/- SE) =  ", .NiceRound(x$param["slope"], 2),
		"+/-",  .NiceRound(sqrt(x$fixefCov["slope", "slope"]), 2)), "\n")
	cat("\n")
	cat("[for more information, use summary()]", "\n")
	cat("\n")
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


summary.calibfit <- function(object, ...) {
	cat("\n")
	cat("Fixed effect estimates of the calibration fit", "\n")
	print(.NiceRound(object$param, 3), quote=FALSE)
	cat("\n")
	cat("Covariance matrix of fixed effect estimates:", "\n")
	print(.NiceRound(object$fixefCov, 3), quote=FALSE)
	cat("\n")
	cat("#########################################################", "\n")
	cat("### spaMM summary of the fit of the calibration model ###", "\n")
	cat("#########################################################", "\n")
	cat("\n")
	print(summary.HLfit(object$calib.fit))
	cat("\n")
	cat(paste("[model fitted with spaMM version ", object$calib.fit$spaMM.version, "]", sep=""), "\n")
	cat("\n")
	return(invisible(NULL))
}


summary.isorix <- function(object, ...) {
	for(i in names(object)[names(object)!="sp.points"]){
		cat(paste("######### assignment raster(s): '", i, "'"), "\n")
		print(object[[i]])
		cat("\n")
	} 
	return(invisible(NULL))
}
