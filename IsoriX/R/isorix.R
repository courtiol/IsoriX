Isorix <- function(
  assign.data,
  isoscape,
  calibfit,
  mask=NULL,
  verbose=interactive()
) {
  .Defunct("isofind")
}

isofind <- function(
	assign.data,
	isoscape,
	calibfit,
	mask=NULL,
	verbose=interactive()
	) {

	### WE COMPUTE THE TEST STATISTIC
	if(verbose)
		print("computing the test statistic and its variance...")

	names.layers <- gsub(" ", "_", as.character(assign.data$animalID))

	time <- system.time({
		## we predict the isotopic value at origin location	
		assign.data$mean.origin <-
			(assign.data$tissue.value - calibfit$param["intercept"])/calibfit$param["slope"]

		## we create individual rasters containing the test statistics
		list.stat.layers <- sapply(1:nrow(assign.data),
			function(i) assign.data$mean.origin[i]-isoscape$isoscape$mean)
		names(list.stat.layers) <- names.layers
		stat.stack <- stack(list.stat.layers)


		### WE COMPUTE THE VARIANCE OF THE TEST

		## we compute fixedVar
		X <- cbind(1, assign.data$mean.origin)
		fixedVar <- rowSums(X * (X %*% calibfit$fixefCov)) ## = diag(X %*% calibfit$fixefCov %*% t(X))

		## we create individual rasters containing the variance of the test statistics
		list.varstat.layers <- sapply(1:nrow(assign.data),
			function(i)
				isoscape$isoscape$mean.predVar +
				calibfit$calib.fit$phi/calibfit$param["slope"]^2 +
				fixedVar[i]/calibfit$param["slope"]^2 +
				0 ## ToDo compute fourth variance term
		)

		names(list.varstat.layers) <- names.layers
		varstat.stack <- stack(list.varstat.layers)


		### WE COMPUTE THE INDIVIDUAL LOG P-VALUE SURFACES
		if(verbose)
			print("running the assignment test...")

		## we initialize the stack
		logpv.stack <- raster(varstat.stack)

		## we create individual rasters containing the p-values of the test
		for(animalID in names.layers) {
			name.layer <- paste("logpv.stack$", animalID, sep="")
			expr.to.run <- paste(name.layer,
				"<- .AssignTest(values(stat.stack[[animalID]]), values(varstat.stack[[animalID]]))")
			eval(parse(text=expr.to.run))
		}

		### WE COMBINE INDIVIDUAL SURFACES USING FISHER'S METHOD
		if(verbose)
			print("combining individual assignments...")

		group.pv <- calc(logpv.stack, .FisherMethod)
	})  ## end of system.time

	## display time
	time <- round(as.numeric((time)[3]))
	if(verbose) {
		print(paste("assignements for all", nrow(assign.data), "organisms have been computed in", time, "sec."))
	}

	## remove log scale
	pv.stack <- exp(logpv.stack)
	names(pv.stack) <- names.layers
	rm(logpv.stack)

	## replacing values by zeros if they fall in the mask (e.g. in water)
	if(!is.null(mask)) {
		if(verbose)
			print("applying the mask...")

		## turn mask into raster with NA inside polygons
		raster.mask <- is.na(rasterize(mask, stat.stack))

		## saving raster names
		names.ind.backup <- names(stat.stack)

		## multiplying rasters by the raster.mask		
		stat.stack <- stat.stack*raster.mask
		names(stat.stack) <- names.ind.backup

		varstat.stack <- varstat.stack*raster.mask
		names(varstat.stack) <- names.ind.backup

		pv.stack <- pv.stack*raster.mask
		names(pv.stack) <- names.ind.backup
		
		group.pv <- overlay(group.pv, raster.mask, fun=prod)
	}


	### RETURNS

	out <- list(
		indiv=list(
			"stat"=stat.stack,
			"stat.var"=varstat.stack,
			"pv"=pv.stack),
		group=list("pv"=group.pv),
		sp.points=list(
			"sources"=isoscape$sp.points$sources,
			"calibs"=calibfit$sp.points$calibs)
	)

	class(out) <- c("isorix", "list")

	# if(save.spatial.files) {
	# 	## export individual p-values surfaces
	# 	writeRaster(
	# 		out$indiv$pv,
	# 		filename=paste(file.prefix.spatial.files, ".asc", sep=""),
	# 		bylayer=TRUE,
	# 		suffix="names",
	# 		overwrite=overwrite.spatial.files
	# 		)

	# 	## export the group p-values surface
	# 	writeRaster(
	# 		out$group$pv,
	# 		filename=paste(file.prefix.spatial.files, "_group", ".asc", sep=""),
	# 		overwrite=overwrite.spatial.files
	# 		)
	# }

	return(out)
}


.AssignTest <- function(stats, vars, log.scale=TRUE) {
	if(!log.scale)
		return(2*(1-pnorm(abs(stats), mean=0, sd=sqrt(vars))))
	log.pva <- pnorm(stats, mean=0, sd=sqrt(vars),
		log.p=TRUE, lower.tail=TRUE)
	log.pvb <- pnorm(stats, mean=0, sd=sqrt(vars),
		log.p=TRUE, lower.tail=FALSE)
	log.pv <- log(2) + apply(cbind(log.pva, log.pvb), 1, min)
	return(log.pv)
}


.FisherMethod <- function(logpv) {
	fisher.stat <- -2*sum(logpv)
	df <- 2*length(logpv)
	pv <- pchisq(q=fisher.stat, df=df, lower.tail=FALSE)
	return(pv)
}


print.isorix <- function(x, ...) {
  print(summary(x))
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
