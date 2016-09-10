plot.isoscape <-
function(
	x,
	which="mean",
	sources=list(draw=TRUE, cex=0.5, pch=2, lwd=1, col="red"),
	borders=list(borders=NULL, lwd=0.5, col="black"),
	mask=   list(mask=NULL, col="black"),
	palette=terrain.colors(20),
	plot=TRUE,
	... ## we cannot remove the dots because of the S3 export...
	) {

	simu <- "isosim" %in% class(x)
	
	## complete input with default setting
	.CompleteArgs(plot.isoscape)

	## checking the inputs
	if(length(palette)<2)
		stop("wrong palette, more colours needed")

	if(("isosim" %in% class(x))) {
		if(sources$draw) {
			sources$draw <- FALSE
			message("you asked to plot sources, but it does not make sense for simulations as each raster cell is a source. The argument 'plot.sources' was thus considered to be FALSE")
		}
		if(!(which %in% c("mean", "disp")))
			stop("for simulated data, the argument 'which' must be 'mean' or 'disp'")
	}

	if(("isofit" %in% class(x)) & !(which %in% c("mean", "mean.predVar", "mean.residVar", "mean.respVar",
		"disp", "disp.predVar", "disp.residVar", "disp.respVar")))
		stop("argument 'which' unknown")

	## define y.title
	simu.title <- ""
	if(simu) simu.title <- "simulated"

	## create the levelplot
	##	note the use of bquote() which contrary to expression(paste())
	##  allows for the evaluation of arguments. The stars are used to remove spaces
	map <- levelplot(
		x$Isoscape[[which]],
		maxpixels=4e6,
		margin=FALSE,
		cuts=length(palette)-1,
		col.regions=palette,
		main=bquote(.(simu.title)~.(sub(".", " ", which, fixed=TRUE))~delta*D[p])
	)
	
	## create the additional plot(s)
	decor <- .BuildAdditionalLayers(x=x, sources=sources,
		calib=NULL, borders=borders, mask=mask)


	complete.map <- map+decor$borders.layer+decor$mask.layer+decor$sources.layer

	## send plot to graphic device
	if(plot)
		print(complete.map)

	## tweak to please codetools::checkUsagePackage('IsoriX', skipWith=TRUE)
	rm(simu.title)

	return(invisible(complete.map))

}
