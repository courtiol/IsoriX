plot.isoscape <-
  function(
    x,
    which="mean",
    sources=list(draw=TRUE, cex=0.5, pch=2, lwd=1, col="red"),
    borders=list(borders=NULL, lwd=0.5, col="black"),
    mask=   list(mask=NULL, lwd=0, col="black", fill="black"),
    palette=terrain.colors(20),
    plot=TRUE,
    ... ## we cannot remove the dots because of the S3 export...
  ) {
    
    simu <- "isosim" %in% class(x)
    
    ## complete input with default setting
    .CompleteArgs(plot.isoscape)
    
    ## checking the inputs
    if (length(palette)<2) {
      stop("wrong palette, more colours needed")
    }
    
    if (("isosim" %in% class(x))) {
      if (sources$draw) {
        sources$draw <- FALSE
        message("you asked to plot sources, but it does not make sense for simulations as each raster cell is a source. The argument 'plot.sources' was thus considered to be FALSE")
      }
      if (!(which %in% c("mean", "disp"))) {
        stop("for simulated data, the argument 'which' must be 'mean' or 'disp'")
      }
    }
    
    if (("isofit" %in% class(x)) & !(which %in% 
        c("mean", "mean.predVar", "mean.residVar", "mean.respVar",
         "disp", "disp.predVar", "disp.residVar", "disp.respVar"))) {
      stop("argument 'which' unknown")
    }
    
    ## define y.title
    simu.title <- ""
    if (simu) simu.title <- "simulated"
    
    ## create the levelplot
    ##	note the use of bquote() which contrary to expression(paste())
    ##  allows for the evaluation of arguments.
    ##  (the stars are used to remove spaces)
    map <- levelplot(
      x$isoscape[[which]],
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


plot.isorix <- function(
	x,
	who="group",
	what="pv",
	cutoff= list(draw=TRUE, level=0.05, col="#909090"),
	sources=list(draw=TRUE, cex=0.5, pch=2, lwd=1, col="red"),
	calib=  list(draw=TRUE, cex=0.5, pch=4, lwd=1, col="blue"),
	borders= list(borders=NULL, lwd=0.5, col="black"),
	mask=    list(mask=NULL, lwd=0, col="black", fill="black"),
	mask2=   list(mask=NULL, lwd=0, col="purple", fill="purple"),
	palette=rev(terrain.colors(100)),
	plot=TRUE,
	... ## we cannot remove the dots because of the S3 export...
	) {

	## complete input with default setting
	.CompleteArgs(plot.isorix)
	
	## checking the inputs
	if (length(palette) < 2) {
		stop("wrong palette, more colors needed")  
	}
  
	## adding colour for non assignment
	if (cutoff$level > 0) {
		palette <- c(cutoff$col, palette)
	}
  
	## changing cutoff level to null when we don't want to draw the cutoff
	if (what!="pv" | !cutoff$draw) {
		cutoff$level <- 0
	}
  
	## create the main plot(s)
	splits <- seq(0, 1, length=length(palette))

	if ("group" %in% who) {
		map <- levelplot(x$group$pv * (x$group$pv > cutoff$level),
					maxpixels=4e6, margin=FALSE, at=splits,
					col.regions=palette, main="Group assignment")
	} else {
	  main.title <- if (length(who)==1) names(x$indiv[[what]][[who]]) else NULL
		map <- levelplot(x$indiv[[what]][[who]] * (x$indiv$pv[[who]] > cutoff$level),
					maxpixels=4e6, margin=FALSE, at=splits, col.regions=palette,
					main=main.title)
  }
		
	## create the additional plot(s)
	decor <- .BuildAdditionalLayers(x=x, sources=sources,
		calib=calib, borders=borders, mask=mask, mask2=mask2)

	## changing the colour below the threshold
	if (cutoff$level > 0) {
		index <- 1:max(which(map$legend$right$args$key$at < cutoff$level))
		map$legend$right$args$key$col[index] <- cutoff$col
	}

	## we add the legend for the side bar
	## (thanks to Deepayan Sarkar, for solving a device opening hicup at this stage)
	map$legend$right <- list(fun = mergedTrellisLegendGrob,
		args = list(map$legend$right,
			list(fun=textGrob, args=list(label="P-value", rot=90)),
		vertical=FALSE))

	## pilling all layers together
	complete.map <- map+decor$borders.layer+decor$mask.layer+decor$mask2.layer+
	  decor$sources.layer+decor$calib.layer

	if(plot) print(complete.map)
	
	return(invisible(complete.map))

}
