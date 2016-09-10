plot.isorix <- function(
	x,
	who="group",
	what="pv",
	cutoff= list(draw=TRUE, level=0.05, col="#909090"),
	sources=list(draw=TRUE, cex=0.5, pch=2, lwd=1, col="red"),
	calib=  list(draw=TRUE, cex=0.5, pch=4, lwd=1, col="blue"),
	borders=list(borders=NULL, lwd=0.5, col="black"),
	mask=   list(mask=NULL, col="black"),
	palette=rev(terrain.colors(100)),
	plot=TRUE,
	... ## we cannot remove the dots because of the S3 export...
	) {

	## complete input with default setting
	.CompleteArgs(plot.isorix)
	
	## checking the inputs
	if(length(palette) < 2)
		stop("wrong palette, more colors needed")  

	## adding colour for non assignment
	if(cutoff$level > 0)
		palette <- c(cutoff$col, palette)

	## changing cutoff level to null when we don't want to draw the cutoff
	if(what!="pv" | !cutoff$draw)
		cutoff$level <- 0

	## create the main plot(s)
	splits <- seq(0, 1, length=length(palette))

	if("group" %in% who)
		map <- levelplot(x$group$pv * (x$group$pv > cutoff$level),
					maxpixels=4e6, margin=FALSE, at=splits,
					col.regions=palette, main="Group assignment")
	else
		map <- levelplot(x$indiv[[what]][[who]] * (x$indiv$pv[[who]] > cutoff$level),
					maxpixels=4e6, margin=FALSE, at=splits, col.regions=palette)

	## create the additional plot(s)
	decor <- .BuildAdditionalLayers(x=x, sources=sources,
		calib=calib, borders=borders, mask=mask)

	## changing the colour below the threshold
	if(cutoff$level > 0) {
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
	complete.map <- map+decor$borders.layer+decor$mask.layer+decor$sources.layer+decor$calib.layer

	if(plot) print(complete.map)
	
	return(invisible(complete.map))

}
