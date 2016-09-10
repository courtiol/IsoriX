library(IsoriX)

for(bool in c(FALSE, TRUE)) {
cat("######################################################################")
cat("\n")
	time1 <- system.time({
		system.time(example(calibrationdata, run.dontrun=bool))
		system.time(example(assignmentdata, run.dontrun=bool))
		system.time(example(isorixpalette, run.dontrun=bool))
		system.time(example(worldcountries, run.dontrun=bool))
		system.time(example(oceanmask, run.dontrun=bool))
		system.time(example(elevationrastersmall, run.dontrun=bool))
		system.time(example(GNIP_World, run.dontrun=bool))
		system.time(example(GNIP_Europe, run.dontrun=bool))
		system.time(example(QueryGNIPdata, run.dontrun=bool))
		system.time(example(Isofit, run.dontrun=bool))
		system.time(example(PrepareElevationRaster, run.dontrun=bool))
		system.time(example(Isoscape, run.dontrun=bool))
		system.time(example(Isosim, run.dontrun=bool))
		system.time(example(Calibfit, run.dontrun=bool))
		system.time(example(Isorix, run.dontrun=bool))
	})
print(paste("All examples have run in", time1[1], "seconds"))

}
