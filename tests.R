library(IsoriX)
setwd("./local_trials/")

for(bool in c(FALSE, TRUE)) {
cat("######################################################################")
cat("\n")
  time1 <- system.time({
    system.time(example(CalibDataBat, run.dontrun=bool))
    system.time(example(AssignDataBat, run.dontrun=bool))
    system.time(example(isopalette2, run.dontrun=bool))
    system.time(example(CountryBorders, run.dontrun=bool))
    system.time(example(OceanMask, run.dontrun=bool))
    system.time(example(ElevRaster, run.dontrun=bool))
    system.time(example(GNIPDataDE, run.dontrun=bool))
    system.time(example(queryGNIP, run.dontrun=bool))
    system.time(example(isofit, run.dontrun=bool))
    system.time(example(isomultifit, run.dontrun=bool))
    system.time(example(relevate, run.dontrun=bool))
    system.time(example(isoscape, run.dontrun=bool))
    system.time(example(isomultiscape, run.dontrun=bool))
    system.time(example(isosim, run.dontrun=bool))
    system.time(example(calibfit, run.dontrun=bool))
    system.time(example(isofind, run.dontrun=bool))
  })
print(paste("All examples have run in", time1[1], "seconds"))

}
