library(IsoriX)
setwd("./local_trials/")

for(bool in c(FALSE, TRUE)) {
cat("######################################################################")
cat("\n")
	time1 <- system.time({
		system.time(example(calibdata, run.dontrun=bool))
		system.time(example(assigndata, run.dontrun=bool))
		system.time(example(isopalette2, run.dontrun=bool))
		system.time(example(countries, run.dontrun=bool))
		system.time(example(oceanmask, run.dontrun=bool))
		system.time(example(elevraster, run.dontrun=bool))
		system.time(example(GNIPdata, run.dontrun=bool))
		system.time(example(GNIPdataEU, run.dontrun=bool))
		system.time(example(queryGNIP, run.dontrun=bool))
		system.time(example(isofit, run.dontrun=bool))
		system.time(example(relevate, run.dontrun=bool))
		system.time(example(isoscape, run.dontrun=bool))
		system.time(example(isosim, run.dontrun=bool))
		system.time(example(calibfit, run.dontrun=bool))
		system.time(example(isofind, run.dontrun=bool))
	})
print(paste("All examples have run in", time1[1], "seconds"))

}
