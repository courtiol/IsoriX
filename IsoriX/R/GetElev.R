GetElev <- function (
	path=NULL,
	address=NULL,
	overwrite=FALSE,
	verbose=interactive()
	) {
	
	if(verbose)
		print("this function attempts to download a high resolution elevation raster from internet")

	## Use our address if path missing
	if(is.null(address)) address <- "http://62.141.164.7/download/elevation_raster_gmted2010_30mn.tif"

	## Use current directory if path is missing
	if(is.null(path)) path <- getwd()

	## Create directory if missing
	if(!dir.exists(path))
		dir.create(path, recursive=TRUE)

	## Guess file name
	strings <- strsplit(address, split="/", fixed=TRUE)[[1]]
	filename <- strings[length(strings)]

	if(filename %in% dir(path) & !overwrite)
		message(paste("the file", filename,"is already present in", path, 
			"so it won't be downloaded again unless
			you set the argument overwrite to TRUE"))
	else
		download.file(address,
			destfile=paste(path, filename, sep=""))

	if(verbose) {
		print(paste("the raster", filename, "is stored in the folder", path))
	}

	return(invisible(NULL))
}

