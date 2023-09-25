#' Download an elevation raster from internet
#' 
#' The function `getelev` downloads an elevation raster from internet. It
#' is a wrapper that 1) calls the function [elevatr::get_elev_raster] to
#' download the data and 2) saves the downloaded raster on the hard drive (so
#' that you don't have to keep downloading the same file over and over again).
#' The file saved on the disk is a *.tif file which you can directly read using
#' the function [terra::rast].
#'
#' By default (and to keep with the spirit of the former implementations of
#' `getelev` in IsoriX, which did not rely on [elevatr::elevatr]), an
#' elevation raster of the whole world is downloaded with a resolution
#' correspond to ca. 0.6 km2 per raster cell. You can increase the resolution by
#' increasing the value of the argument `z`. You can also restrict the area
#' to be downloaded using the arguments `long_min`, `long_max`, `lat_min` &
#' `lat_max`.
#' 
#' Note that when using [prepraster] you will be able to reduce the resolution
#' and restrict the boundaries of this elevation raster, but you won't be able
#' to increase the resolution or expend the boundaries. As a consequence, it is
#' probably a good idea to overshoot a little when using `getelev` and
#' download data at a resolution slightly higher than you need and for a extent
#' larger than your data.
#' 
#' You can customise further what you download by using other parameters of
#' [elevatr::get_elev_raster] (via the elipsis `...`).
#' 
#' Please refer to the documentation of
#' [elevatr::get_elev_raster] for information on the sources and follows link in
#' there to know how to cite them.
#' 
#' @inheritParams prepsources
#' @param file A *string* indicating where to store the file on the hard
#'   drive (default = `~/elevation_world_z5.tif`)
#' @param z An *integer* between 1 and 14 indicating the resolution of the
#'   file do be downloaded (1 = lowest, 14 = highest; default = 5)
#' @param margin_pct The percentage representing by how much the area should
#'   extend outside the area used for cropping (default = 5, corresponding to
#'   5%). Set to 0 if you want exact cropping.
#' @param override_size_check A *logical* indicating whether or not to
#'   override size checks (default = `FALSE`)
#' @param overwrite A *logical* indicating if an existing file should be
#'   re-downloaded
#' @param Ncpu An *integer* specifying the number of CPU's to use when downloading AWS tiles (default set by global package options).
#' @param verbose A *logical* indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is `TRUE` if users use an interactive R
#'   session and `FALSE` otherwise.
#' @param ... Other parameters to be passed to the function
#'   [elevatr::get_elev_raster]
#'
#' @return This function returns the full path where the file has been stored
#' @examples
#' 
#' ## To download the high resolution
#' ## raster at the default location, just type:
#' ## getelev()
#' 
#' @export
getelev <- function(file = "~/elevation_world_z5.tif",
                    z = 5,
                    long_min = -180,
                    long_max = 180,
                    lat_min = -90,
                    lat_max = 90,
                    margin_pct = 5,
                    override_size_check = FALSE,
                    overwrite = FALSE,
                    Ncpu = getOption_IsoriX("Ncpu"),
                    verbose = interactive(),
                    ...
) {
  
  ## Checking that elevatr is installed
  if (!requireNamespace("elevatr", quietly = TRUE)) {
    stop("You must install the package elevatr for this function to run: `install.packages('elevatr')`")
  }
  
  ## Turning path into canonical form
  ## (this avoids the problem of using the wrong slashes and so on)
  file <- normalizePath(file, mustWork = FALSE)
  
  path <- dirname(file)
  
  if (path == ".") {
    path <- getwd()
  } else if (!dir.exists(path)) {
    ## Create directory if missing
    if (verbose > 0) {
      print("(the folder you specified does not exist and will therefore be created)", quote = FALSE)
    }
    dir.create(path, recursive = TRUE)
  }
  
  ## Applying margin_pct
  if (margin_pct != 0) {
    
    margin_long_extra <- (long_max - long_min) * margin_pct/100
    margin_lat_extra  <- (lat_max - lat_min) * margin_pct/100
    
    if (long_min > -180) {
      long_min <- max(c(-180, long_min - margin_long_extra))
    }
    if (long_max < 180) {
      long_max <- min(c(180, long_max + margin_long_extra))
    }
    if (lat_min > -90) {
      lat_min <- max(c(-90, lat_min - margin_lat_extra))
    }
    if (lat_max < 90) {
      lat_max <- min(c(90, lat_max + margin_lat_extra))
    }
  }
  
  ## Conditional file download
  if (file.exists(file) & !overwrite) {
    message(paste("the file", basename(file), "is already present in", path,
                  "so it won't be downloaded again unless you set the argument overwrite to TRUE\n"
    )
    )
  } else {
    
    if (verbose) print("Downloading and formating the elevation raster... (be patient)")
    elev <- elevatr::get_elev_raster(locations = data.frame(x = c(long_min, long_max),
                                                            y = c(lat_min, lat_max)),
                                     z = z,
                                     prj = "+proj=longlat +datum=WGS84 +no_defs",
                                     clip = "bbox",
                                     override_size_check = override_size_check,
                                     ncpu = Ncpu,
                                     verbose = verbose,
                                     ...)
    
    if (verbose) print("Writing the elevation raster on the disk...")
    terra::writeRaster(elev, filename = file, overwrite = overwrite)
    if (verbose) print("Done.")
  }
  
  message("you can load your elevation raster as follows:")
  message(paste0("elev_raster <- terra::rast('", file, "')"))
  
  return(invisible(file))
}


#' Download rasters of monthly precipitation from internet
#' 
#' The function `getprecip` allows for the download of rasters of monthly
#' precipitation from internet. It downloads the "precipitation (mm) WorldClim
#' Version 2.1" at a spatial resolution of 30 seconds (~1 km2). After download,
#' the function also unzip the file. The function `getprecip` uses the
#' generic function `downloadfile` that can also be used to download
#' directly other files. This raster needs further processing with the function
#' [prepcipitate]. It can then be used to predict annual averages
#' precipitation weighted isoscapes with the function
#' [isomultiscape].
#' 
#' In the argument "path" is not provided, the file will be stored in the 
#' current working directory. The functions can create new directories, so you 
#' can also indicate a new path. The integrity of the elevation raster is tested
#' after a call to `getprecip`. In case of corruption, try downloading the
#' file again, specifying overwrite = TRUE to overwrite the corrupted file.
#' 
#' @inheritParams getelev
#' @param path A *string* indicating where to store the file on the hard
#'   drive (without the file name!). Default = current directory.
#' @param verbose A *logical* indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is `TRUE` if users use an interactive R
#'   session and `FALSE` otherwise. If a *numeric* is provided instead,
#'   additional information about the download will be provided if the number is
#'   greater than 1.
#'   
#' @return This function returns the path of the folder where the files have
#'   been stored
#'
#' @source \url{https://worldclim.org/data/worldclim21.html}
#' @examples
#' 
#' ## To download the monthly precipitation
#' ## in your current working
#' ## directory, just type:
#' ## getprecip()
#' ## Mind that the file weights ca. 1GB!
#' @export
getprecip <- function(path = NULL,
                      overwrite = FALSE,
                      verbose = interactive()
) {
  
  ## check options
  old_opts <- options()
  if (options()$timeout == 60) {
    options(timeout = 600)
    message("You were using R default settings of a maximum of 60s per download, this is unsufficient for downloading the large file. IsoriX temporarily increased this limit to 600s. If this is not sufficient, please call `options(timeout = XX)` with XX the number of seconds you want to allow R waiting for the download to be completed before crashing. After this, rerun `getprecip()`.")
  } else if (options()$timeout < 600) {
    message("You are using a custom value of timeout lower than 600s. This could be unsufficient for downloading the large file. If the download crashes as a result, please call `options(timeout = XX)` with XX the number of seconds you want to allow R waiting for the download to be completed before crashing. After this, rerun `getprecip()`.")
  }
  on.exit(options(old_opts))
  
  ## Use current directory if path is missing
  if (is.null(path)) {
    path <- getwd()
  }
  
  ## Normalise path the remove last slash
  path <- base::normalizePath(path, mustWork = FALSE)
  
  ## Define web address and file name
  address_precip <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_prec.zip"
  filename_precip <- "wc2.1_30s_prec.zip"
  
  ## Define md5sum
  ## (created with tools::md5sum("wc2.1_30s_prec.zip"))
  md5sum_precip <- "cc100350d034883c9e925c903fb3c7c3"
  
  ## Download and check file
  path_to_zip <- downloadfile(address = address_precip,
                              filename = filename_precip,
                              path = path,
                              overwrite = overwrite,
                              md5sum = md5sum_precip,
                              verbose = verbose
  )
  
  ## Unzip the file
  if (verbose > 0) {
    print("unzipping in progress...", quote = FALSE)
  }
  outpath <- paste0(path, "/wc2.1_30s_prec")
  utils::unzip(path_to_zip, exdir = outpath)
  
  if (verbose > 0) {
    print("unzipping done!", quote = FALSE)
    print(paste("The files can be found in the folder", outpath), quote = FALSE)
  }
  
  return(invisible(outpath))
}


#' Download files and check their binary integrity
#'
#' This function is the internal function used in IsoriX to download the large
#' files from internet and it could be useful to download anything from within
#' R. We created this function to make sure that the downloaded files are valid.
#' Downloads can indeed result in files that are corrupted, so we tweaked the
#' options to reduce this possibility and the function runs a check if the
#' signature of the file is provided to the argument `md5sum`.
#'
#' @note Users should directly use the function [getelev()] and
#'   [getprecip()].
#'
#' @inheritParams getelev
#' @inheritParams getprecip
#' @param address A *string* indicating the address of the file on internet
#' @param filename A *string* indicating the name under which the file must
#'   be stored
#' @param md5sum A *string* indicating the md5 signature of the valid file
#'   as created with [tools::md5sum()]
#' @param verbose A *logical* indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is `TRUE` if users use an interactive R
#'   session and `FALSE` otherwise. If a *numeric* is provided instead,
#'   additional information about the download will be provided if the number is
#'   greater than 1.
#'
#' @return The complete path of the downloaded file (invisibly)
#'
#' @export
#'
#' @seealso [getelev()], [getprecip()]
#' 
#' 
downloadfile <- function(address = NULL, filename = NULL, path = NULL,
                         overwrite = FALSE, md5sum = NULL, verbose = interactive()
) {
  
  if (verbose > 0) {
    print(paste("the function attempts to download", filename, "from internet"), quote = FALSE)
  }
  
  ## Change internet options to display more information
  opt_ori <- options()$internet.info
  if (verbose > 1) options(internet.info = 1)
  
  ## Use current directory if path is missing
  if (is.null(path)) {
    path <- getwd()
  }
  
  ## Turning path into canonical form
  ## (this avoids the problem of having terminal slash or not)
  path <- normalizePath(path, mustWork = FALSE)
  
  ## Create directory if missing
  if (!dir.exists(path)) {
    if (verbose > 0) {
      print("(the folder you specified does not exist and will therefore be created)", quote = FALSE)
    }
    dir.create(path, recursive = TRUE)
  }
  
  ## Conditional file download
  complete_path <- paste(path, filename, sep = "/")
  if (file.exists(complete_path) & !overwrite) {
    message(paste("the file", filename, "is already present in", path,
                  "so it won't be downloaded again unless you set the argument overwrite to TRUE"
    )
    )
  } else {
    time <- system.time(utils::download.file(address, destfile = complete_path, mode = "wb"))
    message(paste0("The download lasted ", time[["elapsed"]], "s"))
  }
  
  ## Checking MD5sum
  if (!is.null(md5sum)) {
    if (tools::md5sum(complete_path) == md5sum) {
      print("the file seems OK (md5sums do match)", quote = FALSE)
    } else {
      warning("the file seems to be corructed (md5sums do not match). Try to download it again setting the argument overwrite to TRUE.")
    }
  }
  
  ## Display outcome
  if (verbose > 0) {
    print(paste("the file", filename, "is stored in the folder", path), quote = FALSE)
  }
  
  ## Restore original internet options
  options(internet.info = opt_ori)
  
  return(invisible(complete_path))
}

