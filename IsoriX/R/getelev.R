#' Download an elevation raster from internet
#' 
#' The function \code{getelev} downloads an elevation raster from internet. It
#' is a wrapper that 1) calls the function [elevatr::get_elev_raster] to
#' download the data and 2) saves the downloaded raster on the hard drive (so
#' that you don't have to keep downloading the same file over and over again).
#' The file saved on the disk is a *.tif file which you can directly read using
#' the function [raster::raster].
#'
#' By default (and to keep with the spirit of the former implementations of
#' \code{getelev} in IsoriX, which did not rely on [elevatr::elevatr]), an
#' elevation raster of the whole world is downloaded with a resolution
#' correspond to ca. 0.6 km2 per raster cell. You can increase the resolution by
#' increasing the value of the argument \code{z}. You can also restrict the area
#' to be downloaded using the arguments \code{lat} and \code{long}.
#' 
#' Note that when using [prepraster] you will be able to reduce the resolution
#' and restrict the boundaries of this elevation raster, but you won't be able
#' to increase the resolution or expend the boundaries. As a consequence, it is
#' probably a good idea to overshoot a little when using \code{getelev} and
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
#' @param path A \var{string} indicating where to store the file on the hard
#'   drive (without the file name!)
#' @param filename A \var{string} indicating the name under which the file must
#'   be stored (default = `elevation.tif`)
#' @param z An \var{integer} between 1 and 14 indicating the resolution of the
#'   file do be downloaded (1 = lowest, 14 = highest; default = 5)
#' @param long A \var{numeric vector} of length 2 indicating the minimum and
#'   maximum WGS84 longitudes of the raster.
#' @param lat A \var{numeric vector} of length 2 indicating the minimum and
#'   maximum WGS84 latitude of the raster.
#' @param override_size_check A \var{logical} indicating whether or not to
#'   override size checks (default = \var{FALSE})
#' @param overwrite A \var{logical} indicating if an existing file should be
#'   re-downloaded
#' @param verbose A \var{logical} indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is \code{TRUE} if users use an interactive R
#'   session and \code{FALSE} otherwise.
#' @param ... Other parameters to be passed to the function
#'   [elevatr::get_elev_raster]
#'
#' @return This function returns the full path where the file has been stored
#' @examples
#' 
#' ## To download the high resolution
#' ## raster in your current working
#' ## directory, just type:
#' ## getelev()
#' 
#' @export
getelev <- function(path = NULL,
                    filename = "elevWorld_z5.tif",
                    z = 5,
                    long = c(-180, 180),
                    lat = c(-90, 90),
                    override_size_check = FALSE,
                    overwrite = FALSE,
                    verbose = interactive(),
                    ...
                    ) {

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
                  "so it won't be downloaded again unless you set the argument overwrite to TRUE\n"
    )
    )
  } else {
  
    if (verbose) print("Downloading and formating the elevation raster... (be patient)")
    elev <- elevatr::get_elev_raster(location = data.frame(long = long, lat = lat),
                                     z = z,
                                     prj = "+proj=longlat +datum=WGS84 +no_defs",
                                     clip = "bbox",
                                     override_size_check = override_size_check,
                                     verbose = verbose,
                                     ...)
    
    if (verbose) print("Writing the elevation raster on the disk...")
    raster::writeRaster(elev, filename = complete_path, overwrite = overwrite)
    if (verbose) print("Done.")
  }
  
  message("you can load your elevation raster as follows:")
  message(paste0("elev_raster <- raster::raster('", complete_path, "')"))
  
  return(invisible(complete_path))
}


#' Download rasters of monthly precipitation from internet
#' 
#' The function \code{getprecip} allows for the download of rasters of monthly
#' precipitation from internet. It downloads the "precipitation (mm) WorldClim
#' Version 2.1" at a spatial resolution of 30 seconds (~1 km2). After download,
#' the function also unzip the file. The function \code{getprecip} uses the
#' generic function \code{downloadfile} that can also be used to download
#' directly other files. This raster needs further processing with the function
#' \code{\link{prepcipitate}}. It can then be used to predict annual averages
#' precipitation weighted isoscapes with the function
#' \code{\link{isomultiscape}}.
#' 
#' In the argument "path" is not provided, the file will be stored in the 
#' current working directory. The functions can create new directories, so you 
#' can also indicate a new path. The integrity of the elevation raster is tested
#' after a call to \code{getprecip}. In case of corruption, try downloading the
#' file again, specifying overwrite = TRUE to overwrite the corrupted file.
#' 
#' @inheritParams getelev
#' @param verbose A \var{logical} indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is \code{TRUE} if users use an interactive R
#'   session and \code{FALSE} otherwise. If a \var{numeric} is provided instead,
#'   additional information about the download will be provided if the number is
#'   greater than 1.
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
  outpath <- paste0(path, "wc2.1_30s_prec")
  utils::unzip(path_to_zip, exdir = outpath)
  
  if (verbose > 0) {
    print("unzipping done!", quote = FALSE)
    print(paste("The files can be found in the folder", outpath), quote = FALSE)
  }
  
  return(invisible(NULL))
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
#' @param address A \var{string} indicating the address of the file on internet
#' @param filename A \var{string} indicating the name under which the file must
#'   be stored
#' @param md5sum A \var{string} indicating the md5 signature of the valid file
#'   as created with [tools::md5sum()]
#' @param verbose A \var{logical} indicating whether information about the
#'   progress of the procedure should be displayed or not while the function is
#'   running. By default verbose is \code{TRUE} if users use an interactive R
#'   session and \code{FALSE} otherwise. If a \var{numeric} is provided instead,
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
    utils::download.file(address, destfile = complete_path, mode = "wb")
  }
  
  ## Checking MD5sum
  if (!is.null(md5sum)) {
    if (tools::md5sum(complete_path) == md5sum) {
      print("the file seems OK (md5sums do match)", quote = FALSE)
    } else {
      warning("the file seems to be corructed (md5sums do not match). Try to download it again setting the argument overwrite to TRUE.", quote = FALSE)
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

