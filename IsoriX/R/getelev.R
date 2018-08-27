#' Download an elevation raster from internet
#' 
#' The function \code{getelev} allows for the download of an elevation raster
#' from internet. It downloads the "Global Multi-resolution Terrain Elevation
#' Data 2010" from our server. The file was orifginally downloaded from: \cr
#' \url{http://topotools.cr.usgs.gov/gmted_viewer/} \cr and converted into a
#' \var{tif} file by us. The function \code{getelev} uses the generic function
#' \code{downloadfile} that can also be used to download directly other files.
#' This raster needs further processing with the function \code{\link{prepelev}}
#' and can then be passed to \code{\link{isoscape}}.
#' 
#' If the argument "path" is not provided, the file will be stored in the
#' current working directory. The function can create new directories, so you
#' can also indicate a new path. If the package \pkg{\link[tools]{tools}} is
#' installed, the integrity of the elevation raster is tested after a call to
#' \code{\link{getelev}}. In case of corruption, try downloading the file again,
#' specifying overwrite = TRUE to overwrite the corrupted file.
#' 
#' @param path A \var{string} indicating where to store the file on the hard
#' drive (without the file name!)
#' @param overwrite A \var{logical} indicating if an existing file should be
#' re-downloaded
#' @param verbose A \var{logical} indicating whether information about the
#' progress of the procedure should be displayed or not while the function is
#' running. By default verbose is \code{TRUE} if users use an interactive R
#' session and \code{FALSE} otherwise. If a \var{numeric} is provided instead,
#' additional information about the download will be provided if the number is
#' greater than 1.
#' @source \url{http://topotools.cr.usgs.gov/gmted_viewer/}
#' @examples
#' 
#' ## To download the high resolution
#' ## raster in your current working
#' ## directory, just type:
#' ## getelev()
#' 
#' @export
getelev <- function(path = NULL,
                    overwrite = FALSE,
                    verbose = interactive()
                    ) {

  ## Define web address and file name
  address_elev <- "http://62.141.164.7/download/gmted2010_30mn.tif"
  filename_elev <- "gmted2010_30mn.tif"
  
  ## Define md5sum
  ## (created with tools::md5sum("gmted2010_30mn.tif"))
  md5sum_elev <- "9fbbb014e2f27299137bae21be31ac7c" 
  
  ## Download and check file
  downloadfile(address = address_elev,
               filename = filename_elev,
               path = path,
               overwrite = overwrite,
               md5sum = md5sum_elev,
               verbose = verbose
               )

  return(invisible(NULL))
}


#' Download rasters of monthly precipitation from internet
#' 
#' The function \code{getprecip} allows for the download of rasters of monthly
#' precipitation from internet. It downloads the "precipitation (mm) WorldClim
#' Version2" at a spatial resolution of 30 seconds (~1 km2). After download, the
#' function also unzip the file. The function
#' \code{getprecip} uses the generic function \code{downloadfile} that can also be
#' used to download directly other files. This raster needs further processing 
#' with the function \code{\link{prepcipitate}}. It can then be used to predict 
#' annual averages precipitation weighted isoscapes with the function \code{\link{isomultiscape}}.
#' 
#' precipitation weighted isoscapes
#' In the argument "path" is not provided, the file will be stored in the 
#' current working directory. The functions can create new directories, so you 
#' can also indicate a new path. The integrity of the elevation raster is tested
#' after a call to \code{getprecip}. In case of corruption, try downloading the
#' file again, specifying overwrite = TRUE to overwrite the corrupted file.
#' 
#' @inheritParams getelev
#' @source \url{http://worldclim.org/version2}
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
  address_precip <- "http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_30s_prec.zip"
  filename_precip <- "wc2.0_30s_prec.zip"
  
  ## Define md5sum
  ## (created with tools::md5sum("wc2.0_30s_prec.zip"))
  md5sum_precip <- "afd435222a328efb4ab9487a3fe0b6d4"
  
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
  outpath <- paste0(path, "wc2.0_30s_prec")
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

