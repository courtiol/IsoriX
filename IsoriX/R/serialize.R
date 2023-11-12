#' Save and read objects produced by IsoriX into RDS files
#' 
#' Because files created with IsoriX contain [`terra::SpatRaster`] and
#' [`terra::SpatVector`] objects, they cannot be saved using [`base::saveRDS`] or
#' [`base::save`] functions. The reason is that objects created with [terra] point
#' to data stored in memory which are not contained in the R objects themselves.
#' Adapting the approach implemented in the [terra] package, we provide a
#' wrapper for [`base::saveRDS`] and [`base::readRDS`] functions, which allows one
#' to save and read objects produced with IsoriX by simply using `saveRDS()` and
#' `readRDS()`.
#' 
#' [`base::saveRDS`] and [`base::readRDS`] are standard S3 functions. So in
#' order to be able to have a specific behaviour for objects produced with
#' IsoriX, we turned `saveRDS` and `readRDS` into S4 generics that dispatch both
#' S3 and S4 methods (see [Methods_for_S3]). The S3 implementation is consistent
#' with the rest of the package and presents all usual benefits associated with
#' S3 methods (e.g. simple access to the code). The S4 implementation makes
#' IsoriX methods compatible with the use of [`terra::saveRDS`] and
#' [`terra::readRDS`].
#'
#' @param object (definition copied from  [`base::readRDS`]:) R object to serialize.
#' @param file (definition copied from  [`base::readRDS`]:) a connection or the name of the file where the R object is saved to or read from.
#' @param ascii (definition copied from  [`base::readRDS`]:) a logical. If `TRUE` or `NA`, an ASCII representation is written; otherwise (default), a binary one is used. See the comments in the help for [`base::save`].
#' @param version (definition copied from  [`base::readRDS`]:) the workspace format version to use. `NULL` specifies the current default version (3). The only other supported value is `2`, the default from R 1.4.0 to R 3.5.0.
#' @param compress (definition copied from  [`base::readRDS`]:) a logical specifying whether saving to a named file is to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used. Ignored if file is a connection.
#' @param refhook	(definition copied from  [`base::readRDS`]:) a hook function for handling reference objects.
#' 
#' @name serialize
#' @keywords saving
#' 
#' @return 
#' For `saveRDS`, `NULL` invisibly.
#' 
#' For `readRDS`, an R object.
#'
#' @examples
#' if (getOption_IsoriX("example_maxtime") > 30) {
#' 
#' ## We prepare the data
#' GNIPDataDEagg <- prepsources(data = GNIPDataDE)
#' 
#' ## We fit the models
#' GermanFit <- isofit(data = GNIPDataDEagg,
#'                     mean_model_fix = list(elev = TRUE, lat_abs = TRUE))
#' 
#' ## We build the isoscapes
#' GermanScape <- isoscape(raster = ElevRasterDE, isofit = GermanFit)
#' 
#' ## Saving as RDS
#' filename <- tempfile(fileext = ".rds") # or whatever names you want
#' saveRDS(GermanScape, file = filename)
#' 
#' ## Reading RDS
#' GermanScape2 <- readRDS(filename)
#' GermanScape2
#' 
#' ## Saving data.frame object as RDS
#' filename2 <- tempfile(fileext = ".rds") # or whatever names you want
#' saveRDS(iris, file = filename2)
#' 
#' ## Reading RDS containing data.frame
#' iris2 <- readRDS(filename2) 
#' iris2
#' 
#' ## Saving terra object as RDS
#' filename3 <- tempfile(fileext = ".rds") # or whatever names you want
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' saveRDS(r, file = filename3)
#' 
#' ## Reading RDS containing terra object
#' r2 <- readRDS(filename3) 
#' r2
#' 
#' }
#'
NULL


# Defining S4 generics -----------------------------------------------------

#' @describeIn serialize S4 generic function for `saveRDS`
#' 
#' @export
#' 
saveRDS <- function(object, file = "", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL) UseMethod("saveRDS")


#' @describeIn serialize S4 generic function for `readRDS`
#' 
#' @export
#' 
readRDS <- function(file, refhook = NULL) UseMethod("readRDS")


# Defining omnibus function --------------------------------------------------

#' @describeIn serialize S3 function to save IsoriX objects into a RDS file
#' 
#' @export
#' 
saveRDS_IsoriX <- function(object, file = "", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL) {
  #message("Saving RDS using IsoriX method")
  if (!is.null(object$isoscapes)) {
    if (inherits(object$isoscapes, "SpatRaster")) {
      object$isoscapes <- terra::wrap(object$isoscapes)
    } else {
      stop("Saving situation not implemented yet. Please contact the package maintainer.")
    }
  }
  if (!is.null(object$group)) {
    object$group <- lapply(object$group, \(x) {
      if (inherits(x, "SpatRaster")) {
        terra::wrap(x)
      } else {
        stop("Saving situation not implemented yet. Please contact the package maintainer.")
      }
    })
  }
  if (!is.null(object$sample)) {
    object$sample <- lapply(object$sample, \(x) {
      if (inherits(x, "SpatRaster")) {
        terra::wrap(x)
      } else {
        stop("Saving situation not implemented yet. Please contact the package maintainer.")
      }
    })
  }
  if (!is.null(object$sp_points)) {
    object$sp_points <- lapply(object$sp_points, \(x) {
      if (inherits(x, "SpatVector")) {
        terra::wrap(x)
      } else {
        stop("Saving situation not implemented yet. Please contact the package maintainer.")
      }
    })
  }
  base::saveRDS(object, file = file, ascii = ascii, version = version, compress = compress, refhook = refhook)
}


# Defining S3 methods -----------------------------------------------------

#' @describeIn serialize S3 method to save an `ISOSCAPE` object into a RDS file
#' 
#' @method  saveRDS ISOSCAPE
#' @exportS3Method saveRDS ISOSCAPE
#' @export
#' 
saveRDS.ISOSCAPE <- function(object, file = "", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL) {
  saveRDS_IsoriX(object, file = file, ascii = ascii, version = version, compress = compress, refhook = refhook)
}

#' @describeIn serialize S3 method to save a `CALIBFIT` object into a RDS file
#' 
#' @method  saveRDS CALIBFIT
#' @exportS3Method saveRDS CALIBFIT
#' @export
#' 
saveRDS.CALIBFIT <- function(object, file = "", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL) {
  saveRDS_IsoriX(object, file = file, ascii = ascii, version = version, compress = compress, refhook = refhook)
}

#' @describeIn serialize S3 method to save an `ISOFIND` object into a RDS file
#' 
#' @method  saveRDS ISOFIND
#' @exportS3Method saveRDS ISOFIND
#' 
saveRDS.ISOFIND <- function(object, file = "", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL) {
  saveRDS_IsoriX(object, file = file, ascii = ascii, version = version, compress = compress, refhook = refhook)
}


#' @describeIn serialize S3 method to read an object produced with IsoriX (or other) stored in a RDS file
#' 
#' @method readRDS character
#' @exportS3Method readRDS character
#' @export
#' 
readRDS.character <- function(file, refhook = NULL) {
  #message("Reading RDS using IsoriX wrapper")
  object <- base::readRDS(file = file, refhook = refhook)
  if (inherits(object, "PackedSpatRaster") || inherits(object, "PackedSpatVector")) {
    return(terra::unwrap(object))
  }
  if (!inherits(object, "ISOSCAPE") && !inherits(object, "CALIBFIT") && !inherits(object, "ISOFIND")) {
    return(object)
  }
  if (!is.null(object$isoscapes)) {
    if (inherits(object$isoscapes, "PackedSpatRaster")) {
      object$isoscapes <- terra::unwrap(object$isoscapes)
    } else {
      stop("Saving situation not implemented yet. Please contact the package maintainer.")
    }
  }
  if (!is.null(object$sp_points)) {
    object$sp_points <- lapply(object$sp_points, \(x) {
      if (inherits(x, "PackedSpatVector")) {
        terra::unwrap(x)
      } else {
        stop("Saving situation not implemented yet. Please contact the package maintainer.")
      }
    })
  }
  object
}


# Defining S4 methods -----------------------------------------------------

#' @describeIn serialize S4 method to save an `ISOSCAPE` object into a RDS file
#' 
#' @method saveRDS ISOSCAPE
#' @export
#' 
setMethod("saveRDS", signature(object = "ISOSCAPE"), saveRDS.ISOSCAPE)


#' @describeIn serialize S4 method to save an `CALIBFIT` object into a RDS file
#' 
#' @method saveRDS CALIBFIT
#' @export
#' 
setMethod("saveRDS", signature(object = "CALIBFIT"), saveRDS.CALIBFIT)


#' @describeIn serialize S4 method to save an `ISOFIND` object into a RDS file
#' 
#' @method saveRDS ISOFIND
#' @export
#' 
setMethod("saveRDS", signature(object = "ISOFIND"), saveRDS.ISOFIND)


#' @describeIn serialize S4 method to read an object produced with IsoriX (or other) stored in a RDS file
#' 
#' @method readRDS character
#' @export
#' 
setMethod("readRDS", signature(file = "character"), readRDS.character)
