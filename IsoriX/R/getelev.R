GetElev <- function (...) {
  .Defunct("getelev")
}

getelev <- function (
  path=NULL,
  overwrite=FALSE,
  verbose=interactive()
) {
  
  ## Define web address and file name
  address.elev <- "http://62.141.164.7/download/gmted2010_30mn.tif"
  filename.elev <- "gmted2010_30mn.tif"
  
  ## Define md5sum
  ## (created with tools::md5sum("gmted2010_30mn.tif"))
  md5sum.elev <- "9fbbb014e2f27299137bae21be31ac7c" 
  
  ## Download and check file
  out <- downloadfile(
    address=address.elev,
    filename=filename.elev,
    path=path,
    overwrite=overwrite,
    md5sum=md5sum.elev,
    verbose=verbose
  )

  return(invisible(out))
}


## The following function is a generic function to download files and check 
## their binary intergrity

downloadfile <- function (
  address=NULL,
  filename=NULL,
  path=NULL,
  overwrite=FALSE,
  md5sum=NULL,
  verbose=interactive()
) {
  
  if (verbose > 0) {
    print(paste("the function attempts to download", filename, "from internet"))
  }
  
  ## Change internet options to display more information
  opt.ori <- options()$internet.info
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
     print("(the folder you specified does not exist and will therefore be created)")
    }
    dir.create(path, recursive=TRUE)
  }
  
  ## Conditional file download
  complete.path <- paste(path, filename, sep="/")
  if (file.exists(complete.path) & !overwrite) {
    message(paste("the file", filename, "is already present in", path,
"so it won't be downloaded again unless you set the argument overwrite to TRUE"
    ))
  }	else {
    download.file(address, destfile=complete.path, mode="wb")
  }
  
  ## Checking MD5sum
  if (!is.null(md5sum)) {
    if (requireNamespace("tools", quietly = TRUE)) {
      if (tools::md5sum(complete.path) == md5sum) {
        print("the file seems OK (md5sums do match)")
      } else {
        warning("the file seems to be corructed (md5sums do not match)")
      }
    } else {
    warning("the package 'tools' is not installed, so the integrity of the downloaded file has not been checked")
    }
  }
  
  ## Display outcome
  if (verbose > 0) {
    print(paste("the file", filename, "is stored in the folder", path))
  }
  
  ## Restore original internet options
  options(internet.info = opt.ori)
  
  return(invisible(NULL))
}
