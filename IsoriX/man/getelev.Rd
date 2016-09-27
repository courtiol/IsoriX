\name{getelev}
\alias{getelev}
\alias{downloadfile}

\title{Download an elevation raster from internet}

\description{
The function \code{getelev} allows for the download of an elevation raster from internet. It downloads the "Global Multi-resolution Terrain Elevation Data 2010" from our server. The file was orifginally downloaded from: \cr \url{http://topotools.cr.usgs.gov/gmted_viewer/} \cr and converted into a \var{tif} file by us. The function \code{getelev} uses the generic function \code{downloadfile} that can also be used to download directly other files.}

\usage{
getelev(
	path=NULL,
	overwrite=FALSE,
	verbose=interactive()
	) 

downloadfile(
  address=NULL,
  filename=NULL,
  path=NULL,
  overwrite=FALSE,
  md5sum=NULL,
  verbose=interactive()
)
}

\arguments{
\item{path}{A \var{string} indicating where to store the file on the hard drive}
\item{overwrite}{A \var{logical} indicating if an existing file should be re-downloaded}
\item{verbose}{A \var{logical} indicating whether information about the progress of the procedure should be displayed or not while the function is running. By default verbose is \code{TRUE} if users use an interactive R session and \code{FALSE} otherwise. If a \var{numeric} is provided instead, additional information about the download will be provided if the number is greater than 1.}
\item{address}{A \var{string} indicating where to download the file from}
\item{filename}{A \var{string} indicating the name of the file to be writen on the disk}
\item{md5sum}{An optional \var{string} indicating the MD5 hashes of the file for testing its integrity}
}

\details{In the argument "path" is not provided, the file will be stored in the current working directory. The functions can create new directories, so you can also indicate a new path. If the package \pkg{\link[tools]{tools}} is installed, the integrity of the elevation raster is tested after a call to \code{getelev}. In case of corruption, try downloading the file again, specifying overwrite=TRUE to overwrite the corrupted file.}


\source{\url{http://topotools.cr.usgs.gov/gmted_viewer/}}

\examples{
## To download the high resolution
## raster in your current working
## directory, just type:
## getelev()
}