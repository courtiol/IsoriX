\name{GetElev}
\alias{GetElev}

\title{Download an elevation raster from internet}

\description{
This function allows for the download of an elevation raster from internet. By default, it downloads the "Global Multi-resolution Terrain Elevation Data 2010" from our server. The file was orifginally downloaded from: \cr \url{http://topotools.cr.usgs.gov/gmted_viewer/} \cr and converted into a \var{tif} file by us.}

\usage{
GetElev(
	path=NULL,
	address=NULL,
	overwrite=FALSE,
	verbose=interactive()
	) 
}

\arguments{
\item{path}{A \var{string} indicating where to store the file on the hard drive}
\item{address}{A \var{string} indicating where to download the file from}
\item{overwrite}{A \var{logical} indicating if an existing file should be re-downloaded}
\item{verbose}{A \var{logical} indicating whether information about the progress of the procedure should be displayed or not while the function is running. By default verbose is \code{TRUE} if users use an interactive R session and \code{FALSE} otherwise.}
}

\details{In the argument "path" is not provided, the file will be stored in the current working directory. The function can create new directories, so you can also indicate a new path. To download the elevation we prepared, leave the address to NULL.}

\source{\url{http://topotools.cr.usgs.gov/gmted_viewer/}}

\examples{
## To download the high resolution
## raster in your current working
## directory, just type:
## GetElev()
}