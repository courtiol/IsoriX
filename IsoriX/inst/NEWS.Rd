\name{NEWS}
\title{IsoriX News}

\encoding{UTF-8}

\section{version 1.0}{
	\subsection{Upcoming features planned for future releases}{
			\itemize{
			\item (version 1.0 does not exist yet)
			\item feature requests can be defined and watched here: \url{https://github.com/courtiol/IsoriX_project/issues}
		}
	}
}


\section{version 0.4-2}{
  \subsection{Solved BUGS}{
    \itemize{
      \item the package could not be detached and reloaded
      \item the citation was not correct
      \item the path in GetElev was breaking in some cases
      \item the title of the assignment plot was missing when a single individual was plotted
    }
  }

	\subsection{New feature(s)}{
			\itemize{
			\item links to our GitHub directory have been added
			\item new function DownloadFile to download non standard elevation raster or any other file
			\item function DownloadFile can perform MD5 sum checks if the package 'tools' is installed
			\item function GetElev and DowloadFile can display additional information during download if verbose > 1
			\item the column animalID in the assignment dataset can now handle names within spaces
			}
	}

	\subsection{Minor changes}{
			\itemize{
			\item function GetElev lost its 'address' argument as DownloadFile should now be used to download non standard elevation rasters
			\item some typo fixed in documentation files
			}
	}
}

\section{version 0.4-1}{
	\subsection{New feature(s)}{
			\itemize{
			\item this was the first version of IsoriX submitted to CRAN
			}
	}
}

