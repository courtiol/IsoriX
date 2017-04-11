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


\section{version 0.5}{
  \subsection{Solved BUGS}{
    \itemize{
      \item the package could not be detached and reloaded
      \item the citation was not correct
      \item the path in getelev was breaking in some cases
      \item the title of the assignment plot was missing when a single individual was plotted
    }
  }

	\subsection{New feature(s)}{
			\itemize{
			\item the file GNIPdata has been updated and now contain 2014 data
			\item names of all functions and objects have been refactored to remove upper cases
			\item links to our GitHub directory have been added
			\item new function downloadfile to download non standard elevation raster or any other file
			\item function getelev can perform MD5 sum checks if the package 'tools' is installed
			\item function getelev can display additional information during download if verbose > 1
			\item the column animalID in the assignment dataset can now handle names with spaces
			}
	}

	\subsection{Minor changes}{
			\itemize{
			\item the modification of the option set_ll_warn from the 'sp' package has been moved to onLoad (instead of onAttached) and the original state is now restored while unloading 'IsoriX' 
			\item the Earth distance method has been moved to the package 'spaMM'
			\item function getelev lost its 'address' argument as downloadfile should now be used to download non-standard elevation rasters
			\item some typo fixed in documentation files
			\item RandomFields moved to suggest
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

