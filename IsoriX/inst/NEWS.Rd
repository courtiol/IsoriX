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
      \item the path in GetElev could break. This has now been solved.
    }
  }

	\subsection{New feature(s)}{
			\itemize{
			\item link to GitHub directory have been added
			\item new function DownloadFile to download non standard elevation raster or any other file
			\item function DownloadFile can perform MD5 checksum if the package 'tools' is installed
			\item function GetElev and DowloadFile can display additional information during download if verbose > 1
			}
	}

	\subsection{Minor changes}{
			\itemize{
			\item function GetElev lost 'address' argument has DownloadFile should now be used to download the non standard elevation raster
			\item typo fixed in description file
			}
	}
}

\section{version 0.4-1}{
	\subsection{New feature(s)}{
			\itemize{
			\item this is the first version of IsoriX submitted to CRAN!!!
			}
	}
}

