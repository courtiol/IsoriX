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

\section{version 0.8}{
  \subsection{New features}{
    \itemize{
      \item new option in plot that allows to map the isoscape onto a sphere
      \item a new dataset PrecipBrickDE containing monthly precipitation amounts for Germany
    }
  }
  \subsection{Major changes}{
    \itemize{
      \item the vignettes have now been rewriten as a bookdown and moved out from the package. To access the documentation you should now visit: \url{https://bookdown.org/content/782/}
    }
  }
}

\section{version 0.7}{
  \subsection{New features}{
    \itemize{
      \item the calibration step is now optional, allowing for users to use an isoscape directly fitted on tissues instead of precipitation water
      \item the function queryGNIP has been renamed and is now called prepdata, this function can also handle other datasets than GNIP
      \item the function relevate has been modified to make crop possible around the pacific meridian -180/180 (but several issues remain to handle extra plot layers automatically)
    }
  }
  \subsection{Geeky changes}{
    \itemize{
      \item an additional options as been added to prevent prompting during examples
      \item new internal function .converts_months_to_numbers
    }
  }
}

\section{version 0.6}{
  \subsection{New features}{
      \itemize{
        \item the maximum duration of running time for examples can now be controlled using IsoriX.options(example_maxtime = XX)
        \item due to new GNIP policies, we no longer provide the GNIP dataset for the entire World, but only a subset containing data for Germany (users should thus compile their precipitatin data themselves from the 'wiser' plateform provided by GNIP; see vignette Workflow)
        \item it is now possible to control the colours and labels for the levels of isotopes or p-values in plots
        \item for plotting, it is no longer needed to load the ocean mask and country borders (it now happens automatically)
        \item the function relevate now allows for a cropping larger than the extent of the weather stations by means of the argument margin_pct
        \item it is now possible to create the so-called annual averaged precipitation isoscapes!
        \item queryGNIP can now split the dataset per month or year at each location during the aggregation
        \item new function prepcipitate to prepare the precipitation brick
        \item new function getprecip to download monthly precipitation rasters from WorldClim
        \item new function isomultifit fitting isoscapes per strata (month, year, or any "split")
        \item new function isomultiscape building isoscapes averaged across strata
        \item new function create_aliens simulating of organism data
      }
  }
    \subsection{Minor changes}{
      \itemize{
      \item the inputs for filtering data by month or year using queryGNIP have changed
      \item the default fixed effect structure for the mean model is isofit has changed 
      }
    }
    \subsection{Geeky changes}{
      \itemize{
        \item the namespace is now generated with Roxygen2
        \item the datasets are now 'lazy-loaded'
        \item new vignette for coding conventions
        \item changed some object names following our coding convention (more to come)
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
      \item new vignette explaining how to export spatial objects to GIS
      \item the file GNIPdata has been updated and now contain 2014 data
      \item names of all functions and objects have been refactored to remove upper cases
      \item links to our GitHub directory have been added
      \item new function downloadfile to download non standard elevation raster or any other file
      \item function getelev can perform MD5 sum checks if the package 'tools' is installed
      \item function getelev can display additional information during download if verbose > 1
      \item the column animalID in the assignment dataset can now handle names with spaces
      \item added Codecov to track test coverage for the package
      }
  }

  \subsection{Minor changes}{
      \itemize{
      \item the modification of the option set_ll_warn from the 'sp' package has been moved to onLoad (instead of onAttached) and the original state is now restored while unloading 'IsoriX' 
      \item the Earth distance method has been moved to the package 'spaMM'
      \item function getelev lost its 'address' argument as downloadfile should now be used to download non-standard elevation rasters
      \item some typo fixed in documentation files
      \item RandomFields moved to suggest
      \item .Rd files for documentation are now generated with Roxygen2
      \item queryGNIP is now provided with a single month argument specifying the months to select
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

