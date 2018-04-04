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
      \item new option in plots that allows to map the isoscape onto a sphere
      \item a new dataset PrecipBrickDE containing monthly precipitation amounts for Germany
      \item an argument y_title for the plotting function for isoscapes to allow one to simply change the title
      \item arguments xlab and ylab for the plotting function for calibration fits
      \item new method points for plotting more than one calibration fit
      \item the plotting function for assignments can now show the location of the assignment samples
    }
  }
  
  \subsection{Major changes}{
    \itemize{
      \item many objects have been renamed to prepare the release of the version 1.0
      \item the vignettes have now been rewriten as a bookdown and moved out from the package. To access the documentation you should now visit: \url{https://bookdown.org/content/782/}
    }
  }
  
  \subsection{Minor changes}{
    \itemize{
      \item all arguments 'bla.bla' have been renamed 'bla_bla'
      \item the plotting function for calibfit gains an argument "..." for more control
      \item a ploting method for rasterLayer has been included for conveniance
      \item the function relevate is now called prepraster
      \item the function prepdata is now called prepsources
      \item in several functions the argument elevation.raster has been renamed as raster
      \item in several functions the argument xxx.data has been renamed as data
    }
  }
  
  \subsection{Geeky changes}{
    \itemize{
      \item large temporary objects are now deleted within isofind to limit memory usage
      \item the package is now being tested using testthat, but test will be implemented in the future
      \item a lot of the internal code as been rewriten to comply more closely to the IsoriX coding style
      \item the list of suggested packages has been revised and rgdal removed as it caused (again) problems with Travis
      \item following a change in spaMM predict.HLfit, the prediction are now being made by chunck of 1000 points instead of 150. This should lead to a tiny gain in performance under spaMM version higher than 2.2.
      \item the function isoscape was performing predictions twice every 150 (or now 1000) locations, this was not influencing the isoscapes produced, but this has now been corrected
      \item the function prepraster now produces an raster stored in memory if it is possible. This should prevent bugs that appears when using loaded rasters that were previously saved (the temporary link to the hard drive location is no longer correct in this case).
      \item the function .objective_fn_calib has been moved within the function calibfit as it is not used elsewhere
      \item the function calibfit as been prepared for a possible activation of a random effect for species ID in the future. But whether it would make sense or not remains to be determined.
      \item the function .Fisher_method now directly computes the exponential of the log pv if only one value is provided. This leads to much faster assignment in the case of a single observation.
    }
  }
  
  \subsection{Solved BUGS}{
    \itemize{
      \item the plotting function for calibration fit was displaying CI based on variance instead of SD
      \item the function getprecip and prepcipitate were not handling paths manualy defined properly
      \item the plotting functions were crashing in case of no variation in the landscape
      \item the plotting functions were crashing when called on multiple-raster objects not stored 'inMemory'
      \item the plotting function for fitted model was not displaying one plot in RStudio when called on objects of class MULTIISOFIT
    }
  }
}

\section{version 0.7.1}{
  \subsection{New features}{
    \itemize{
     \item this is a minor update necessary to maintain compatibility with spaMM 2.4
    }
  }
  \subsection{Geeky changes}{
    \itemize{
      \item the syntax for the extraction of correlation terms of spaMM objects has changed
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

