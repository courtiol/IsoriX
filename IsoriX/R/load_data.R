#' Load data 
#' 
#' This function downloads and adds climatic data to improve the fit. 
#' 
#' Data comes from the University of East Anglia Climate Research Unit (CRU)
#' climatology data (see \url{\link{https://docs.ropensci.org/getCRUCLdata/}}).
#' 
#' @param data A \var{dataframe} containing raw isotopic measurements of sources
#' @param data_needed A \var{list} of \var{logical} indicating which variable to add. 
#'   The following data are available:
#' \describe{
#' \item{pre}{precipitation (millimetres/month)}
#'   \describe{
#'     \item{pre_cv}{cv of precipitation (percent)}
#'   }
#' \item{rd0}{wet-days (number days with >0.1mm rain per month)}
#' \item{tmp}{mean temperature (degrees Celsius)}
#' \item{tmn}{minimum temperature (degrees Celsius)}
#' \item{tmx}{maximum temperature (degrees Celsius)}
#' \item{dtr}{mean diurnal temperature range (degrees Celsius)}
#' \item{reh}{relative humidity (percent)}
#' \item{sunp}{sunshine (percent of maximum possible (percent of day length))}
#' \item{frs}{ground-frost (number of days with ground-frost per month)}
#' \item{wnd}{10 metre windspeed (metres/second)}
#' \item{elv}{elevation (automatically converted to metres)}
#' }  
#'    
#' @param long_min A \var{numeric} indicating the minimum longitude to select
#'   from. Should be a number between -180 and 180. If not provided, -180 will
#'   be considered.
#' @param long_max A \var{numeric} indicating the maximal longitude to select
#'   from. Should be a number between -180 and 180. If not provided, 180 will be
#'   considered.
#' @param lat_min A \var{numeric} indicating the minimum latitude to select
#'   from. Should be a number between -90 and 90. If not provided, -90 will be
#'   considered.
#' @param lat_max A \var{numeric} indicating the maximal latitude to select
#'   from. Should be a number between -90 and 90. If not provided, 90 will be
#'   considered.
#' @param split_by A \var{string} indicating whether data should be aggregated
#'   per location (\code{split_by = NULL}, the default), per location:month
#'   combination (\code{split_by = "month"}), or per location:year combination
#'   (\code{split_by = "year"}).
#' @return This function returns a \var{list} of two elements: A \var{dataframe}
#' containing the extended data and a \var{list} containing the raster.
#' @export
#' @examples
#' load_data(data = GNIPDataDE, 
#'           data_needed = list(pre = TRUE, tmp = TRUE, elv = FALSE), split_by = "year")  
load_data <- function(data,
                     data_needed = list(pre = FALSE,
                                        pre_cv = FALSE,
                                        rd0 = FALSE,
                                        tmp = FALSE,
                                        dtr = FALSE,
                                        reh = FALSE,
                                        tmn = FALSE,
                                        tmx = FALSE,
                                        sunp = FALSE,
                                        frs = FALSE,
                                        wnd = FALSE,
                                        elv = FALSE),
                     long_min,
                     long_max,
                     lat_min,
                     lat_max,
                     split_by = NULL) {
  
   .complete_args(load_data)
  
  ### add missing values not needed if inside a function already completing the args. 
  if (missing("long_min")) long_min <- -180
  if (missing("long_max")) long_max <- 180
  if (missing("lat_min")) lat_min <- -90
  if (missing("lat_max")) lat_max <- 90 
  
  ## get the data 
   CRU_data <- getCRUCLdata::get_CRU_stack(pre = data_needed$pre,
                                           pre_cv = data_needed$pre_cv,
                                           rd0 = data_needed$rd0,
                                           tmp = data_needed$tmp,
                                           dtr = data_needed$dtr,
                                           reh = data_needed$reh,
                                           tmn = data_needed$tmn,
                                           tmx = data_needed$tmx,
                                           sunp = data_needed$sunp,
                                           frs = data_needed$frs,
                                           wnd = data_needed$wnd,
                                           elv = data_needed$elv)
   
   ## crop the data according to the user choice
   raster <- lapply(CRU_data, function(x) {
      raster::crop(x,
        raster::extent(long_min,
                       long_max,
                       lat_min,
                       lat_max))})
   
   ## Case for extracting one value per location
     ## first extract the value for each location 
   if(is.null(split_by) || split_by == "year") {
     raster <- lapply(raster, function(x) raster::mean(x))
     data_points <- lapply(raster, function(x) raster::extract(x = x, y = data[ ,c("long", "lat")]))
     
     ## save as a brick with each variable as a layer
     raster_out <- raster::brick(raster)
     
   ## Case for extracting one value per month:location  
   } else if (split_by == "month") {
      
     ## Use raster extract directly for rasterlayer (elv) and a loop going along the different layers (month) of a rasterbrick.  
     data_points <- lapply(raster, function(x) {
       if(length(names(x)) == 1) { ## 
       raster::extract(x = x, y = data[ ,c("long", "lat")])
       } else {
         sapply(seq_len(nrow(data)), function(i) {
         raster::extract(x = x, y = data[i, c("long", "lat")], layer = data$month[i], nl = 1)
         })
       }
     })
     ## save the raster and shape it as a list of brick by month with each layer being a variable. 
     if (data_needed$elv == TRUE) elvq <- raster[names(raster) == "elv"]
     
     raster_list <- raster[names(raster) != "elv"]
     raster_out <- vector(mode = "list", length = 12)
     names(raster_out) <- names(raster_list[[1]])
     
     for(month in seq_along(raster_out)) {
      raster_out[[month]] <- raster::brick(lapply(raster_list, function(x) x[[month]]))
   
     } 
     if(data_needed$elv == TRUE) raster_out <- lapply(raster_out, function(x) raster::addLayer(x, elvq))
      
   } else {
     stop("The argument you chose for split_by is unknown.")
   }
   
   ## aggregate the output into a list 
   data_out <- cbind(data, data_points)
   list(data_out, raster_out)
}

  
