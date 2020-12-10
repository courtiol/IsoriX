#' Load data 
#' 
#' This function download and add climatic data to improve the fit. 
#' 
#' Data comes from the University of East Anglia Climate Research Unit (CRU)
#' climatology data (see \url{\link{https://docs.ropensci.org/getCRUCLdata/}}).
#' 
#' @param data A \var{dataframe} containing raw isotopic measurements of sources
#' @param data_to_add A \var{list} of \var{logical} indicating which variable to add. 
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
#'        
#' @examples
#' load_data(GNIPDataDE, 
#'          data_needed = list(pre = TRUE, tmp = TRUE, elv = T), split_by = "month")  
load_data <- function(data,
                     data_needed = list(pre = TRUE,
                                        pre_cv = FALSE,
                                        rd0 = TRUE,
                                        tmp = FALSE,
                                        dtr = TRUE,
                                        reh = FALSE,
                                        tmn = FALSE,
                                        tmx = FALSE,
                                        sunp = FALSE,
                                        frs = FALSE,
                                        wnd = FALSE,
                                        elv = TRUE),
                     long_min,
                     long_max,
                     lat_min,
                     lat_max,
                     split_by = NULL) {
  
   .complete_args(get_data)
  
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
   
   ## summarise the brick into layer 
   location <- data[,c("long", "lat")]
   
   ## Case for extracting one value per location
   if(is.null(split_by) || split_by == "year") {
     raster <- lapply(raster, function(x) raster::mean(x))
     data_points <- lapply(raster, function(x) raster::extract(x = x, y = location))
     
     ## save as a brick with each variable as a layer
     raster_out <- raster::brick(raster)
     
    ## Case for extracting one value per month:location  
   } else if (split_by == "month") {
     layer <- data$month

     ## need different ways to extract data from rasterlayer and from raster brick
     data_points <- lapply(raster, function(x) {
       if(raster::nlayers(x) < 2) {
       raster::extract(x = x, y = location)
       } else {
         sapply(seq_len(layer), function(i) {
         raster::extract(x = x, y = data[i, c("long", "lat")], layer = data$month[i], nl = 1)
         })
       }
     })
     ## save the raster and transpose it to a list of brick by month
     elev <- raster[names(raster) == "elv"]
     raster_list <- raster[names(raster) != "elv"]
     
     raster_out <- vector(mode = "list", length = 12)
     for(month in 1:12) {
      raster_out[month] <-  raster::brick(lapply(raster_list, function(x) x[[month]]))
     }
      raster_out <- lapply(raster_out, function(x) raster::addLayer(x, elev))
     
   } else {
     stop("data cannot be split by this variable")
   }
   
   ## aggregate the output into a list 
   data_out <- cbind(data, data_points)
   list(data_out, raster_out)
  

  
