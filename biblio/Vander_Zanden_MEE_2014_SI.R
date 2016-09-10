#Some libraries you may need
library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(fields)
library(spam)
library(pracma)
library(usdm)
library(SDMTools)


###### RESCALING FUNCTION #######
#This function conducts 1000 simulated regressions sampled from normal distributions of data randomly generated from the means and SDs at each site.  
#The output contains the slopes and intercepts of each of the 1000 regression lines, from which a mean or distrubtion can be extracted.
#Start with a .csv table that has the calibration data organized by site with tissue mean and SD, number of individuals per site, and the corresponding precip mean and SD extracted from the appropriate precip isoscape
#Function includes:
#table = the filename (with directory, if applicable) from which to load the data
#siteID = column # containing unique site IDS
#count = column # containing number of individuals sampled per site
#tissue.mean = column # containing mean d2H tissue values of individuals sampled at each site
#tissue.SD = column # containing SD of d2H tissue values of individuals sampled at each site
#precip.mean = column # containing mean d2H precip values at each site
#precip.SD = column # containing SD of d2H precip values at each site

rescale <- function(table, siteIDs, count, tissue.mean, tissue.SD, precip.mean, precip.SD) { 
  calibration <- read.table(table, header=TRUE,
                            sep=",", na.strings="NA")
  
  AllSites <- unique(calibration[, siteIDs])
  slopes <- vector('numeric', length=1000)
  intercepts <- vector('numeric', length=1000)
  
  for (k in 1:1000){
    counter <- 1
    tissue.d2H <- vector('numeric', length=sum(calibration[,count]))
    precip.d2H <- vector('numeric', length=sum(calibration[,count]))
    
    for (i in 1:length(AllSites)){
      Site.i <- AllSites[i]
      Data.i <- calibration[calibration[,siteIDs]==Site.i,]
      n <- Data.i[,count]
      
      for (j in 1:n){
        tissue.d2H[counter:(counter+n-1)] <- rnorm(n, mean=Data.i[1, tissue.mean], sd=Data.i[1, tissue.SD])
        precip.d2H[counter:(counter+n-1)] <- rnorm(n, mean=Data.i[1, precip.mean], sd=Data.i[1, precip.SD])
        counter <- counter+1
        lmResult.k <- lm(tissue.d2H~precip.d2H)
        intercepts[k] <- coef(lmResult.k)[1]
        slopes[k] <- coef(lmResult.k)[2]
        
      }}}
  
  slope <- mean(slopes)
  intercept <-mean(intercepts)
  return(data.frame(slopes, intercepts))
}

###### RASTER CONVERSION ######
#This function uses the output from the rescaling function to convert the precip rasters to tissue-specific mean raster and rescale SD raster (used in the pooled error)
#Function includes:
#original.raster = the filename (and directory, if applicable) of the original precip raster created in IsoMAP
#reg.par = the output from the function above
#scratch.dir = the directory of a scratch folder to store the rasters temporarily

raster.conversion <- function (original.raster, reg.par, scratch.dir) { 
  
  for (i in 1:length(reg.par[,1])) {
    reg.par.i <- reg.par[i,]
    raster.i <- original.raster*reg.par.i$slopes + reg.par.i$intercepts
    name <- paste(scratch.dir, i, ".grd", sep="")
    writeRaster(raster.i, filename=name, overwrite=TRUE)
  }
  setwd(scratch.dir)
  all.files <- dir(pattern=".grd")
  n <- length(all.files)
  all.rasters <- stack(all.files)
  mean.raster <- original.raster*mean(reg.par$slopes) + mean(reg.par$intercepts) 
  SD.raster <- stackApply(all.rasters, fun=sd, indices=c(rep(1,n)))
  return(list(mean.raster=mean.raster, SD.raster=SD.raster))
}

###### ASSIGNMENT FUNCTION ######
#This function uses the likelihood term (Equation S2 in Appendix 1) to determine the probability that an individual sample was from a particular geographic location and writes an ascii file to a chosen directory
#Because IsoMAP produces the files in the ascii format that is compatible with ArcGIS software, I have maintained that format here and throughout
#Function includes:
#rescaled_raster = the tissue-specific d2H raster created in the function above
#rescaleded_SD_raster = the SD raster created in the function above.  This the component of the error term related to the rescaling process.
#precip_SD_raster = this is the SD raster associated with the IsoMAP output.  This is the precip component of the variance term.
#SD_indv =the individual component of the variance term.  This is a value determined by the user.  We calculated the mean of the SDs observed among individuals at all of the calibration sites.
#assign_table = this a csv filename (and directory, if applicable) containing the tissue d2H values of the individuals for which the assignments will be made 
#d2Htissue = column number in the assign_table with the d2H tissue values
#ID = column number with individual identifiers
#save_dir is where the output assignments should be saved as an ascii, but could be changed

assignment <- function(rescaled_raster, rescaled_SD_raster, precip_SD_raster, SD_indv, assign_table, d2Htissue, ID, save_dir){
  error <- sqrt((rescaled_SD_raster)^2 + (precip_SD_raster)^2 + (SD_indv)^2)
  data <- read.table(assign_table, sep=",", header=T)
  data <- data[1:5,]
  n <- length(data[,d2Htissue])
  
  for (i in 1:n){
    indv.data <- data[i,]
    indv.id <- indv.data[1, ID] 
    assign <- (1/(2*pi*error^2))*exp(-1*(indv.data[1,d2Htissue]-rescaled_raster)^2/(2*error^2))
    assign_norm <- assign/cellStats(assign, "sum") #normalize so all pixels sum to 1
    filename <- paste(save_dir, indv.id, ".like", ".asc", sep="")
    writeRaster(assign_norm, file=filename, format="ascii", overwrite=TRUE)
  }
}


###### POPULATION-LEVEL ACCURACY FUNCTION ######
#This function calculates the accuracy (whether or not the known location is included in the assignment at a given relative probability density value) for all of the assignment ascii files in the provided directory.
#For each file, the function determines the latitude and longitude of the known vaidation site and determines whether the relative probability is included within a given interval between 0.01 to 0.99
#Function includes:
#wd = directory where assignment ascii files are stored
#validation.data = the filename (with directory, if applicable) with the validation data locations (containing latitude and longitude values for each sample)
#long.col = the column number in the validation.data file containing the longitude of each validation point, in the same order that the ascii files are listed in the working directory
#lat.col = the column number in the validation.data file containing the latitude of each validation point, in the same order that the ascii files are listed in the working directory

accuracy <- function(wd, validation.data, long.col, lat.col) {
  setwd(wd)
  allfiles <- dir(pattern=".asc")
  n <- length(allfiles)
  validation <- read.table(validation.data, sep=",", header=T, na.strings=NA)
  cumulative <- matrix(nrow=n, ncol=99, byrow=TRUE)
  known.probs <- vector('numeric', length=n)
  
  for(i in 1:n){
    assignment.asc <- read.asc(allfiles[i], gz=FALSE)
    assignment <- raster.from.asc(assignment.asc)
    projection(assignment) <- CRS("+proj=longlat +datum=WGS84")
    assignment.scaled <- assignment/cellStats(assignment, 'max')
    known.xy <- validation[i, c(long.col, lat.col)]
    known.probs[i] <- extract(assignment.scaled, known.xy)
    cumulative[i,] <- known.probs[i]>seq(0.01, 0.99, 0.01)
  }      
  output <- as.data.frame(cumulative) 
 }    

###### INDIVIDUAL LEVEL ACCURACY FUNCTION ######
#This function calculates the individual-level change in accuracy at the known location between two assignment methods
#Paired assignment files in two different directoreis are compared at the known origin to determine the change (short-term minus long-term) in the relative probability across a range of intervals between 0.01 and 0.99
#Function includes:
#wd1 = directory where assignment ascii files are stored for the first assignment method/model (these were the assignments done with the short-term model)
#wd2 = directory where assignment ascii files are stored for the second assignment method/model (these were the assignments done with the long-term model)
  #(these two directories should have paired assignment files)
#validation.data = the filename (with directory, if applicable) with the validation data locations (containing latitude and longitude values for each sample)
#long.col = the column number in the validation.data file containing the longitude of each validation point, in the same order that the ascii files are listed in the working directories
#lat.col = the column number in the validation.data file containing the latitude of each validation point, in the same order that the ascii files are listed in the working directories


accuracy.ind <- function(wd1, wd2, validation.data, long.col, lat.col) {
  validation <- read.table(validation.data, sep=",", header=T, na.strings=NA)
  
  setwd(wd1)
  allfiles1 <- dir(pattern=".asc")
  n1 <- length(allfiles1)
  known.prob1 <- vector('numeric', length=n1)
  
  for(i in 1:n1){
    assignment.asc <- read.asc(allfiles1[i], gz=FALSE)
    assignment <- raster.from.asc(assignment.asc)
    projection(assignment) <- CRS("+proj=longlat +datum=WGS84")
        assignment.scaled <- assignment/cellStats(assignment, 'max')
    known.xy <- validation[i, c(long.col, lat.col)]
    known.prob1[i] <- extract(assignment.scaled, known.xy)
  }
  
  setwd(wd2)
  allfiles2 <- dir(pattern=".asc")
  n2 <- length(allfiles2)
  known.prob2 <- vector('numeric', length=n2)
  
  for(j in 1:n2){
    assignment.asc <- read.asc(allfiles2[j], gz=FALSE)
    assignment <- raster.from.asc(assignment.asc)
    projection(assignment) <- CRS("+proj=longlat +datum=WGS84")
    assignment.scaled <- assignment/cellStats(assignment, 'max')
    known.xy <- validation[j, c(long.col, lat.col)]
    known.prob2[j] <- extract(assignment.scaled, known.xy)
  }
  output <- as.data.frame(cbind(known.prob1, known.prob2, prob.dif = known.prob1-known.prob2))
}    

###### POPULATION-LEVEL PRECISION FUNCTION ######
#This function calculates the precision (or relative surface area) for all of the assignment ascii files in the provided directory.
#For each file, the function relativizes the probability values to the maximum, 
  #and calculates the relative surface area (normalized to the total possible surface) at each realtive probability value (or interval between 0.01 and 0.99 at 0.01 increments)
#Function includes:
#wd = directory where assignment ascii files are stored

precision <- function(wd) {
  setwd(wd)
  allfiles <-  dir(pattern=".asc")
  n <- length(allfiles)
  thresholds <- seq(0.01, 0.99, 0.01)
  all.areas <- matrix(nrow=n, ncol=99)
  
  for(i in 1:n){
    assignment.asc <- read.asc(allfiles[i], gz=FALSE)
    assignment <- raster.from.asc(assignment.asc)
    projection(assignment) <- CRS("+proj=longlat +datum=WGS84")
    assignment.scaled <- assignment/maxValue(assignment)
    cell.size <- area(assignment.scaled, na.rm=TRUE)
    assignment.pixels <- assignment.scaled>0
    total.area <- cellStats(cell.size, 'sum', na.rm=TRUE)
    
    for (j in 1:length(thresholds)) {
      threshold.j <- thresholds[j]
      surface.j <- assignment.scaled>threshold.j
      all.areas[i, j] <- cellStats(surface.j*cell.size, 'sum', na.rm=TRUE)/total.area
      
    }
  }
  output <- as.data.frame(all.areas)
}

###### INDIVIDUAL-LEVEL PRECISION FUNCTION ######
#This function calculates the change in surface area (short-term minus long-term) of the posterior probability surface between the two assignment methods.  
#The surface area is defined by using a threshold equal to the relative probability density value at the known origin in both cases.
#Paired assignment files in two different directoreis are compared at the known origin to determine the change in the relative probability across a range of intervals from 0.01 to 0.99
#Function includes:
#wd1 = directory where assignment ascii files are stored for the first assignment method/model (these were the assignments made with the short-term model)
#wd2 = directory where assignment ascii files are stored for the second assignment method/model (these were the assignments madee with the long-term model)
#(these two directories should have paired assignment files)
#validation.data = the filename (with directory, if applicable) that contains the validation data locations (containing latitude and longitude values for each sample)
#long.col = the column number in the validation.data file containing the longitude of each validation point, in the same order that the ascii files are listed in the working directories
#lat.col = the column number in the validation.data file containing the latitude of each validation point, in the same order that the ascii files are listed in the working directories

precision.ind <- function(wd1, wd2, validation.data, long.col, lat.col) {
  validation <- read.table(validation.data, sep=",", header=T, na.strings=NA)
  
  setwd(wd1)
  allfiles1 <- dir(pattern=".asc")
  n1 <- length(allfiles1)
  known.prob1 <- vector('numeric', length=n1)
  ind.area1 <- vector('numeric', length = n1)
  
  for(i in 1:n1){
    assignment.asc <- read.asc(allfiles1[i], gz=FALSE)
    assignment <- raster.from.asc(assignment.asc)
    projection(assignment) <- CRS("+proj=longlat +datum=WGS84")
    assignment.scaled <- assignment/cellStats(assignment, 'max')
    cell.size <- area(assignment.scaled, na.rm=TRUE)
    known.xy <- validation[i, c(long.col, lat.col)]
    known.prob1[i] <- extract(assignment.scaled, known.xy)
    surface.at.prob <- assignment.scaled > known.prob1[i]
    ind.area1[i] <- cellStats(surface.at.prob*cell.size, 'sum', na.rm=TRUE)
  }
  
  setwd(wd2)
  allfiles2 <- dir(pattern=".asc")
  n2 <- length(allfiles2)
  known.prob2 <- vector('numeric', length=n2)
  ind.area2 <- vector('numeric', length=n2)
  
  for(j in 1:n2){
    assignment.asc <- read.asc(allfiles2[j], gz=FALSE)
    assignment <- raster.from.asc(assignment.asc)
    projection(assignment) <- CRS("+proj=longlat +datum=WGS84")
    assignment.scaled <- assignment/cellStats(assignment, 'max')
    cell.size <- area(assignment.scaled, na.rm=TRUE)
    known.xy <- validation[j, c(long.col, lat.col)]
    known.prob2[j] <- extract(assignment.scaled, known.xy)
    surface.at.prob <- assignment.scaled > known.prob2[j]
    ind.area2[j] <- cellStats(surface.at.prob*cell.size, 'sum', na.rm=TRUE)
  }
  output <- as.data.frame(cbind(ind.area1, ind.area2, ind.area.dif = ind.area1-ind.area2))
}    

###### SIMILARITY INDEX ######
#This function that calculates the similarity index between two rasters was adopted from the Expected fraction of Shared Presences (ESP) metric proposed by Godsoe (2014).  
#This index calculates the ratio of the number of shared cells between paired assignment rasters to the total number of possible cells at a range of relative probabilities between 0.01 and 0.99  
#Function makes all pairwise comparisons in the 2 working directories.  The two rasters should have the same extent, and I use the extent function in the raster package to ensure that.
#Function includes:
#wd1 = directory where assignment ascii files are stored for the first assignment method/model (these were the assignments done with the short-term model)
#wd2 = directory where assignment ascii files are stored for the second assignment method/model (these were the assignments done with the long-term model)

similarity <- function(wd1, wd2) {
  setwd(wd1)
  allfiles1 <- dir(pattern=".asc")
  setwd(wd2)
  allfiles2 <- dir(pattern=".asc")
  ind1 <- length(allfiles1)
  ind2 <- length(allfiles2) 
  
  area <- vector('numeric', length=ind1)
  
  for(j in 1:ind1){
    setwd(wd1)
    assignment.asc1 <- read.asc(allfiles1[j], gz=FALSE)
    assign1 <- raster.from.asc(assignment.asc1)
    projection(assign1) <- CRS("+proj=longlat +datum=WGS84")
    assign1.scaled <- assign1/maxValue(assign1)
    
    setwd(wd2)
    assignment.asc2 <- read.asc(allfiles2[j], gz=FALSE)
    assign2 <- raster.from.asc(assignment.asc2)
    projection(assign2) <- CRS("+proj=longlat +datum=WGS84")
    assign2.scaled <- assign2/maxValue(assign2)
    
    y <- intersect(assign1.scaled, assign2.scaled)
    assign1r <- resample(assign1.scaled, y, method = "ngb")
    assign2r <- resample(assign2.scaled, y, method = "ngb")
    
    ESP <- vector('numeric', length=99)
    
    for (i in 1:99) {
      reassign1 <- assign1r>i/100
      reassign2 <- assign2r>i/100
      ESP[i] <- 2*cellStats(reassign1*reassign2, 'sum')/(cellStats(reassign1, 'sum') + cellStats(reassign2, 'sum'))
    }
    
    x <- seq(0.01,0.99,0.01)
    area[j] <- trapz(x,ESP)
  }
  return(area)
}
