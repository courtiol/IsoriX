createassignData <- function(assignFunction = list(slope = 0.5, intercept = 3),
                             isoscape=isoscape) {
  
  ## choose location for these sendentary animals
  
  cells <- rep(sample(1:ncell(isoscape$isoscape$mean), 1, replace=F), 255)
  xy <- xyFromCell(isoscape$isoscape$mean, cells)
  
  ## get the isotopic value in the environment
  environment.value <- extract(isoscape$isoscape$mean, xy)
  residualVar <- rnorm(length(cells), mean = 1, sd = 0.02)
  
  ## prepare the dataset

  animalID <- paste0("animal", sep = "_", 1:length(cells))
  tissue.value <- (intercept + environment.value * slope) * residualVar
  long <- xy[, 1]
  lat <- xy[, 2]
  assignData <- data.frame(animalID, tissue.value, long, lat)
  
  return(assignData)
}
