if (FALSE) { ## for not running during checks
  library(IsoriX)
  options_IsoriX(example_maxtime = Inf)

  example(options_IsoriX)
  example(getOption_IsoriX)

  example(getelev) # uncomment and run by hand
  example(getprecip) # uncomment and run by hand
  example(prepcipitate) # uncomment and run by hand

  example(prepraster)
  example(prepsources)

  example(isofit)
  example(isomultifit)

  example(isoscape)
  example(isomultiscape)

  example(calibfit)

  example(isofind)

  example(create_aliens)

  example(saveRDS_IsoriX)

  library(terra)
  example(saveRDS_IsoriX)
}

# devtools::check()
# devtools::build_manual()
