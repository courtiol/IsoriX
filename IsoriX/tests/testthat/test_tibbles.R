test_that("The workfow works with tibble", {
  skip_if_not_installed("tibble")

  set.seed(123)
  index <- sample(seq_len(nrow(GNIPDataDE)), 50)
  test_df <- GNIPDataDE[index, ]
  test_tbl <- tibble::as_tibble(test_df)
  test_tbl$source_ID <- as.character(test_tbl$source_ID)
  ref <- prepsources(test_df)
  job <- prepsources(test_tbl)
  expect_equal(class(job), "data.frame")
  expect_equal(ref, job)


  ## Test that spaMM works correctly with tibbles
  ## Isofit

  ref_isofit <- isofit(ref)
  ref_2 <- ref_isofit$mean_fit$fixef

  job <- tibble::as_tibble(job)
  job_isofit <- isofit(job)
  job_2 <- job_isofit$mean_fit$fixef

  expect_equal(ref_2, job_2)

  ## calibfit
  test_df2 <- CalibDataBat[1:10, ]
  test_tbl2 <- tibble::as_tibble(test_df2)
  test_tbl2$site_ID <- as.character(test_tbl2$site_ID)

  ref_calib <- calibfit(test_df2, ref_isofit) ## df and df
  ref_3 <- ref_calib$fixefCov

  job_calib <- calibfit(test_tbl2, ref_isofit) ## tbl and df
  job_3 <- job_calib$fixefCov

  job_calib2 <- calibfit(test_tbl2, job_isofit) ## tbl and tbl
  job_33 <- job_calib2$fixefCov
  expect_equal(job_3, ref_3)
  expect_equal(job_33, ref_3)
})
