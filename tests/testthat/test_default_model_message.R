test_that(".is_default_isofit_model() detects the default model structure", {
  defaults <- formals(isofit)
  args <- lapply(
    c("mean_model_fix", "disp_model_fix", "mean_model_rand", "disp_model_rand", "uncorr_terms"),
    \(arg) eval(defaults[[arg]])
  )
  names(args) <- c("mean_model_fix", "disp_model_fix", "mean_model_rand", "disp_model_rand", "uncorr_terms")
  expect_true(do.call(.is_default_isofit_model, args))

  args_elev <- args
  args_elev$mean_model_fix$elev <- TRUE
  expect_false(do.call(.is_default_isofit_model, args_elev))

  args_rand <- args
  args_rand$disp_model_rand$spatial <- FALSE
  expect_false(do.call(.is_default_isofit_model, args_rand))
})

test_that("isofit() displays the default model message only once per session", {
  old_flag <- .data_IsoriX$default_model_msg_shown
  on.exit(.data_IsoriX$default_model_msg_shown <- old_flag, add = TRUE)
  .data_IsoriX$default_model_msg_shown <- NULL

  agg <- prepsources(data = GNIPDataDE)

  ## not displayed when the model is not the default one
  expect_no_message(
    isofit(data = agg, mean_model_fix = list(elev = TRUE), verbose = FALSE),
    message = "default model of isofit"
  )

  ## displayed the first time the default model is fitted
  expect_message(
    isofit(data = agg, verbose = FALSE),
    "default model of isofit"
  )

  ## not displayed again during the same session
  expect_no_message(
    isofit(data = agg, verbose = FALSE),
    message = "default model of isofit"
  )
})
