## Delete Rplots.pdf if it exists (if tails fail, it does not otherwise get deleted)

withr::defer(
  {
    if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  },
  teardown_env()
)

## The test files are already run in parallel by testthat (see
## Config/testthat/parallel in DESCRIPTION), so the functions of IsoriX must not
## start parallel computations of their own by default during the tests: that
## would use more than 2 CPUs, which CRAN does not allow. The global option Ncpu
## is thus set to 1 in each test process; tests of parallel computations set
## `Ncpu` explicitly and are skipped on CRAN.
old_options_IsoriX <- options_IsoriX()
options_IsoriX(Ncpu = 1L)
withr::defer(options_IsoriX(old_options_IsoriX), teardown_env())
