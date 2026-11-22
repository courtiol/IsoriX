library(testthat)
library(IsoriX)

## The test files are run in parallel (see Config/testthat/parallel in
## DESCRIPTION). If R uses a multithreaded linear algebra library (BLAS, e.g.
## OpenBLAS), each test process would use all the cores and the processes would
## compete for them, which makes the tests slower than when run one file after
## the other. So each process uses a single thread for linear algebra: the test
## processes, started by test_check() below, inherit these settings (those
## already defined by the user are kept).
for (var in c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS")) {
  if (Sys.getenv(var) == "") {
    do.call(Sys.setenv, stats::setNames(list("1"), var))
  }
}

test_check("IsoriX")
