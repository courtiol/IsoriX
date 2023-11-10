## Delete Rplots.pdf if it exists (if tails fail, it does not otherwise get deleted)

withr::defer({
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  }, teardown_env())
