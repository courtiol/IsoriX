# Sources for the R package IsoriX

This is the development folder for the R package IsoriX.

See [main page](https://courtiol-isorix.share.connect.posit.cloud/) for description and installation procedure.

## Steps before new release

  - run `gitcreds::gitcreds_set()` to enter the GitHub token.
  - run `usethis::use_release_issue(version = "XX")` with `"XX"` the future release number.
  - follow the steps in the issue created by the previous command.

## Some useful links for developers

 - [Wickham & Bryan's book *R packages*](https://r-pkgs.org/)
 - [Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
 - [R coding standard](https://style.tidyverse.org/)
 - [Using version control in RStudio](https://docs.posit.co/ide/user/ide/guide/tools/version-control.html)
 - [Preparing your package for a CRAN submission](https://github.com/ThinkR-open/prepare-for-cran)
