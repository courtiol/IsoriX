# A note on static vignettes

Static vignette won't be compiled during build time and thus allows the presentation of real life example in IsoriX that would be too long for CRAN or users otherwise.

## How to make static vignettes?

* just paste the html or pdf in the folder vignette in the package and copy the Rmd file after renaming it sources_for_nameofthevignette.Rmd in inst/doc/

* create the asis file (see pdf in this folder for how to)

* if vignettes are pdfs, you can reduce their size by creating png pictures at 100 dpi and by comrpessing the pdfs using Ghostscript. The can be done easily in R by typing:

```{r}
tools::compactPDF(getwd(), gs_quality="ebook")
```

