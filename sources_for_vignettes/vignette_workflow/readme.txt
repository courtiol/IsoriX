just paste the html or pdf in the folder vignette in the package and copy the Rmd file after renaming it sources_for_nameofthevignette.Rmd in inst/doc/

if pdf, you can reduce the size massivelly by using:
tools::compactPDF(getwd(), gs_quality="ebook")
