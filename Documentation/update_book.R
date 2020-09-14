## Run the following to build the book and upload it!

## Note: mind that if you experience issues, it could be that the cache is outdated and so knitr
## tries to access packages in folders that no longer exist and so forth. Therefore, it could be a
## good idea to delete the cache folder. You may also want to delete the rda files in output to
## recreate all large objects create by the package to make sure everything does work. Finally,
## since the gif of the showcase shown in the starting page is based on the compilation of all other
## files, it is probably best to render twice the whole book.

bookdown::render_book('index.Rmd', output_format = 'bookdown::gitbook')
bookdown::publish_book(name = "IsoriX")
