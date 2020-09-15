## Run the following to build the book and upload it!

## Note: mind that if you experience issues, it could be that the cache is outdated and so knitr
## tries to access packages in folders that no longer exist and so forth. Therefore, it could be a
## good idea to delete the cache folder and not rely on it. Only use the cache, to modify text only
## once everything runs smoothly! You may also want to delete the rda files in output to recreate
## all large objects create by the package to make sure everything does work. Finally, since the gif
## of the showcase shown in the starting page is based on the compilation of all other files, it is
## probably best to render twice the whole book. Also, if a pandoc error shows up, make sure that
## pandoc is installed and that RStudio can find it. I had to create links by hand on one computer:
## sudo ln -s -f /usr/bin/pandoc /usr/lib/rstudio/bin/pandoc/pandoc sudo ln -s -f
## /usr/bin/pandoc-citeproc /usr/lib/rstudio/bin/pandoc/pandoc-citeproc


bookdown::render_book('index.Rmd', output_format = 'bookdown::gitbook')
bookdown::publish_book(name = "IsoriX")
