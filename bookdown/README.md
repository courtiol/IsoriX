# Sources for the bookdown about IsoriX

This is the development folder for the documentation about IsoriX.

You can find the compiled (ie. readable) version of this documentation [here](https://bookdown.org/content/782/).

## Notes for developers

This bookdown follows the model merge and knit, rather than knit and merge.
As a consequence, do not attempt to knit individual chapters, it won't work.
Instead, you must render the whole thing at once.
Before doing that, do make sure to upload all your packages and to delete the content of the folder "output"; otherwise the bookdown will use objects created with old versions of packages.

### Workflow for preparing an update

Make sure that you have the following packages installed (but no need to load them): rsconnect, servr.

Then, render the full book once, so as to create all the content stored in the folder "output".

For this, use:
```r
bookdown::render_book("index.Rmd", "bookdown::gitbook")
```

Importantly, this job should not break even if you work on a remote computer and that your connection to RStudio server is not kept alive the whole time.

Once this is done, you can check the output by opening "IsoriX_project/Documentation/docs/index.html" if it did not open automatically.

Then, you should serve the book:
```r
bookdown::serve_book()
```

This will display the bookdown in RStudio and refresh on the fly an time you save your edits on a chapter.
This is much more convenient than calling `render_book()` repeatedly, but it breaks as soon as the connection with RStudio Server is interrupted.

Once you are satisfied with the results, you may want to delete once again the content of the folder "output" and render (not serve) the book again.

Pull, commit and push the changes and then publish the content to update the website using:

```r
bookdown::publish_book(name = "IsoriX")
```

Check the bookdown online.


### Pandoc issues?

If a pandoc error shows up, make sure that pandoc is installed and that RStudio can find it.
For RStudio to find pandoc, I had to create links by hand on one computer (but usually it works automatically):

```bash
sudo ln -s -f /usr/bin/pandoc /usr/lib/rstudio/bin/pandoc/pandoc
sudo ln -s -f /usr/bin/pandoc-citeproc /usr/lib/rstudio/bin/pandoc/pandoc-citeproc
```
