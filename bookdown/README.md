# Sources for the bookdown about IsoriX

This is the development folder for the documentation about IsoriX.

You can find the compiled (i.e., readable) version of this documentation
[here](https://courtiol-isorix.share.connect.posit.cloud/).

## Notes for developers

This bookdown follows the model merge and knit, rather than knit and
merge. As a consequence, do not attempt to knit individual chapters, it
won’t work. Instead, you must render the whole thing at once.

Before doing that, do make sure to upload all your packages and to
delete the content of the folder “output”; otherwise the bookdown will
use objects created with old versions of packages.

Do make sure however that the folder “output” exists, otherwise objects
won’t be saved.

### Workflow for preparing an update

Make sure that you have the following packages installed (but no need to
load them):

    if (!"bookdown" %in% installed.packages()[, 1]) install.packages("bookdown")
    if (!"rsconnect" %in% installed.packages()[, 1]) install.packages("rsconnect")
    if (!"servr" %in% installed.packages()[, 1]) install.packages("servr")
    if (!"gifski" %in% installed.packages()[, 1]) install.packages("gifski")
    if (!"kableExtra" %in% installed.packages()[, 1]) install.packages("kableExtra")
    if (!"ggplot2" %in% installed.packages()[, 1]) install.packages("ggplot2")
    if (!"dplyr" %in% installed.packages()[, 1]) install.packages("dplyr")
    if (!"sf" %in% installed.packages()[, 1]) install.packages("sf")
    if (!"stars" %in% installed.packages()[, 1]) install.packages("stars")
    if (!"tidyterra" %in% installed.packages()[, 1]) install.packages("tidyterra")
    if (!"Cairo" %in% installed.packages()[, 1]) install.packages("Cairo")

Then, render the full book once, so as to create all the content stored
in the folder “output”.

For this, use:

    bookdown::render_book("index.Rmd", "bookdown::gitbook")

Importantly, this job should not break even if you work on a remote
computer and that your connection to RStudio Server is not kept alive
the whole time.

Once this is done, you can check the output by opening
“IsoriX\_project/bookdown/docs/index.html” (if it did not open
automatically).

Then, if using RStudio, you may serve the book (it also creates the gif
of the starting page):

    bookdown::serve_book()

This displays the bookdown in RStudio and refreshes on the fly any time
you save your edits on a chapter. This is much more convenient than
calling `render_book()` repeatedly, but it breaks as soon as the
connection with RStudio Server is interrupted.

Once you are satisfied with the results, pull, commit and push the
changes and then publish the content to update the website using:

    rsconnect::connectCloudUser()
    bookdown::publish_book(name = "IsoriX")

Check the bookdown online.

### Do not cash anything

If some steps are particularly slow, instead of relying on knitr
cashing, it is best to store the created objects in the folder “output”
and to use code similar to this:

    if (file.exists("output/some_object_slow_to_create.rds")) {
      some_object_slow_to_create <- readRDS("output/some_object_slow_to_create.rds")
    } else {
      some_object_slow_to_create <- slow_fn()
      saveRDS(some_object_slow_to_create, file = "output/some_object_slow_to_create.rds", compress = "xz")
    }

### Pandoc issues?

If a pandoc error shows up, make sure that pandoc is installed and that
RStudio can find it. For RStudio to find pandoc, I had to create links
by hand on one computer (but usually it works automatically):

    sudo ln -s -f /usr/bin/pandoc /usr/lib/rstudio/bin/pandoc/pandoc
    sudo ln -s -f /usr/bin/pandoc-citeproc /usr/lib/rstudio/bin/pandoc/pandoc-citeproc
