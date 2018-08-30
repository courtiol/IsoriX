## run the following to build the book and upload it
bookdown::render_book('index.Rmd', output_format = 'bookdown::gitbook')
bookdown::publish_book(name = "IsoriX")
