# Welcome!

This is the development repository of __IsoriX__, an [R](https://www.r-project.org/) package aiming at building isoscapes using mixed models and inferring the geographic origin of organisms based on their isotopic ratios.

![isoscape](image/isoscape.png)

## How to download and install IsoriX?
You can download and install the stable version of IsoriX directly from within R by typing:

```
#!R
install.packages("IsoriX")
```

You can download and install the development version of IsoriX by typing:

```
#!R
devtools::install_bitbucket("courtiol/IsoriX_project/IsoriX")
```

## Where to learn about IsoriX?

Here, on bitbucket, we are planning to provide documentation in [our Wiki](https://bitbucket.org/courtiol/isorix_project/wiki/Home). It is just starting, so check regularly for updates.

Otherwise, you can access the documentation and tutorials about IsoriX on [CRAN](https://cran.r-project.org/web/packages/IsoriX/index.html), or if you have installed IsoriX you can type:

```
#!R
help(package="IsoriX", help_type="html")
```
to access the help files of all exported IsoriX functions and objects. Or you can type:

```
#!R
browseVignettes(package="IsoriX")
```
to find our tutorials.


## How can you contribute?
There are plenty way you can contribute! If you are fluent in R programming, you can improve the code and develop new functions. If you are not so fluent, you can still edit the documentation files to make them more complete and clearer, write new vignettes, report bugs or make feature requests.

## Some useful links

* about making R packages:

[Simple introduction to the making of R packages](http://r-pkgs.had.co.nz/)

[Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)

[R coding standard](https://google.github.io/styleguide/Rguide.xml)

* about the environment to develop IsoriX:

[Learning git](https://www.atlassian.com/git/tutorials/)

[Using version control in RStudio](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN)

[Markdown in Bitbucket](https://bitbucket.org/tutorials/markdowndemo)

* about other R packages on which IsoriX strongly depends:

[spaMM](http://kimura.univ-montp2.fr/~rousset/spaMM.htm)

[rasterVis](https://oscarperpinan.github.io/rastervis/)

## Authors
* [Alexandre Courtiol](https://sites.google.com/site/alexandrecourtiol/home)
* [Stephanie Kramer-Schadt](https://www.researchgate.net/profile/Stephanie_Kramer-Schadt)
* [François Rousset](http://www.isem.univ-montp2.fr/recherche/teams/evolutionary-genetics/staff/roussetfrancois/?lang=en)