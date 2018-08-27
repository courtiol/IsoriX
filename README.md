# Welcome to IsoriX
[![Build Status](https://travis-ci.org/courtiol/IsoriX.svg?branch=master)](https://travis-ci.org/courtiol/IsoriX_project)
[![CRAN](http://www.r-pkg.org/badges/version/IsoriX)](https://cran.r-project.org/web/packages/IsoriX)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/IsoriX?color=brightgreen)](http://www.r-pkg.org/pkg/IsoriX)
[![Rdoc](http://www.rdocumentation.org/badges/version/IsoriX)](http://www.rdocumentation.org/packages/IsoriX)
[![codecov](https://codecov.io/gh/courtiol/IsoriX/branch/master/graph/badge.svg)](https://codecov.io/gh/courtiol/IsoriX)



This is the development repository of __IsoriX__, an [R](https://www.r-project.org/) package aiming at building isoscapes using mixed models and inferring the geographic origin of organisms based on their isotopic ratios.

![isoscape](image/isoscape.png)

## How to download and install IsoriX?
You can download and install the stable version of IsoriX directly from within R by typing:

```R
install.packages("IsoriX")
```

If you want the development version of IsoriX, you can download and install it by typing:

```R
devtools::install_github("courtiol/IsoriX_project/IsoriX")
```

Mind that the development version, being under development, can sometimes be broken. So before downloading it make sure that the current build satus is ![build passing](image/build_passing.png). The current built status is provided at the top of this readme document.

Also, if you access the network via a proxy, you may experience troubles with ```install_github```. In such case try something like:

```R
library(httr)
with_config(use_proxy("192.168.2.2:3128"), devtools::install_github("courtiol/IsoriX_project/IsoriX"))
```

Off course, unless you are in the same institute than some of us, replace the numbers with your proxy settings!


## Where to learn about IsoriX?

You can access the documentation and tutorials about IsoriX using our [bookdown](https://bookdown.org/content/782/).

For more specific help on IsoriX functions and objects, simply type:

```R
help(package="IsoriX", help_type="html")
```
in R after having installed and loaded the package.


## How can you contribute?
There are plenty way you can contribute! If you are fluent in R programming, you can improve the code and develop new functions. If you are not so fluent, you can still edit the documentation files to make them more complete and clearer, write new vignettes, report bugs or make feature requests.

## Some useful links

* about making R packages:

[Simple introduction to the making of R packages](http://r-pkgs.had.co.nz/)

[Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)

[R coding standard](https://google.github.io/styleguide/Rguide.xml)

* about the environment to develop IsoriX:

[Using version control in RStudio](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN)

* about other R packages on which IsoriX strongly depends:

[spaMM](http://kimura.univ-montp2.fr/~rousset/spaMM.htm)

[rasterVis](https://oscarperpinan.github.io/rastervis/)

## Authors
* [Alexandre Courtiol](https://sites.google.com/site/alexandrecourtiol/home)
* [François Rousset](http://www.isem.univ-montp2.fr/recherche/teams/evolutionary-genetics/staff/roussetfrancois/?lang=en)
* [Stephanie Kramer-Schadt](https://www.researchgate.net/profile/Stephanie_Kramer-Schadt)
