PKG := IsoriX
VERSION := $(shell grep Version $(PKG)/DESCRIPTION | cut -d " " -f 2)
PKGFILE := $(PKG)_$(VERSION).tar.gz

build:
	R CMD build $(PKG)

checkcode:
	Rscript -e "if(require(IsoriX)) codetools::checkUsagePackage('IsoriX', skipWith=TRUE)"

quickcheck:
	R CMD check $(PKGFILE)

check:
	R CMD check --as-cran $(PKGFILE)

fullcheck:
	R CMD check --as-cran --run-dontrun $(PKGFILE)

clean:
	-rm -r $(PKG)/src/*.{o,rds,so} $(PKG)_*.tar.gz $(PKG).Rcheck *.Rout .RData 2> /dev/null

install: $(PKGFILE)
	R CMD INSTALL $(PKGFILE)

$(PKGFILE): build

remove:
	R CMD REMOVE $(PKG)

test:
	Rscript -e "setwd('./IsoriX'); devtools::test()"
	-rm testthat.Rout IsoriX/tests/testthat/Rplots.pdf> /dev/null

reinstall: remove install
