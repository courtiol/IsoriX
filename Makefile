PKG := IsoriX
VERSION := $(shell grep Version DESCRIPTION | cut -d " " -f 2)
PKGFILE := $(PKG)_$(VERSION).tar.gz

build:
	R CMD build .

checkcode:
	Rscript -e "if(require(IsoriX)) codetools::checkUsagePackage('IsoriX', skipWith=TRUE)"

quickcheck:
	R CMD check $(PKGFILE)

check:
	R CMD check --as-cran $(PKGFILE)

fullcheck:
	R CMD check --as-cran --run-dontrun $(PKGFILE)

clean:
	-rm -r src/*.{o,rds,so} $(PKG)_*.tar.gz $(PKG).Rcheck *.Rout .RData 2> /dev/null

install: $(PKGFILE)
	R CMD INSTALL $(PKGFILE)

$(PKGFILE): build

remove:
	R CMD REMOVE $(PKG)

test:
	Rscript -e "devtools::test()"
	-rm testthat.Rout tests/testthat/Rplots.pdf> /dev/null

reinstall: remove install
