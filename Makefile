
.PHONY: all
all: README.md \
     documentation

## Update README

README.md : README.rmd
	Rscript -e 'knitr::knit("README.Rmd")'



## Documentation

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

