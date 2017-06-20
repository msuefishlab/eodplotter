## ----eval=F--------------------------------------------------------------
#  install.packages('devtools')

## ----eval=F--------------------------------------------------------------
#  devtools::install_github('msuefishlab/eodplotter')

## ----eval=F--------------------------------------------------------------
#  install_github('msuefishlab/eodplotter@0.0.2')

## ------------------------------------------------------------------------
library(eodplotter)
plotTdms('../tests/testthat/file.tdms')

## ------------------------------------------------------------------------
p = peakFinder('../tests/testthat/file.tdms')
print(head(p))

