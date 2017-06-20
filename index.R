## ----eval=F--------------------------------------------------------------
#  install.packages('devtools')

## ----eval=F--------------------------------------------------------------
#  devtools::install_github('msuefishlab/eodplotter')

## ----eval=F--------------------------------------------------------------
#  install_github('msuefishlab/eodplotter@0.0.2')

## ------------------------------------------------------------------------
library(eodplotter)
print(getwd())
plotTdms('../tests/testthat/file.tdms')

