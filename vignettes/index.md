---
title: "EODPlotter manual"
author: "Colin Diesh"
date: "2017-06-20"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


# EODPlotter User Manual

## Source Code

https://github.com/msuefishlab/eodplotter

## Installation

EODPlotter is designed as an R package, to be installed using a package
manager. EODPlotter is a command line alternative to the [TDMSViewer](https://github.com/msuefishlab/tdmsviewer)

We will install EODPlotter from github. Open RStudio and run


```r
install.packages('devtools')
```

This will install the devtools package which has the `install_github` function. Then run


```r
devtools::install_github('msuefishlab/eodplotter')
```

After running this it should install the EODViewer code and its
dependencies automatically. 

To install a specific version, e.g. the version mentioned in this guide, you
can also run


```r
install_github('msuefishlab/eodplotter@0.0.2')
```
## Using EODPlotter

Once the library is installed, you can use it using either


- Command line
- On R interactive prompt
  
### Usage

Plot the raw signal of the data using plotTdms


```r
library(eodplotter)
plotTdms('../tests/testthat/file.tdms')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Find peaks


```r
p = peakFinder('../tests/testthat/file.tdms')
print(head(p))
```

```
##     peaks direction
## 1 0.08696         +
## 2 0.10198         +
## 3 0.48870         +
## 4 0.87200         +
## 5 1.04231         +
## 6 1.16931         +
```

Get a matrix of EOD signal


```r
m = getEODMatrix('../tests/testthat/file.tdms', p)
print(head(m))
```

```
##       col  time       data
## 1 0.08696 0e+00 0.01767369
## 2 0.08696 1e-05 0.01701586
## 3 0.08696 2e-05 0.01931826
## 4 0.08696 3e-05 0.01734477
## 5 0.08696 4e-05 0.01767369
## 6 0.08696 5e-05 0.01931826
```

Plot the matrix of EOD signals


