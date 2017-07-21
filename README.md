# eodplotter

Plot EODs from a tdms file

[![Build Status](https://travis-ci.org/msuefishlab/eodplotter.svg?branch=master)](https://travis-ci.org/msuefishlab/eodplotter)


User guide: https://msuefishlab.github.io/eodplotter/index.html


## Install

    install.packages('devtools')
    devtools::install_github('msuefishlab/eodplotter')

## Notes

Builds off the msuefishlab/tdmsreader. Also see msuefishlab/tdmsviewer


## User guide

User guide/package vignette here: https://msuefishlab.github.io/eodplotter/index.html


## Useful helpers for bulk processing


### Add command line scripts to your path

```
export PATH=$PATH:`Rscript -e 'cat(paste0(.libPaths(),"/eodplotter/scripts",collapse=":"))'`:~/bin
```


