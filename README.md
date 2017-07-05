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


### Process all files in a folder

For example, save this script and run e.g. process.sh ~/path/to/tdms/files/*.tdms

```
#!/usr/bin/env bash

parallel "tdmsplot -f {}" ::: "$1"
parallel "peak_finder -f {} -v" ::: "$1"
parallel "eodplot -f {} -p {/}.peaks.csv" ::: "$1"
```


### Combine all stats.csv

Run this script in directory full of stats.csv files e.g. 'Rscript combine.r'

```
#!/usr/bin/env Rscript


myfiles=list.files(pattern='*.stats.csv')
print(myfiles)

mycsvs = lapply(myfiles, read.csv)
names(mycsvs)=myfiles
print(mycsvs)


for(i in 1:length(mycsvs)) {
   mycsvs[[i]]$name = myfiles[i]
}



mytable = do.call(rbind, mycsvs)

write.csv(mytable, 'combined.data.csv', quote=F,row.names=F)

print('combined files into combined.data.csv')
```



### Plot EOD amplitude from different experiments

This will plot the amplitude from multiple experiments where it assumes some particular file naming conventions e.g. 

1_MOCntrl1_Baseline_2017_06_29_07_34_08.tdms
2_MOCntrl1_30_2017_06_29_08_21_23.tdms
3_MOCntrl1_60_2017_06_29_09_00_21.tdms

Generic pattern <number>_MO<experimentname>_minutes_timestamp.tdms

```
library(stringr)
library(lubridate)
library(ggplot2)
x=read.csv('combined.data.csv',stringsAsFactors=F)

x$date=str_match(x$name,'(2017.*).tdms')[,2]
x$trial=str_match(x$name,"([0-9]+)_")[,2]
x$group=str_replace(tolower(str_match(x$name,'(MO.[a-zA-Z0-9]+)')[,2]), '_','')
x$date=ymd_hms(x$date)


png('combined.fixed.png', width=1000,height=800)
ggplot(x,aes(date,amplitude,color=group))+geom_line()
dev.off()

write.csv(x, 'combined.fixed.csv',row.names=F,quote=F)
```
