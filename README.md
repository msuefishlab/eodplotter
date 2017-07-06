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

- 1_MOCntrl1_Baseline_2017_06_29_07_34_08.tdms
- 2_MOCntrl1_30_2017_06_29_08_21_23.tdms
- 3_MOCntrl1_60_2017_06_29_09_00_21.tdms

Generic pattern `<number>_MO<experimentname>_minutes_timestamp.tdms`

```
library(stringr)
library(ggplot2)
suppressMessages(library(lubridate))
combined=read.csv('combined.data.csv',stringsAsFactors=F)

l = str_split(substr(combined$name,0,str_locate(combined$name,'2017')[,1]-2),'_')
combined$trial = as.numeric(sapply(l,function(r) r[1]))
combined$group = tolower(sapply(l,function(r) r[2]))
m = sapply(l, function(r) r[3])
m[m == 'Baseline'] = 0
combined$timepoint = as.numeric(m)
combined$date = ymd_hms(str_replace(substring(combined$name, str_locate(combined$name,'2017')[,1]), '.tdms.stats.csv',''))
 
png('combined.fixed.png', width=1000,height=800)
ggplot(combined, aes(timepoint, amplitude, color = group)) + geom_line() + ggtitle('MO EOD amplitude') + scale_x_continuous(name="Timepoint") + scale_y_continuous(name="Peak-to-peak amplitude") +scale_colour_brewer(palette="Set2")
invisible(dev.off())
pdf('combined.fixed.pdf', width=11,height=8)
ggplot(combined, aes(timepoint, amplitude, color = group)) + geom_line() + ggtitle('MO EOD amplitude') + scale_x_continuous(name="Timepoint") + scale_y_continuous(name="Peak-to-peak amplitude") +scale_colour_brewer(palette="Set2")
invisible(dev.off())



ret = do.call(rbind, lapply(unique(combined$group), function(group) {
    r = combined[combined$group == group, ]
    r$relative_amplitude = r$amplitude / r[r$timepoint == 0, ]$amplitude
    r
}))



png('baseline.fixed.png', width=1000,height=800)
ggplot(ret, aes(timepoint, relative_amplitude, color = group)) + geom_line() + ggtitle('MO EOD amplitude (relative to baseline)') + scale_x_continuous(name="Timepoint") + scale_y_continuous(name="Relative amplitude") +scale_colour_brewer(palette="Set2")
invisible(dev.off())
pdf('baseline.fixed.pdf', width=11,height=8)
ggplot(ret, aes(timepoint, relative_amplitude, color = group)) + geom_line() + ggtitle('MO EOD amplitude (relative to baseline)') + scale_x_continuous(name="Timepoint") + scale_y_continuous(name="Relative amplitude") +scale_colour_brewer(palette="Set2")
invisible(dev.off())

write.csv(ret, 'combined.fixed.csv', row.names = F, quote = F)
```


### MultiEodPlotter pipeline

Output a list of peaks and tdms files and combine them for the input into multieodplot

```
ls -1 *exp2_*.peaks.csv > peaks.csv; 
ls -1 *exp2_*.tdms > tdms.csv;
paste -d',' tdms.csv peaks.csv > file.csv
multieodplot -f file.csv -b
```




