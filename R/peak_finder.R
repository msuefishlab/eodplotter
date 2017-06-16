#!/usr/bin/env Rscript

library(tdmsreader)

peakFinder <- function(filename, channel, type, number, remove, verbose) {
    m = file(filename, 'rb')
    main = TdmsFile$new(m)

    r = main$objects[[channel]]
    if(is.null(r)) {
        stop('Channel not found')
    }
    total_vals = r$number_values * r$properties[['wf_increment']]
    e = total_vals - remove
    s = remove

    main$read_data(m, s, e)
    t = r$time_track(start = s, end = e)
    dat = r$data
    close(m)

    mysd = sd(dat)
    mymean = mean(dat)
    currTime = 0

    if(direction == 'none' & type == 'volts') {
        stop('Need to specify direction if using voltage cutoff')
    }

    if(verbose) {
        cat(sprintf("finding peaks for %s\n", filename))
    }
    peaks = data.frame(peaks=numeric(), direction=character())
    for(i in seq(1,length(dat),by=3)) {
        ns = max(i - 1000,1)
        ne = i + 1000
        
        if(verbose & i%%100000==0) {
            cat(sprintf("\rprogress %d%%",round(100*i/length(dat))))
        }
        if(t[i] - currTime > 0.001) {
            if(dat[i] > mymean + mysd * number) {
                loc_max = which.max(dat[ns:ne])
                loc_min = which.min(dat[ns:ne])
                if(loc_min>=loc_max) {
                    peaks = rbind(peaks, data.frame(peaks=t[ns + loc_max], direction='+'))
                    currTime = t[i]
                }
            } else if(dat[i] < mymean - mysd * number) {
                loc_max = which.max(dat[ns:ne])
                loc_min = which.min(dat[ns:ne])
                if(loc_max>=loc_min) {
                    peaks = rbind(peaks, data.frame(peaks=t[ns + loc_min], direction='-'))
                    currTime = t[i]
                }
            }
        }
    }

    if(verbose) {
        cat("\r", sprintf("progress 100%%\n"))
        cat("done scanning\n")
    }
    if(nrow(peaks)==0) {
        cat(sprintf('No peaks found in %s\n', filename))
    }
    write.table(peaks, file=paste0(basename(filename),'.peaks.csv'), quote=F, row.names=F, sep='\t', col.names=F)
    peaks
}
