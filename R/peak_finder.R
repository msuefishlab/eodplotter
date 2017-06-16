#!/usr/bin/env Rscript

library(tdmsreader)

#' Find peaks from a TDMS file
#' @export
#'
#' @param filename The filename
#' @param channel The channel name, default /'Untitled'/'Dev1/ai0' which is just common in our lab
#' @param direction Only get positive or negative peaks
#' @param threshold Threshold for cutoff
#' @param remove Remove N seconds from start and end of recording
#' @param verbose Verbose output
peakFinder <- function(filename, channel="/'Untitled'/'Dev1/ai0'", direction="none", threshold=5, remove=0, verbose=F) {
    m = file(filename, 'rb')
    main = TdmsFile$new(m)

    c = ifelse(is.null(channel), "/'Untitled'/'Dev1/ai0'", channel)
    r = main$objects[[c]]
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

    if(verbose) {
        cat(sprintf("finding peaks for %s\n", filename))
    }
    peaks = data.frame(peaks=numeric(), direction=character(), stringsAsFactors=F)
    for(i in seq(1,length(dat),by=3)) {
        ns = max(i - 1000,1)
        ne = i + 1000
        
        if(verbose & i%%100000==0) {
            cat(sprintf("\rprogress %d%%",round(100*i/length(dat))))
        }
        if(t[i] - currTime > 0.001) {
            if(dat[i] > mymean + mysd * threshold) {
                loc_max = which.max(dat[ns:ne])
                loc_min = which.min(dat[ns:ne])
                if(loc_min>=loc_max) {
                    peaks = rbind(peaks, data.frame(peaks=t[ns + loc_max], direction='+',stringsAsFactors=F))
                    currTime = t[i]
                }
            } else if(dat[i] < mymean - mysd * threshold) {
                loc_max = which.max(dat[ns:ne])
                loc_min = which.min(dat[ns:ne])
                if(loc_max>=loc_min) {
                    peaks = rbind(peaks, data.frame(peaks=t[ns + loc_min], direction='-',stringsAsFactors=F))
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
    peaks
}
