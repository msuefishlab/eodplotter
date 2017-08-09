#!/usr/bin/env Rscript

library(tdmsreader)

#' Find peaks from a TDMS file
#' @export
#' @import zoo
#'
#' @param filename The filename
#' @param channel The channel name, default /'Untitled'/'Dev1/ai0' which is just common in our lab
#' @param direction Only get positive or negative peaks
#' @param threshold Threshold for cutoff
#' @param remove Remove N seconds from start and end of recording, mutually exclusive to start/end
#' @param start Start of rec, mutually exclusive to remove
#' @param end End of rec
#' @param verbose Verbose output
#' @param progressCallback Callback for progress event update
peakFinder <- function(filename, channel="/'Untitled'/'Dev1/ai0'", direction = "none", threshold = 5, start = NULL, end = NULL, remove = NULL, verbose = F, progressCallback = NULL) {
    m = file(filename, 'rb')
    main = TdmsFile$new(m)

    c = ifelse(is.null(channel), "/'Untitled'/'Dev1/ai0'", channel)
    r = main$objects[[c]]
    if(is.null(r)) {
        stop('Channel not found')
    }
    total_vals = r$number_values * r$properties[['wf_increment']]
    s = 0
    e = total_vals
    if(!is.null(remove)) {
        e = e - remove
        s = s + remove
    } else {
        e = ifelse(is.null(end), e, end)
        s = ifelse(is.null(start), 0, start)
    }

    main$read_data(m, s, e)
    t = r$time_track(start = s, end = e)
    dat = r$data
    close(m)

    #mysd = sd(dat)
    currTime = 0

    if(verbose) {
        cat(sprintf("finding peaks for %s\n", filename))
    }
    peaks = data.frame(peaks=numeric(), direction=character(), stringsAsFactors=F)
    winsize = 3000
    // fast rolling mean
    mymeans = rollmean(dat, winsize)
    // fast sd calculation https://stackoverflow.com/questions/24066085/rolling-standard-deviation-in-a-matrix-in-r
    mysds = sqrt((winsize/(winsize-1)) * (rollmean(dat^2, winsize) - mymeans^2))

    for(i in seq(winsize+1, length(dat) - winsize, by = 3)) {
        ns = max(i - 1000, 1)
        mymean = mymeans[i]
        ne = i + 1000
        mysd = mysds[i]
        if(i %% 100000 == 0) {
            if(verbose) {
                cat(sprintf("\rprogress %d%%", round(100 * i / length(dat))))
            }
            if(!is.null(progressCallback)) {
                progressCallback(i / length(dat))
            }
        }
        if(t[i] - currTime > 0.001) {
            #cat(paste('mymean',mymean, 'dat',dat[i],'t',mymean+mysd*threshold,'\n'))
            if(dat[i] > mymean + mysd * threshold) {
                loc_max = which.max(dat[ns:ne])
                loc_min = which.min(dat[ns:ne])
                if(loc_min >= loc_max) {
                    peaks = rbind(peaks, data.frame(peaks = t[ns + loc_max], direction = '+', stringsAsFactors = F))
                    currTime = t[i]
                }
            } else if(dat[i] < mymean - mysd * threshold) {
                loc_max = which.max(dat[ns:ne])
                loc_min = which.min(dat[ns:ne])
                if(loc_max >= loc_min) {
                    peaks = rbind(peaks, data.frame(peaks = t[ns + loc_min], direction = '-', stringsAsFactors = F))
                    currTime = t[i]
                }
            }
        }
    }

    if(verbose) {
        cat("\r", sprintf("progress 100%%\n"))
        cat("done scanning\n")
    }
    if(nrow(peaks) == 0) {
        cat(sprintf('No peaks found in %s\n', filename))
    }
    peaks
}
