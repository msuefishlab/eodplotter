library(tdmsreader)

#' Plot the signal vs time for a TDMS file
#' @export
#'
#' @param filename The filename
#' @param channel The channel name, default /'Untitled'/'Dev1/ai0' which is just common in our lab
#' @param start Default 0
#' @param end Default end of tdms file data
plotTdms <- function(filename, channel = "/'Untitled'/'Dev1/ai0'", start = 0, end = NULL, peaks = NULL) {
    m = file(filename, 'rb')
    main = TdmsFile$new(m)
    c = ifelse(is.null(channel), "/'Untitled'/'Dev1/ai0'", channel)

    r = main$objects[[c]]
    if(is.null(r)) {
        stop('Channel not found')
    }
    max = r$number_values * r$properties[['wf_increment']]
    e = ifelse(is.null(end), max, end)
    s = ifelse(is.null(start), 0, start)

    main$read_data(m, s, e)
    t = r$time_track(start = s, end = e)
    dat = r$data
    close(m)
    if(e - s > 20) {
        dat2 = dat[seq(1, length(dat), by = 10)]
        t = t[seq(1, length(t), by = 10)]
        dat2 = dat2[1:length(t)]
        plot(t, dat2, type = 'l', xlab = 'time', ylab = 'volts')
    } else if(e - s > 10) {
        dat2 = dat[seq(1, length(dat), by = 5)]
        t = t[seq(1, length(t), by = 5)]
        dat2 = dat2[1:length(t)]
        plot(t, dat2, type = 'l', xlab = 'time', ylab = 'volts')
    } else {
        plot(t, dat, type = 'l', xlab = 'time', ylab = 'volts')
    }
    if(!is.null(peaks)) {
        peaks = peaks[peaks[,1]>s & peaks[,1]<e,]
        vals = peaks[,1] / r$properties[['wf_increment']]
        v = dat[vals-s/r$properties[['wf_increment']] ]
        points(peaks[,1], v, col='red', pch=20)
    }
    title(filename)
}
