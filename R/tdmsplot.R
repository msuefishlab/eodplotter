library(tdmsreader)

function plotTdms(filename, channel = "/'Untitled'/'Dev/ai0'", start = 0, end = NULL) {
    m = file(filename, 'rb')
    main = TdmsFile$new(m)

    r = main$objects[[channel]]
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
        dat = dat[seq(1, length(dat), by = 10)]
        t = t[seq(1, length(t), by = 10)]
        dat = dat[1:length(t)]
        plot(t, dat, type = 'l', xlab = 'time', ylab = 'volts')
    } else if(e - s > 10) {
        dat = dat[seq(1, length(dat), by = 5)]
        t = t[seq(1, length(t), by = 5)]
        dat = dat[1:length(t)]
        plot(t, dat, type = 'l', xlab = 'time', ylab = 'volts')
    } else {
        plot(t, dat, type = 'l', xlab = 'time', ylab = 'volts')
    }
    title(filename)
}
