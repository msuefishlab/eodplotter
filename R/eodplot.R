library(tdmsreader)



#' Get EOD matrix
#' @export
#' @import tdmsreader
#'
#' @param filename The filename
#' @param peaks A data.frame of peaks (time, direction +/-)
#' @param channel The channel name, default /'Untitled'/'Dev1/ai0' which is just common in our lab
#' @param prebaseline Subtract baseline pre normalization
#' @param postbaseline Subtract baseline post normalization
#' @param normalize Normalize data to 0-1
#' @param alpha Alpha channel for all EODs plot
#' @param window Window size
#' @param verbose Set verbose output
getEODMatrix <- function(filename, peaks, channel = "/'Untitled'/'Dev1/ai0'", prebaseline = F, postbaseline = F, normalize = F, alpha = F, window = 0.005, verbose = F) {
    if(nrow(peaks)==0) {
        print('No peaks found')
        return(NULL)
    }
    m = file(filename, 'rb')
    main = TdmsFile$new(m)
    c = ifelse(is.null(channel), "/'Untitled'/'Dev1/ai0'", channel)
    r = main$objects[[c]]
    if(is.null(r)) {
        stop('Channel not found')
    }
    inc = r$properties[['wf_increment']]
    max = r$number_values * inc
    main$read_data(m, 0, max)
    close(m)

    if(verbose) {
        cat('gathering peak data...\n')
    }
    peakdata = apply(peaks, 1, function(row) {
        start = as.numeric(row[[1]])
        s = start - window / 2
        e = start + window / 2
        sp = s/inc
        ep = e/inc
        dat = r$data[sp:ep]
        t = seq(s, e, by = inc) - start
        t = t[1:length(dat)]

        if(row[[2]] == '-') {
            dat = -dat
        }
        if(prebaseline) {
            dat = dat - mean(dat[1:25])
        }
        if(normalize) {
            dat = (dat - min(dat)) / (max(dat) - min(dat))
        }
        if(postbaseline) {
            dat = dat - mean(dat[1:25])
        }
        # rounding important here to avoid different values being collapsed. significant digits may change on sampling rate of tdms
        data.frame(col = start, time = round(t, digits=6), data = dat)
    })
    if(verbose) {
        cat('combining data frames...\n')
    }
    do.call(rbind, peakdata)
}



#' Plot EOD signal with landmarks
#' @export
#' @import reshape2
#'
#' @param plotdata The EOD matrix from getEODMatrix
findLandmarks <- function(plotdata) {

    #reorganize data into matrix (rows = each eod, column each point)
    ret = acast(plotdata, time ~ col, value.var = 'data', fun.aggregate = mean)
    neods<-dim(ret)[2]
    npoints<-dim(ret)[1]

    # calculate average waveform
    avg = apply(ret, 1, mean)
    avg = avg[1:(length(avg)-1)]
    data = data.frame(time = as.numeric(names(avg)), voltage = as.numeric(avg))
    data = data[1:nrow(data)-1,]

    #find p1 in average waveform
    p1pos = which.max(data$voltage)

    #calculate waveform  voltage @ P1
    p1 = data[p1pos, ]
    p1$index=p1pos


    #find p1 in each individual waveform
    p1pos_e = apply(ret,2,which.max)
    p1pos_e = data.frame(time = as.numeric(names(p1pos_e)), index = as.numeric(p1pos_e))

    #calculate waveform voltage @ each P1
    p1_e<-data.frame(time = p1pos_e$time, voltage = ret[,1:neods][p1pos_e$index],index = p1pos_e$index )

    #find p2 in average waveform
    p2pos = which.min(data$voltage)

    #calcualte waveform voltage @ P2
    p2 = data[p2pos, ]
    p2$index=p2pos
    

    #find p2 in each individual waveform
    p2pos_e = apply(ret,2,which.min)
    p2pos_e = data.frame(time = as.numeric(names(p2pos_e)), index = as.numeric(p2pos_e))


    #calculate waveform voltage @ each P2
    p2_e<-data.frame(time = p2pos_e$time, voltage = ret[,1:neods][p2pos_e$index], index=p2pos_e$index)


    avg_p1_p2_data<-data.frame(p1=p1,p2=p2,p1_i=as.numeric(rownames(p1)),p2_i=as.numeric(rownames(p2)))


    # drop in for species-specific stuff
    #lm_av<-NULL
    #lm_av<-findmormyridlandmarks(data,p1$index,p2$index,p1$voltage,p2$voltage,25)

    lm_raw<-NULL
    for (i in 1:neods) {
      lm_raw[[i]]<-findmormyridlandmarks(data.frame(time=as.numeric(names(ret[,i])),voltage=(ret[,i])),p1_e$index[i],p2_e$index[i],p1_e$voltage[i],p2_e$voltage[i], 25)
      lm_raw[[i]]$eodno<-i
    }
    
    #merge list
    lm_raw<-do.call(rbind,lm_raw)
    
    #melt into molten frame
    a<-melt(lm_raw,id.vars=c("eodno","landmark"), measure.vars=c("time","voltage"))
    
    # calculate row-wise average
    b<-acast(a,variable~landmark,fun.aggregate=function(x) mean(as.numeric(x)),value.var="value")
    
    #calculate row wise sd
    c<-acast(a,variable~landmark,fun.aggregate=function(x) sd(as.numeric(x)),value.var="value")
    
    #rename columns
    rownames(c)<-c("time_sd","voltage_sd")
    
    #add number of eods
    n_eods<-rep(neods,dim(c)[2])
    lm_raw<-t(rbind(b,c,n_eods))
    
    #ensure it's a dataframe for plotting, clean up rownames
    lm_raw<-as.data.frame(lm_raw)
    lm_raw$landmark<-rownames(lm_raw)
    rownames(lm_raw)<-NULL
    
    #move landmark column to first position for later export
    lm_raw[c("landmark", setdiff(names(lm_raw), "landmark"))]
    }


#' Plot average EOD signal
#' @export
#' @import ggplot2
#'
#' @param plotdata The EOD matrix from getEODMatrix
#' @param verbose output debug info
plotAverage <- function(plotdata, verbose = F) {
    if(verbose) {
        cat('plotting average peak...\n')
    }

    ggplot(data=plotdata, aes(x=time, y=data)) + stat_summary(aes(y = data), fun.y=mean, geom='line')
}

#' Plot all EOD signals
#' @export
#' @import ggplot2
#'
#' @param plotdata The EOD matrix from getEODMatrix
#' @param alpha level of transparency
#' @param verbose output debug info
plotTotal <- function(plotdata, alpha = 0.05, verbose = F) {
    if(verbose) {
        cat('plotting average peak...\n')
    }

    ggplot(data=plotdata, aes(x=time, y=data, group=col)) + geom_line(alpha=alpha)
}


#' Plot all EOD signal with landmarks
#' @export
#' @import ggplot2
#'
#' @param plotdata The EOD matrix from getEODMatrix
#' @param landmark_table The landmark table from findLandmarks
#' @param verbose output debug info
plotLandmarks <- function(plotdata, landmark_table, verbose = F) {
    if(verbose) {
        cat('plotting eod with landmarks\n')
    }
    ggplot(data=plotdata, aes(x=time, y=data)) + stat_summary(aes(y = data), fun.y=mean, geom='line') + geom_point(data = na.omit(landmark_table), aes(x=time, y=voltage, color=landmark), size = 4) + scale_colour_brewer(palette = "Set1")
}


#' Plot EODs and find EOD statistics
#' @export
#' @import ggplot2
#'
#' @param filename The filename
#' @param peaks A data.frame of peaks (time, direction +/-)
#' @param channel The channel name, default /'Untitled'/'Dev1/ai0' which is just common in our lab
#' @param prebaseline Subtract baseline pre normalization
#' @param postbaseline Subtract baseline post normalization
#' @param normalize Normalize data to 0-1
#' @param alpha Alpha channel for all EODs plot
#' @param window Window size
#' @param verbose Set verbose output
plotEod <- function(filename, peaks, channel = "/'Untitled'/'Dev1/ai0'", prebaseline = F, postbaseline = F, normalize = F, alpha = F, window = 0.005, verbose = F) {
    if(nrow(peaks)==0) {
        print('No peaks found')
        return(NULL)
    }
    plotdata = getEODMatrix(filename, peaks, channel, prebaseline, postbaseline, normalize, alpha, window, verbose)
    landmark_table = findLandmarks(plotdata)
    if(verbose) {
        print(landmark_table)
    }

    write.csv(landmark_table, paste0(basename(filename), '.landmarks.csv'), quote=F, row.names=F)

    mtitle = basename(filename)

    plotAverage(plotdata, verbose) + ggtitle(mtitle)
    ggsave(paste0(basename(filename), '.average.png'))

    plotTotal(plotdata, alpha, verbose) + ggtitle(mtitle)
    ggsave(paste0(basename(filename), '.all.png'))

    plotLandmarks(plotdata, landmark_table, verbose) + ggtitle(mtitle)
    ggsave(paste0(basename(filename), '.average.landmarks.png'))
}
