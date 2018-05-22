#' Find Landmarks from a Mormyrid EOD Waveform 
#' @export
#'
#' @param data Two column waveform dataframe, time and voltage x n pts
#' @param baseline_n number of points to use in calculating baseline


findmormyridlandmarks <- function(data,baseline_n) {

#find p1 in waveform
p1pos = which.max(data$voltage)
p2pos = which.min(data$voltage)

p1 = data[p1pos, ]
p2 = data[p2pos, ]  
  
  
leftside = data[1:p1pos, ]
middle = data[p1pos:p2pos, ]
rightside = data[p2pos:nrow(data), ]

baseline = mean(data$voltage[1:baseline_n]) 

# null the variables
p0 = NULL
t1 = NULL
t2 = NULL
slope1 = NULL
slope2 = NULL
s1 = NULL
s2 = NULL
zc1 = NULL
zc2 = NULL

for(i in nrow(leftside):1) {
  if(leftside[i, 'voltage'] < baseline) {
    zc1 = leftside[i,]
    tzc1 = zc1$time
    p0calculator = leftside[leftside$time >= tzc1-0.0005 & leftside$time <= tzc1,]
    p0 = p0calculator[which.min(p0calculator$voltage), ]
    break
  }
}
for(i in nrow(leftside):1) {
  if(leftside[i, 'voltage'] < baseline + 0.02 * (p1$voltage - p2$voltage)) {
    t1 = leftside[i,]
    slope1 = leftside[i:nrow(leftside), ]
    break
  }
}
for(i in 1:nrow(rightside)) {
  if(rightside[i, 'voltage'] > baseline - 0.02 * (p1$voltage - p2$voltage)) {
    t2 = rightside[i,]
    break
  }
}
if(is.null(t2)) {
  t2 = rightside[20,]
}

slope1_max = -100000
for(i in 1:(nrow(slope1)-1)) {
  s = (slope1[i+1, 'voltage'] - slope1[i, 'voltage']) / (slope1[i+1, 'time'] - slope1[i, 'time'])
  if(s > slope1_max) {
    slope1_max = s
    s1 = slope1[i,]
  }
}
slope2_max = 100000
for(i in 1:(nrow(middle)-1)) {
  s = (middle[i+1, 'voltage'] - middle[i, 'voltage']) / (middle[i+1, 'time'] - middle[i, 'time'])
  if(s < slope2_max) {
    slope2_max = s
    s2 = middle[i, ]
  }
}
for(i in 1:nrow(middle)) {
  if(middle[i, 'voltage'] < baseline) {
    zc2 = middle[i,]
    break
  }
}

if(!is.null(p0)) landmark_table = data.frame(landmark = 'p0', time = p0$time, voltage = p0$voltage)
if(!is.null(p1)) landmark_table = rbind(landmark_table, data.frame(landmark = 'p1', time = p1$time, voltage = p1$voltage))
if(!is.null(p2)) landmark_table = rbind(landmark_table, data.frame(landmark = 'p2', time = p2$time, voltage = p2$voltage))
if(!is.null(t1)) landmark_table = rbind(landmark_table, data.frame(landmark = 't1', time = t1$time, voltage = t1$voltage))
if(!is.null(t2)) landmark_table = rbind(landmark_table, data.frame(landmark = 't2', time = t2$time, voltage = t2$voltage))
if(!is.null(s1)) landmark_table = rbind(landmark_table, data.frame(landmark = 's1', time = s1$time, voltage = s1$voltage))
if(!is.null(s2)) landmark_table = rbind(landmark_table, data.frame(landmark = 's2', time = s2$time, voltage = s2$voltage))
if(!is.null(zc1)) landmark_table = rbind(landmark_table, data.frame(landmark = 'zc1', time = zc1$time, voltage = zc1$voltage))
if(!is.null(zc2)) landmark_table = rbind(landmark_table, data.frame(landmark = 'zc2', time = zc2$time, voltage = zc2$voltage))
landmark_table = rbind(landmark_table, data.frame(landmark = 'duration', time = t2$time-t1$time,voltage="NA_real_"))
landmark_table = rbind(landmark_table, data.frame(landmark = 'amplitude', time = "NA_real_", voltage = p1$voltage-p2$voltage))

landmark_table

  #data.frame(
  #  p0 = p0,
  #  t1 = t1,
  #  t2 = t2,
  #  #slope1 = slope1,
  #  #slope2 = slope2,
  #  s1 = s1,
  #  s2 = s2,
  #  zc1 = zc1,
  #  zc2 = zc2
  #)
}