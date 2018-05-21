findmormyridlandmarks <- function(data,p1pos,p2pos,baseline_n) {

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
  data.frame(
    p0 = p0,
    t1 = t1,
    t2 = t2,
    #slope1 = slope1,
    #slope2 = slope2,
    s1 = s1,
    s2 = s2,
    zc1 = zc1,
    zc2 = zc2
  )
}