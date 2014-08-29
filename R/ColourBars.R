# Adapted from function by John Colby 
# (http://www.colbyimaging.com/wiki/statistics/color-bars)
ColourBar <- function(lut, min, max=-min, nticks=6, ticks=seq(min, max, len=nticks), title="") {
  scale = (length(lut)-1)/(max-min)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}