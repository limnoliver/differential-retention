#import differential retention data and 
#get it all in same columns/units
setwd("data")
epa <- read.csv("EPA data extraction.csv", header = TRUE)

hist(epa$Rp[epa$Rp>0])
length(which(epa$Rp<0))
hist(epa)

hist(epa$Rn[epa$Rn>0])
hist(epa$Rn)
length(which(epa$Rn<0))

plot(epa$Rn[epa$Rn>0]~epa$Rp[epa$Rn>0])

#get rid of unrealistic values (< -1)
epa.filtered = epa[epa$Rn>-1 & epa$Rp>-1,]
plot(epa.filtered$Rn~epa.filtered$Rp)

#get rid of all negative values
epa.positive = epa[epa$Rn>0 & epa$Rp>0,]
plot(epa.positive$Rn ~ epa.positive$Rp)
abline(0,1, col = "red")
epa.positive$rret = epa.positive$Rn/epa.positive$Rp
epa.positive$h = epa.positive$mean_depth_m/epa.positive$retention_time_years
plot(epa.positive$rret~log10(epa.positive$h))

# show relative retention vs res time
plot(epa.positive$Rp~log10(epa.positive$retention_time_years))
points(epa.positive$Rn~log10(epa.positive$retention_time_years), col = "red", add = TRUE)

# show really short residence time relationships
epa.short = epa.positive[epa$retention_time_years<.5,]
plot(epa.short$rret~log10(epa.short$retention_time_years))
