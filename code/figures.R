# This script creates figures for differential retention project
library(RColorBrewer)
# Set colors for all plots
col.n1 <- rgb(.955, .7975, .27625,.7)
col.n2 <- rgb(.94,.73,0.035,.7)
col.n3 <- rgb(.705,.5475,.02625,.7)
col.n4 <- rgb(.47, .365, .0175, .7)
col.n5 <- rgb(.313, .243, .0117, .7)

col.p = rgb(.07,.57,.45,0.7)
# gradient of blues for depth where appropriate
new.cols = c(brewer.pal(n = 9, name = "Blues"), "black")


# set model parameters
Vf = 6.68

# depth min, max, quartiles
# data from hydrolakes
z.mean = c(0.1, 2.3, 3.3, 4.5, 739)

# Rp equation with depth
Rp.depth <= 1-(1/(1+((5.1/z)*x)))

############################################
# create a histogram that shows distribution of 
# depth and residence time of our lakes, global lakes,
# and NLA lakes

library(ggplot2)

nla.u <- aggregate(nla$RT, by=list(nla$SITE_ID), data = nla, FUN=mean)
names(nla.u) <- c("SITE_ID", "RT")
nla.merge <- as.data.frame(nla)
nla.merge <- nla.merge[,c(1,13)]

nla.dat <- merge(nla.u, nla.merge, by = "SITE_ID", all.x = TRUE)
nla.dat <- nla.dat[nla.dat$RT>0, ]
ggplot(nla.dat, aes(log10(RT), weight = WGT_NLA)) +
  geom_histogram()

library(plotrix)
weighted.hist(log10(nla.dat$RT), nla.dat$WGT_NLA)
h2 <- hist(log10(nla.dat$RT), plot = FALSE)
h2$breaks <- h$breaks
h2$density <- h$counts/sum(h$counts)*100
plot(h2, freq=FALSE)

h1 <- hist(log10(stoich$res_time), breaks = c(-3:8), plot = FALSE)
h1$density <- h1$counts/sum(h1$counts)*100

h <- weighted.hist(log10(nla.dat$RT), nla.dat$WGT_NLA, breaks = h1$breaks,  plot = FALSE)
h3 <- hist(log10(nla.dat$RT), breaks = h1$breaks,  plot = FALSE)
h3$counts <- h$counts
h3$density <- h3$counts/sum(h3$counts)*100

lakes$Res_time[lakes$Res_time==-9999] = NA
lakes$Res_time[lakes$Res_time==-1] = NA

h4 <- hist(log10(lakes$Res_time), breaks = h1$breaks, plot = FALSE)
h4$density <- h4$counts/sum(h4$counts)*100
plot(h3, freq = FALSE, col = rgb(122,122,122,max=255,122))
plot(h1, freq = FALSE, ylim = c(0,40), add = TRUE, col = col.n)
plot(h4, freq = FALSE, add = TRUE, col = col.p)


############################################
# Calculate summary statistics for Table 2
############################################
vars <- c("volume", "surface_area", "mean_depth", "res_time", "tp_in_conc", "tp_out_conc",
          "Rp", "tn_in_conc", "tn_out_conc", "Rn", "RnRpdiff")
sum.stats <- function(input.dat, percentile) {
  d <- input.dat
  d$RnRpdiff <- d$Rn-d$Rp
  summ <- c()
  for (i in 1:length(vars)){
  summ[i] <- as.numeric(quantile(d[,vars[i]], probs = percentile, na.rm = TRUE))
  }
  return(as.data.frame(t(summ)))
}

r1 <- sum.stats(dat.np, percentile = 0.9)
r2 <- sum.stats(dat.np, percentile = 0.5)
r3 <- sum.stats(dat.np, percentile = 0.1)
r4 <- sum.stats(dat.n, percentile = .5)
r5 <- sum.stats(dat.n.real, percentile = 0.5)
r6 <- sum.stats(dat.n.pos, percentile = 0.5)
r7 <- sum.stats(dat.p, percentile = .5)
r8 <- sum.stats(dat.p.real, percentile = 0.5)
r9 <- sum.stats(dat.p.pos, percentile = 0.5)

dat.sum <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
names(dat.sum) <- vars

write.csv(dat.sum, "table2.csv")

sum.stats <- 

##################################################################
# Figure 1: N and P retention according to Vollenweider & Harrison
##################################################################
png("R_restime.png", height = 600, width = 800)
# plot N and P lines together 
par(mar=c(5,5,1,1))

# plot new P curve + Brett & Benjamin curve
curve(1-(1/(1+(1.12*(x^.47)))), 0.001,1000,log = "x",
     ylab = "Retention Efficiency", xlab = "Residence Time (y)",
     col = col.p, ylim = c(0, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
#curve(1-(1/(1+(1.12*(x^.47)))), 0.001,1000,log = "x",
#         col = col.p, lwd = 2, lty = 2)

axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)
# Harrison N curves + new fit N curves
#curve(1-(exp((-Fit.N.real$par[1]*x)/3)), .001, 1000, 
      col = col.n1, add = TRUE, lwd = 4)
#curve(1-(exp((-8.91*x)/3)), .001, 1000, 
      col = col.n1, add = TRUE, lwd = 2, lty =2)
#curve(1-(exp((-Fit.N.real$par[1]*x)/5.9)), .001, 1000, 
      col = col.n3, add = TRUE, lwd = 4)
curve(1-(exp((-8.91*x)/6)), .001, 1000, 
      col = col.n3, add = TRUE, lwd = 4)
#curve(1-(exp((-Fit.N.real$par[1]*x)/11.1)), .001, 1000, 
      col = col.n5, add = TRUE, lwd = 4)
#curve(1-(exp((-8.91*x)/11.1)), .001, 1000, 
      col = col.n5, add = TRUE, lwd = 2, lty =2)

abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v=10, col = "gray", lty = 2)
abline(v=100, col = "gray", lty = 2)
legend("topleft", legend = c("Nitrogen", "Phosphorus"), 
       col = c(col.n3, col.p), lty = 1, lwd = 3, cex = 1.7)
dev.off()

########################################################
# Figure 2: Differential retention according to V & H
#######################################################
png("RnRp_restime.png", height = 600, width = 800)
par(mar=c(5,5,1,1))
curve((1-(exp((-Fit.N.real$par[1]*x)/1.8)))/(1-(1/(1+(Fit.Rp.real$par[1]*(x^(1+Fit.Rp.real$par[2])))))), 0.001,1000,log = "x",
      ylab = "Rn:Rp", xlab = "Residence Time (y)", 
      col = new.cols[3], ylim = c(0, 1.7), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)
curve((1-(exp((-Fit.N.real$par[1]*x)/6)))/(1-(1/(1+(Fit.real$par[1]*(x^(1+Fit.real$par[2])))))), 0.001,1000,log = "x",
      col = new.cols[6], lwd = 4, add = TRUE)
curve((1-(exp((-Fit.N.real$par[1]*x)/19.44)))/(1-(1/(1+(Fit.real$par[1]*(x^(1+Fit.real$par[2])))))), 0.001,1000,log = "x",
      col = new.cols[9], lwd = 4, add = TRUE)

abline(h=1, col = "red", lwd = 2, lty = 2)
abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v=10, col = "gray", lty = 2)
abline(v=100, col = "gray", lty = 2)
legend("topleft", legend = c("1.8", "6.0", "19.4"), title = "Depth (m)", 
       col = new.cols[c(3,6,9)], lty = 1, lwd = 3, cex = 1.7)
text(x=100, y=1.5, "Remove more N \nDecrease N:P", cex = 1.7)
text(x=100, y=0.5, "Remove more P \nIncrease N:P", cex = 1.7)

dev.off()

##########################################################################
# Figure 3: Differential retention with realistic depth and residence time
##########################################################################
## create a plot that shows percent change in N:P by depth and residence time
# create percentiles for depth, and find residence time for the median depth in those bins
p <- .bincode(dat.all$mean_depth, breaks = as.numeric(quantile(dat.all$mean_depth, seq(0,1,by=0.1), na.rm = TRUE)), right = FALSE)
depth = as.numeric(tapply(dat.all$mean_depth, INDEX = c(p), median, na.rm = TRUE))
x = as.numeric(tapply(dat.all$res_time, INDEX = c(p), median, na.rm = TRUE))
res_time_sd = as.numeric(tapply(dat.all$res_time, INDEX = c(p), sd, na.rm = TRUE))
n_outin = exp((-9.92*x)/depth)
p_outin = 1/(1+(1.12*(x^.47)))
np_perc = -100*(1-(n_outin/p_outin))

png("PercentChange_restime.png", height = 600, width = 800)
par(cex = 1, mar = c(5,5,1,1))
curve(-100*(1-((exp((-9.92*x)/depth[1]))/(1/(1+(1.12*(x^.47)))))), from=.001,to=1000, log="x",
      n=1000,
      cex.lab = 2,
      cex.axis = 1.3,
      ylab="% Change TN:TP", 
      xlab = "Residence Time (y)",
      lwd=4, 
      ylim=c(-100,100), col=new.cols[1], bty="l", xaxt = "n")
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)
for (i in 1:length(depth)){
  curve(-100*(1-((exp((-9.92*x)/depth[i]))/(1/(1+(1.12*(x^.47)))))), from=.001,to=1000, log="x",
        n=1000,
        lwd=4, 
        col=new.cols[i], bty="l", add = TRUE) 
}
legend("topright", legend = paste(depth, "m"), col = new.cols, lwd = 3, cex = 1.3)
box()

abline(h=0, col = "gray")
abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v = 10, col = "gray", lty = 2)
abline(v = 100, col = "gray", lty = 2)

# now add points of where real lakes can be
points(x, y = np_perc, xlog = TRUE, bg = c(brewer.pal(n = 9, name = "Blues"), "black"), pch = 21,cex = 2)
text(x = 1/365, y = -30, "Remove more N \nDecrease N:P", cex = 1.3)
text(x = 1/365, y = 30, "Remove more P \nIncrease N:P", cex = 1.3)

dev.off()

########################################################
# Figure 4: Mass balance lakes - depth vs residence time
########################################################

## another way to look at depth vs res time is by boxplots
hist(log10(dat.all$res_time))
breaks <- hist(log10(dat.all$mean_depth), breaks = 5)
z <- .bincode(dat.all$mean_depth,breaks = c(0,2,3,5,10,20,50,315), right = FALSE)

png("restime_depth_boxplot.png", height = 600, width = 800, pointsize = 14)
par(cex = 1.2, mar = c(4,4,1,2))
boxplot(log10(dat.all$res_time)~z, xaxt = "n", yaxt = "n", cex.lab = 1.5, ylim = c(-3,3.3))
axis(1, labels = c("<2", "2-3", "3-5", "5-10", "10-20", "20-50", ">50"), 
     at = c(1,2,3,4,5,6,7), main = "Mean Depth (m)", cex.axis = 1.3, mgp = c(1.5,.7,0))
title(xlab = "Mean Depth (m)", ylab = "Residence Time (yr)", mgp = c(2.5,.7,0), cex.lab = 2)
axis(2, labels = c(0.001,0.01, 0.1, 1, 10, 100,1000), 
     at = c(-3,-2,-1,0,1,2,3), cex.axis = 1.3)
text(x = c(1,2,3,4,5,6,7), 
     y= c(1,1.5,1.9,1.5,1.6,1.97,3),
     c("12%", "10%", "17%", "29%",  "20%", "8%", "3%"),col = "red")
text(x = c(1,2,3,4,5,6,7), y = as.numeric(tapply(log10(dat.all$res_time), INDEX = c(dat.all$z), median, na.rm = TRUE)), 
     c("21 d", "50 d", "4 mo", "5 mo", "10 mo", "1.1 yr", "6.6 yr"), col = "blue", pos = 3)


dev.off()

# Finally, use EPA data to show relationship between DA:SA ~ restime to 
# use in LAGOS

#create a relationship between drainage area to residence time
epa$dasa <- epa$drainage_area_km2/epa$surface_area_km2

png("restime_dasa.png", height = 600, width = 800, pointsize = 14)
par(cex = 1.2, mar = c(5,5,1,1))
plot(log10(epa$retention_time_years[epa$type=="reservoir"])~log10(epa$dasa[epa$type=="reservoir"]), cex = 1.5,
     pch = 21, bg = rgb(200,200,200,alpha=200, max = 255),
     xlab = "log Drainage Area:Surface Area", 
     ylab = "log Residence Time", cex.lab = 1.8, cex.axis = 1.3, xlim = c(0,5), ylim=c(-3,3),
     xaxt = "n", yaxt = "n")
axis(1,labels=c("1, 100, 1000, 10000"))
points(log10(epa$retention_time_years[epa$type=="lake"])~log10(epa$dasa[epa$type=="lake"]), cex = 1.5,
       pch = 21, bg = rgb(100,200,200,alpha=200, max = 255))  
abline(lm(log10(epa$retention_time_years[epa$type=="reservoir"])~log10(epa$dasa[epa$type=="reservoir"])), col = "gray", lwd = 2)     
abline(lm(log10(epa$retention_time_years[epa$type=="lake"])~log10(dasa[epa$type=="lake"])), col = rgb(100,200,200,alpha=200,max=255), lwd = 2)     
abline(lm(log10(epa$retention_time_years)~log10(epa$dasa)), lty = 2, lwd=2)
text(3.5, 3, expression("Residence Time = 6.9X"^-0.68), col = rgb(150,150,150, max = 255), cex = 1.3)       
text(3.5, 2.6, expression("Residence Time = 24.5X"^-1.06), col = rgb(100,200,200, max = 255), cex = 1.3)       
dev.off()

#########################################
# Figure 5: N and P retention vs H and V
#########################################

pdf("RvsEq.pdf", height = 6, width = 14)
par(mar=c(5,5,1,1), mfrow = c(1,2))
#plot retention vs residence time then add curve of Harrison
curve(1-(exp((-Fit.N.real$par[1]*x)/6)), 0.001,1000,log = "x",
      ylab = "Rn", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-1, 1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(1, labels = c("1 d", "1 wk", "1 mo", "1 yr", "10 yr", "100 yr"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)

points(dat.n$Rn~dat.n$res_time, xlog = TRUE, pch = 21, cex = 1.1,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(exp((-Fit.N.real$par[1]*x)/5.9)), 0.001,1000,log = "x",add = TRUE,
      col = "red", ylim = c(-.1, 1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)

abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v=10, col = "gray", lty = 2)
abline(v=100, col = "gray", lty = 2)
text(x=2/365, y = .9, labels = "n = 903", cex = 1.3, col = "red")

#plot retention vs residence time then add curve of Brett & Benjamin
curve(1-(1/(1+(Fit.Rp.real$par[1]*(x^(1+Fit.Rp.real$par[2]))))), 0.001,1000,log = "x",
      ylab = "Rp", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-1, 1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.2)
axis(1, labels = c("1 d", "1 wk", "1 mo", "1 yr", "10 yr", "100 yr"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.2)

points(dat.all$Rp~dat.all$res_time, xlog = TRUE, pch = 21, cex = 1.1,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(1/(1+(Fit.Rp.real$par[1]*(x^(1+Fit.Rp.real$par[2]))))), 0.001,1000,log = "x",add = TRUE,
      col = "red", ylim = c(-.1, 1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)

abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v=10, col = "gray", lty = 2)
abline(v=100, col = "gray", lty = 2)
text(x=2/365, y = .9, labels = "n = 1076", cex = 1.3, col = "red")

dev.off()

## need to: optimize models and plot new model lines
## optimize with 1) only positive values, 2) only "realistic" values,
## 3) all values

#########################################
# Figure 6: N vs P retention + histograms
#########################################

pdf("Rn_Rp_hist_xy.pdf", height = 6, width = 12)
par(mar=c(5,5,1,1), mfrow=c(1,2))
hist(stoich$Rn[stoich$Rn>-1], xlim = c(-1,1), col = col.n, main = "", 
     xlab = "Proportion Retention", cex.lab = 1.8, cex.axis = 1.3)
hist(stoich$Rp[stoich$Rp>-1], add = TRUE, col = col.p)
legend("topleft", legend = c("Rn", "Rp"), fill = c(col.n, col.p), cex = 1.3)

plot(stoich$Rn~stoich$Rp, pch = 21, 
     bg = rgb(222,222,222,alpha = 200, max = 255), cex = 1.3, cex.lab = 1.8,
     xlab = "Rp", ylab = "Rn", cex.axis = 1.3)

abline(0,1,col = "red", lwd = 2)
text(x=-.6, y = .9, "587 of 807 (73%) \nRp > Rn", col = "red", cex = 1.3)

dev.off()
scatterhist = function(x, y, xlab="", ylab=""){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(4.5,4.5,1,1), cex = 2)
  plot(x,y, pch = 21, 
       bg = rgb(222,222,222,alpha = 200, max = 255), xlab = xlab, ylab = ylab, cex.lab = 1.4)
  abline(0,1,lwd=2, col = "red")
  text(x=-.6, y = .9, "587 of 807 (73%) \nRp > Rn", col = "red", cex = 1.3)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(4,4,0,0))
  #mtext(xlab, side=1, line=2, outer=TRUE, adj=0, 
  #      at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  #mtext(ylab, side=2, line=2, outer=TRUE, adj=0, 
  #      at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}
png("R_scatterhist.png", height = 1000, width = 1000)
scatterhist(x = stoich$Rp, y = stoich$Rn, xlab = "Rp", ylab = "Rn")
#abline(0,1,col = "red", lwd = 2)
#text(x=-.6, y = .9, "587 of 807 (73%) \nRp > Rn", col = "red", cex = 1.3)

dev.off()

# Figure 6.123: histograms of broken up by N retention > P retention,
# histogram of size, input concentrations, etc.
pdf("R_depth_restime.pdf", height = 6, width = 12)
par(mar=c(5,5,1,0), mfrow=c(1,2))
vals = hist(log10(stoich$mean_depth[stoich$Rn>stoich$Rp]), plot = FALSE, breaks = 10)
vals$counts = vals$counts/length(stoich$mean_depth[stoich$Rn>stoich$Rp])
breaks.n = vals$breaks
plot(vals, col = col.n, xlab = "Mean Depth", ylab = "Proportion of Lakes", cex.lab = 1.8,
     cex.axis = 1.3, main = "", ylim = c(0, .25), xaxt = "n")
axis(1, labels = c("1", "5", "10","20", "50", "100"), at = c(0,log10(5), 1, log10(20), log10(50), 2),
     cex.axis=1.2)
legend("topright", legend = c("Rn > Rp", "Rn < Rp"), fill = c(col.n, col.p), cex = 1.3)
vals = hist(log10(stoich$mean_depth[stoich$Rn<stoich$Rp]), breaks = breaks.n, plot = FALSE)
vals$counts = vals$counts/length(stoich$mean_depth[stoich$Rn<stoich$Rp])
plot(vals, col = col.p, add = TRUE)

vals = hist(log10(stoich$res_time[stoich$Rn>stoich$Rp]), plot = FALSE, breaks = 10)
vals$counts = vals$counts/length(stoich$res_time[stoich$Rn>stoich$Rp])
breaks.n = vals$breaks
plot(vals, col = col.n, xlab = "Residence Time", ylab = "Proportion of Lakes", cex.lab = 1.8,
     cex.axis = 1.3, main = "", ylim = c(0, .3), xaxt = "n")
axis(1, labels = c("day", "wk", "mon", "yr", "10 yr", "100 yr"), 
     at = c(log10(1/365), log10(7/365), log10(30/365), 0, 1, 2), cex.axis=1.2)
vals = hist(log10(stoich$res_time[stoich$Rn<stoich$Rp]), breaks = breaks.n, plot = FALSE)
vals$counts = vals$counts/length(stoich$res_time[stoich$Rn<stoich$Rp])
plot(vals, col = col.p, add = TRUE)
dev.off()

#####################################################################
# Figure 7: Differential retention with deciles for depth and retention time + 
# real N and P retention
#####################################################################

## create a plot that shows percent change in N:P by depth and residence time
# create percentiles for depth, and find residence time for the median depth in those bins
p <- .bincode(stoich$mean_depth, breaks = as.numeric(quantile(stoich$mean_depth, seq(0,1,by=0.1), na.rm = TRUE)), right = FALSE)
depth = as.numeric(tapply(stoich$mean_depth, INDEX = c(p), median, na.rm = TRUE))
x = as.numeric(tapply(stoich$res_time, INDEX = c(p), median, na.rm = TRUE))
res_time_sd = as.numeric(tapply(stoich$res_time, INDEX = c(p), sd, na.rm = TRUE))

stoich$np_perc = -100*(1-(stoich$np_out * (1/stoich$np_in)))
np_perc <- as.numeric(tapply(stoich$np_perc, INDEX = c(p), median, na.rm = TRUE))

n_outin = exp((-Fit.N.real$par[1]*x)/depth)
p_outin = 1/(1+(Fit.real$par[1]*(x^(1+Fit.real$par[2]))))
np_perc_pred = -100*(1-(n_outin/p_outin))



png("PercentChange_restime.png", height = 600, width = 800)
par(cex = 1, mar = c(5,5,1,1))
curve(-100*(1-((exp((-Fit.N.real$par[1]*x)/depth[1]))/(1/(1+(Fit.real$par[1]*(x^(1+Fit.real$par[2]))))))), from=.001,to=1000, log="x",
      n=1000,
      cex.lab = 2,
      cex.axis = 1.3,
      ylab="% Change TN:TP", 
      xlab = "Residence Time (y)",
      lwd=4, 
      ylim=c(-100,200), col=new.cols[1], bty="l", xaxt = "n")
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)
for (i in 1:length(depth)){
  curve(-100*(1-((exp((-Fit.N.real$par[1]*x)/depth[i]))/(1/(1+(Fit.real$par[1]*(x^(1+Fit.real$par[2]))))))), from=.001,to=1000, log="x",
        n=1000,
        lwd=4, 
        col=new.cols[i], bty="l", add = TRUE) 
}
legend("topright", legend = paste(depth, "m"), col = new.cols, lwd = 3, cex = 1.3)
box()

abline(h=0, col = "gray")
abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v = 10, col = "gray", lty = 2)
abline(v = 100, col = "gray", lty = 2)

# now add points of where real lakes can be
#points(x, y = np_perc, xlog = TRUE, bg = c(brewer.pal(n = 9, name = "Blues"), "black"), col = "red", pch = 21,cex = 2)
points(x, y = np_perc_pred, xlog = TRUE, bg = c(brewer.pal(n = 9, name = "Blues"), "black"), col = "red", pch = 21,cex = 1.)
arrows(x0 = x, x1 = x, y0 = np_perc_pred, y1 = np_perc, lwd=2, col = "red", length = .1)
text(x = 1/365, y = -30, "Remove more N \nDecrease N:P", cex = 1.3)
text(x = 1/365, y = 30, "Remove more P \nIncrease N:P", cex = 1.3)

dev.off()

#############################################################################
# Figure 8: Differential retention as % change in TN:TP vs rank or res time
# - predicted from models
#############################################################################


xin <- stoich$rank_sum
xout <- stoich$rank_sum
yin <- log10(stoich$np_in)
yout <- log10(stoich$np_out)

stoich.up <- which(yout>yin)
stoich.down <- which(yin>yout)

# decide stoich cutoffs for N:P in 
# just diverging colors by decile
library(RColorBrewer)
stoich.cols <- brewer.pal(6, "PRGn")[c(1,3,4,6)]
stoich.cols <- adjustcolor(stoich.cols, alpha.f = .7)

get.col.bins <- function(stoich.vals) {
  
  ii <- cut(log10(stoich.vals), as.numeric(quantile(log10(stoich.vals),probs = c(0,.2,.5,.8,1))), 
            include.lowest = TRUE)
  
  levels(ii) <- stoich.cols
  ii = as.character(ii)
  return(ii)
}

stoich$colors <- get.col.bins(stoich$mean_depth)

# create function for stoich cutoffs
get.col.bins.limiting <- function(stoich.vals) {
  
  ii <- cut(stoich.vals, c(0, 44, 110, 9000), 
            include.lowest = TRUE)
  
  levels(ii) <- stoich.cols
  ii = as.character(ii)
  return(ii)
}
stoich.cols <- c(col.p, "white", col.n)
stoich.cols <- adjustcolor(stoich.cols, alpha.f = .7)
stoich$colors <- get.col.bins.limiting(stoich$np_in)


# remove outliers from stoich based on TN in ~ TP in
mod <- lm(log10(tn_in_mass_aerial)
stoich$np
#stoich$np_change_log <- log10(stoich$np_change)
#stoich$np_change_log[stoich$np_out<stoich$np_in] <- abs(stoich$np_change_log[stoich$np_out<stoich$np_in])*-1
#stoich$np_change_log[stoich$np_out>stoich$np_in] <- abs(stoich$np_change_log[stoich$np_out>stoich$np_in])

# now plot difference as points, with zero in middle
pdf("diffinout_col.pdf")
plot(stoich$np_change~stoich$rank_sum, cex.lab = 1.8, cex = 1.6, 
     xlab = "Res Time & Depth Rank", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors)
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", col = stoich.cols[c(1,10)], pch = 16, cex = 1.8, legend = c("low TN:TP", "high TN:TP"))
dev.off()

###################################################
#  Fig. 8.2: 2x2 plot; panels = residence time quartile (remove residence time
# effect), x axis = depth, colors = input N:P

stoich.cols <- c(col.p, "white", col.n)
stoich.cols <- adjustcolor(stoich.cols, alpha.f = .7)
stoich$colors <- get.col.bins.limiting(stoich$np_in)


pdf("PerChange_depth_stoich.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$np_change[stoich$res_time<.0872]~log10(stoich$mean_depth[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Limiting Nutrient", legend = c("N", "N and/or P", "P"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$np_change[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$mean_depth[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$mean_depth[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$mean_depth[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("Change in Stoichiometry", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Mean Depth (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()
########################################################################
#  Fig. 8.3 2x2 plot; panels = residence time quartile (remove residence time
# effect), x axis = depth, colors = input P
stoich.cols <- brewer.pal(6, "PRGn")[c(1,3,4,6)]
stoich.cols <- adjustcolor(stoich.cols, alpha.f = .7)

stoich$colors <- get.col.bins(stoich$tp_in_conc)

pdf("PerChange_depth_Pin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$np_change[stoich$res_time<.0872]~log10(stoich$mean_depth[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "[P]in Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$np_change[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$mean_depth[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$mean_depth[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$mean_depth[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("Change in Stoichiometry", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Mean Depth (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

########################################################################
#  Fig 8.4: 2x2 plot; panels = residence time quartile (remove residence time
# effect), x axis = depth, colors = input P
stoich.cols <- brewer.pal(6, "PRGn")[c(1,3,4,6)]
stoich.cols <- adjustcolor(stoich.cols, alpha.f = .7)
stoich$colors <- get.col.bins(stoich$mean_depth)

col.vals = quantile(stoich$mean_depth, c(0,.2,.5,.8,1))
pdf("PerChange_stoichin_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$np_change[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
text(x = 0, y= -1.2, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
plot(stoich$np_change[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -1.2, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
plot(stoich$np_change[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -1.2, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$np_change[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("Change in Stoichiometry", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input TN:TP", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 0, y= -1.2, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

dev.off()

################################################
# Fig 8.5 - exact same fig but for predicted change in stoich

pdf("PerChangePred_stoichin_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$np_change_predicted[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
text(x = 0, y= -1.2, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
plot(stoich$np_change_predicted[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -1.2, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
plot(stoich$np_change_predicted[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= 1.8, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$np_change_predicted[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(0,3))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("Change in Stoichiometry", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input TN:TP (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 0, y= 1.8, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

dev.off()


# now create a 2x2 plot that each contains a residence time quartile (remove residence time
# effect), and plot TN:TP concentration on the x axis
par(mfrow=c(2,2))
plot(stoich$np_change[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TN:TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TN:TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

# now create a 2x2 plot that each contains a residence time quartile (remove residence time
# effect), and plot TN:TP concentration on the x axis -- predicted % change
par(mfrow=c(2,2))
plot(stoich$np_change_predicted[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change_predicted[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change_predicted[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$np_change_predicted[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.6, 
     xlab = "log TP in", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

# now plot difference as points, with zero in middle
# same plot, but with predicted data
pdf("diffinout_col.pdf")
plot(stoich$np_change_predicted~stoich$rank_sum, cex.lab = 1.8, cex = 1.6, 
     xlab = "Res Time & Depth Rank", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors)
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", col = stoich.cols[c(1,10)], pch = 16, cex = 1.8, legend = c("low TN:TP", "high TN:TP"))
dev.off()
##################################
# Figure 9: load N:P vs out N:P
##################################
plot(log10(stoich$np_out)~log10(stoich$np_in),xlim = c(-1,4),ylim=c(-1,4))
abline(0,1,col = "red")

plot(log10(stoich$np_out)~log10(stoich$np_in), cex.lab = 1.8, cex = 1.6, 
     xlab = "N:P in", ylab = "N:P out",
     pch = 21, bg = stoich$colors)
abline(0,1,col = "red", lwd = 2, lty = 2)

pdf("Stoich_in_out_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$np_out)[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
text(x = 4, y= -1, expression(paste(tau, " < 1 month")), col = "red", pos = 2)
plot(log10(stoich$np_out)[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = 4, y= -1, expression(paste(tau, " = 1-5 months")), col = "red", pos = 2)
plot(log10(stoich$np_out)[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = 4, y= -1, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 2)

plot(log10(stoich$np_out)[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
mtext("log TN:TP out", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log TN:TP in", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 4, y= -1, expression(paste(tau, " > 1.2 years")), col = "red", pos = 2)
dev.off()

# same fig but with predicted N:P out
pdf("Stoich_in_outpred_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$np_out_predicted)[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
text(x = 4, y= -1, expression(paste(tau, " < 1 month")), col = "red", pos = 2)
plot(log10(stoich$np_out_predicted)[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = 4, y= -1, expression(paste(tau, " = 1-5 months")), col = "red", pos = 2)
plot(log10(stoich$np_out_predicted)[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = 4, y= -1, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 2)

plot(log10(stoich$np_out_predicted)[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1, 4), xlim = c(-1, 4))
abline(0,1, lty = 2, col = "red", lwd = 2)
mtext("log TN:TP out", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log TN:TP in", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 4, y= -1, expression(paste(tau, " > 1.2 years")), col = "red", pos = 2)
dev.off()

#################################################
# Same figs except breaking up by H
#################################################
pdf("PerChange_npin_h_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$np_change[stoich$h<5.549]~log10(stoich$np_in[stoich$h<5.549]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$h<5.549], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topright", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
text(x = 0, y= -1.2, "h < 5.5", col = "red", pos = 4)
plot(stoich$np_change[stoich$h>=5.549&stoich$h<18.730]~log10(stoich$np_in[stoich$h>=5.549&stoich$h<18.730]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$h>=5.549&stoich$h<18.730], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -1.2, "h = 5.5-18.7", col = "red", pos = 4)
plot(stoich$np_change[stoich$h>=18.730&stoich$h<55.260]~log10(stoich$np_in[stoich$h>=18.730&stoich$h<55.260]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$h>=18.730&stoich$h<55.260], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -1.2, "h = 18.7-55.2", col = "red", pos = 4)

plot(stoich$np_change[stoich$h>55.260]~log10(stoich$np_in[stoich$h>55.260]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$h>=1.2&stoich$h<478], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("Change in Stoichiometry", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input TN:TP (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 0, y= -1.2, "h > 55.2", col = "red", pos = 4)

dev.off()

pdf("PerChangePred_npin_h_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$np_change_predicted[stoich$h<5.549]~log10(stoich$np_in[stoich$h<5.549]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$h<5.549], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topright", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
text(x = 0, y= -1.2, "h < 5.5", col = "red", pos = 4)
plot(stoich$np_change_predicted[stoich$h>=5.549&stoich$h<18.730]~log10(stoich$np_in[stoich$h>=5.549&stoich$h<18.730]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$h>=5.549&stoich$h<18.730], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -1.2, "h = 5.5-18.7", col = "red", pos = 4)
plot(stoich$np_change_predicted[stoich$h>=18.730&stoich$h<55.260]~log10(stoich$np_in[stoich$h>=18.730&stoich$h<55.260]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$h>=18.730&stoich$h<55.260], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -1.2, "h = 18.7-55.2", col = "red", pos = 4)

plot(stoich$np_change_predicted[stoich$h>55.260]~log10(stoich$np_in[stoich$h>55.260]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$h>=1.2&stoich$h<478], ylim = c(-1.3, 2), xlim = c(0,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("Predicted Change in Stoichiometry", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input TN:TP (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 0, y= -1.2, "h > 55.2", col = "red", pos = 4)

dev.off()


# Figure 12: calculate Vf and sedimentation coefficient and summarize by lake type. 
# show how sed coef or Vf changes with nutrient input concentration

##
plot(log10(dat.np$mean_depth)~log10(dat.np$res_time))
points(log10(dat.np$mean_depth[dat.np$Rn>2*dat.np$Rp])~log10(dat.np$res_time[dat.np$Rn>2*dat.np$Rp]), pch = 21,bg = col.n)
points(log10(dat.np$mean_depth[dat.np$Rp>2*dat.np$Rn])~log10(dat.np$res_time[dat.np$Rp>2*dat.np$Rn]), pch = 21,bg = col.p)


## Figure - depth dependence of Rn
stoich$colors <- get.col.bins(stoich$np_in)

pdf("Rp_restime_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp[stoich$res_time<.0872]~log10(stoich$mean_depth[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "N:P in Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rp[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$mean_depth[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$mean_depth[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$mean_depth[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Mean Depth (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

stoich$colors <- get.col.bins(stoich$mean_depth)

pdf("Rp_restime_stoichin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rp[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input N:P", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

pdf("Rp_restime_stoichin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp_res[stoich$res_time<.0872]~log10(stoich$tp_in_conc[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.1, 1.1), xlim = c(-4,3.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
#legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rp_res[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tp_in_conc[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.1, 1.1), xlim = c(-4,3.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp_res[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tp_in_conc[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.1, 1.1), xlim = c(-4,3.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp_res[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$tp_in_conc[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.1, 1.1), xlim = c(-4,3.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input N:P", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()
#######################################################
# 4x4 plots with concentration as panels
#######################################################
conc.q <- quantile(stoich$tp_in_conc, c(0.25,.5,.75))

pdf("Rpr_bypc_pin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp_res[stoich$tp_in_conc<conc.q[1]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc<conc.q[1]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc<conc.q[1]], ylim = c(-1.5, .5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
#legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rp_res[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]], ylim = c(-1.5,.5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp_res[stoich$tp_in_conc >=conc.q[2] & stoich$tp_in_conc<conc.q[3]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]], ylim = c(-1.5,0.5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp_res[stoich$tp_in_conc>=conc.q[3]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc>=conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[3]&stoich$tp_in_conc<478], ylim = c(-1.5, 0.5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log P in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)
dev.off()

pdf("Rpr_bypc_pin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp_res[stoich$tp_in_conc<conc.q[1]]~log10(stoich$np_in[stoich$tp_in_conc<conc.q[1]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc<conc.q[1]], ylim = c(-1.5, .5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
#legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rp_res[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]~log10(stoich$np_in[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]], ylim = c(-1.5,.5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp_res[stoich$tp_in_conc >=conc.q[2] & stoich$tp_in_conc<conc.q[3]]~log10(stoich$np_in[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]], ylim = c(-1.5,0.5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp_res[stoich$tp_in_conc>=conc.q[3]]~log10(stoich$np_in[stoich$tp_in_conc>=conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[3]&stoich$tp_in_conc<478], ylim = c(-1.5, 0.5), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log P in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)
dev.off()


pdf("Npr_bypc_pin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn_res[stoich$tp_in_conc<conc.q[1]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc<conc.q[1]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc<conc.q[1]], ylim = c(-1.3, 1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
#legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rn_res[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]], ylim = c(-1.3,1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn_res[stoich$tp_in_conc >=conc.q[2] & stoich$tp_in_conc<conc.q[3]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]], ylim = c(-1.3,1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn_res[stoich$tp_in_conc>=conc.q[3]]~log10(stoich$tp_in_mass_aerial[stoich$tp_in_conc>=conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[3]&stoich$tp_in_conc<478], ylim = c(-1.3, 1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log P in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)
dev.off()

pdf("Npr_bypc_nin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn_res[stoich$tp_in_conc<conc.q[1]]~log10(stoich$tn_in_mass_aerial[stoich$tp_in_conc<conc.q[1]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc<conc.q[1]], ylim = c(-1.3, 1), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)
#legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rn_res[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]~log10(stoich$tn_in_mass_aerial[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[1]&stoich$tp_in_conc<conc.q[2]], ylim = c(-1.3,1), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn_res[stoich$tp_in_conc >=conc.q[2] & stoich$tp_in_conc<conc.q[3]]~log10(stoich$tn_in_mass_aerial[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[2]&stoich$tp_in_conc<conc.q[3]], ylim = c(-1.3,1), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn_res[stoich$tp_in_conc>=conc.q[3]]~log10(stoich$tn_in_mass_aerial[stoich$tp_in_conc>=conc.q[3]]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$tp_in_conc>=conc.q[3]&stoich$tp_in_conc<478], ylim = c(-1.3, 1), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log N in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)
dev.off()

pdf("Rp_restime_pin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp[stoich$res_time<.0872]~log10(stoich$tp_in_mass_aerial[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 1.3), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rp[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 1.3), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 1.3), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 1.3), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log P in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

dat.p.real$tp_in_mass_aerial <- dat.p.real$tp_in_mass/dat.p.real$surface_area
dat.p.real$tp_out_mass_aerial <- dat.p.real$tp_out_mass/dat.p.real$surface_area
dat.p.real$tp_r_mass_aerial <- dat.p.real$tp_in_mass_aerial-dat.p.real$tp_out_mass_aerial
dat.p.real$trophic <- "mesotrophic"
dat.p.real$trophic[dat.p.real$tp_out_conc < .010] <- "oligotrophic"
dat.p.real$trophic[dat.p.real$tp_out_conc > .030] <- "eutrophic"

dat.n.real$tn_in_mass_aerial <- dat.n.real$tn_in_mass/dat.n.real$surface_area
dat.n.real$tn_out_mass_aerial <- dat.n.real$tn_out_mass/dat.n.real$surface_area
dat.n.real$tn_r_mass_aerial <- dat.n.real$tn_in_mass_aerial-dat.n.real$tn_out_mass_aerial
dat.n.real$trophic <- "mesotrophic"
dat.n.real$trophic[dat.n.real$tp_out_conc < .010] <- "oligotrophic"
dat.n.real$trophic[dat.n.real$tp_out_conc > .030] <- "eutrophic"

palette(c(rgb(215,25,28,100,max=255), rgb(255,255,191,250, max=255), rgb(44,123,182,200, max = 255)))
shapes <- as.factor(dat.p.real$trophic)
levels(shapes) = c(24,21,22)
shapes <- as.numeric(as.character(shapes))

dat1 <- dat.p.real[dat.p.real$res_time<.0872&!is.na(dat.p.real$Rp)&!is.na(dat.p.real$tp_in_mass_aerial)&dat.p.real$tp_in_mass_aerial!= Inf & !is.na(dat.p.real$trophic), ]
dat2 <- dat.p.real[dat.p.real$res_time>=.0872&dat.p.real$res_time<.4025&!is.na(dat.p.real$Rp)&!is.na(dat.p.real$tp_in_mass_aerial)&dat.p.real$tp_in_mass_aerial!= Inf & !is.na(dat.p.real$trophic), ]
dat3 <- dat.p.real[dat.p.real$res_time>=.4025&dat.p.real$res_time<1.2&!is.na(dat.p.real$Rp)&!is.na(dat.p.real$tp_in_mass_aerial)&dat.p.real$tp_in_mass_aerial!= Inf & !is.na(dat.p.real$trophic), ]

test.1 <- aov(Rp~log10(tp_in_mass_aerial)*trophic, data = dat1)
test.12 <- aov(Rp~log10(tp_in_mass_aerial) + trophic, data = dat1)
test.1.e <- lm(Rp~log10(tp_in_mass_aerial), data = dat1[dat1$trophic == "eutrophic",])
test.1.m <- lm(Rp~log10(tp_in_mass_aerial), data = dat1[dat1$trophic == "mesotrophic",])
test.1.o <- lm(Rp~log10(tp_in_mass_aerial), data = dat1[dat1$trophic == "oligotrophic",])

test.2 <- aov(Rp~log10(tp_in_mass_aerial)*trophic, data = dat2)
#test.22 <- aov(Rp~log10(tp_in_mass_aerial) + trophic, data = dat2)
test.2.e <- lm(Rp~log10(tp_in_mass_aerial), data = dat2[dat2$trophic == "eutrophic",])
test.2.m <- lm(Rp~log10(tp_in_mass_aerial), data = dat2[dat2$trophic == "mesotrophic",])
test.2.o <- lm(Rp~log10(tp_in_mass_aerial), data = dat2[dat2$trophic == "oligotrophic",])

test.3 <- aov(Rp~log10(tp_in_mass_aerial)*trophic, data = dat3)
test.32 <- aov(Rp~log10(tp_in_mass_aerial) + trophic, data = dat3)
test.3.e <- lm(Rp~log10(tp_in_mass_aerial), data = dat3[dat3$trophic == "eutrophic",])
test.3.m <- lm(Rp~log10(tp_in_mass_aerial), data = dat3[dat3$trophic == "mesotrophic",])
test.3.o <- lm(Rp~log10(tp_in_mass_aerial), data = dat3[dat3$trophic == "oligotrophic",])

dat.p.real$Rp_res = dat.p.real$Rp - dat.p.real$Rp_predicted

dat.0 <- dat.p.real[!is.na(dat.p.real$Rp)&!is.na(dat.p.real$tp_in_mass_aerial)&dat.p.real$tp_in_mass_aerial!= Inf & !is.na(dat.p.real$trophic), ]

test.all <- aov(Rp_res~log10(tp_in_mass_aerial)*trophic, data = dat.0)
test.all.2 <- aov(Rp_res~log10(tp_in_mass_aerial) + trophic, data = dat.0)
test.all.e <- lm(Rp_res~log10(tp_in_mass_aerial), data = dat.0[dat.0$trophic == "eutrophic",])
test.all.m <- lm(Rp_res~log10(tp_in_mass_aerial), data = dat.0[dat.0$trophic == "mesotrophic",])
test.all.o <- lm(Rp_res~log10(tp_in_mass_aerial), data = dat.0[dat.0$trophic == "oligotrophic",])

pdf("Rp_restime_pin_trophic.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(dat.p.real$Rp[dat.p.real$res_time<.0872]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time<.0872]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.1, 1.1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Trophic Status", legend = c("Eutrophic", "Mesotrophic", "Oligotrophic"), pch = c(24,21,22),pt.bg = c(1,2,3), cex = 1, pt.cex = 1.3)
plot(dat.p.real$Rp[dat.p.real$res_time>=.0872&dat.p.real$res_time<.4025]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time>=.0872&dat.p.real$res_time<.4025]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.1, 1.1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(dat.p.real$Rp[dat.p.real$res_time>=.4025&dat.p.real$res_time<1.2]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time>=.4025&dat.p.real$res_time<1.2]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.1, 1.1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(dat.p.real$Rp[dat.p.real$res_time>=1.2&dat.p.real$res_time<478]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time>=1.2&dat.p.real$res_time<478]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.1, 1.1), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext(expression(paste("log P in (kg ", h^-1, y^-1, " )")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()


pdf("RpR_restime_pin_trophic.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(dat.p.real$Rp_res[dat.p.real$res_time<.0872]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time<.0872]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.5, .7), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Trophic Status", legend = c("Eutrophic", "Mesotrophic", "Oligotrophic"), pch = c(24,21,22),pt.bg = c(1,2,3), cex = 1, pt.cex = 1.3)
plot(dat.p.real$Rp_res[dat.p.real$res_time>=.0872&dat.p.real$res_time<.4025]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time>=.0872&dat.p.real$res_time<.4025]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.5, .7), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(dat.p.real$Rp_res[dat.p.real$res_time>=.4025&dat.p.real$res_time<1.2]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time>=.4025&dat.p.real$res_time<1.2]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.5, .7), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(dat.p.real$Rp_res[dat.p.real$res_time>=1.2&dat.p.real$res_time<478]~log10(dat.p.real$tp_in_mass_aerial[dat.p.real$res_time>=1.2&dat.p.real$res_time<478]), cex.lab = 1.8, cex = 1.1, 
     xlab = "", ylab = "",
     pch = shapes, bg = as.factor(dat.p.real$trophic), ylim = c(-1.5, .7), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("Rp Residual", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext(expression(paste("log P in (kg ", h^-1, y^-1, " )")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

pdf("Rn_restime_pin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<.0872]~log10(stoich$tp_in_mass_aerial[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(-1,6))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("N Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log P in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

pdf("Rp_restime_nin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp[stoich$res_time<.0872]~log10(stoich$tn_in_mass_aerial[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rp[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rp[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log N in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

pdf("Rn_restime_nc.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<.0872]~log10(stoich$tn_in_conc[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(-3,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tn_in_conc[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(-3,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tn_in_conc[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(-3,4))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$tn_in_conc[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(-3,4))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("N Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log N in (ug/L)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

pdf("Rn_restime_nin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<.0872]~log10(stoich$tn_in_mass_aerial[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(1,7))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("N Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log N in (kg h-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

pdf("Rn_restime_stoichin.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<.0872]~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
legend("topleft", title = "Depth Percentile", legend = c("<20th", "20-50th", "50-80th", ">80th"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)
plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)

plot(stoich$Rn[stoich$res_time>=1.2&stoich$res_time<478]~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.4, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1.3, 2), xlim = c(-0.5,2.5))
abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("N Retention", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input N:P", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

#############################################
# Fig. - recreate Finlays N retention vs trophic status
#################################################

stoich$trophic <- "mesotrophic"
stoich$trophic[stoich$tp_out_conc < .010] <- "oligotrophic"
stoich$trophic[stoich$tp_out_conc > .030] <- "eutrophic"

palette(c(rgb(166,217,106,200, max = 255), rgb(222,222,222,200, max=255), rgb(5,113,176,200,max=255)))
shapes <- as.factor(stoich$trophic)
levels(shapes) = c(24,21,25)
shapes <- as.numeric(as.character(shapes))
plot(stoich$Rn~log10(stoich$res_time), bg = as.factor(stoich$trophic), 
     ylim = c(-1,1), cex = 1.2, pch = shapes)
plot(stoich$Rp~log10(stoich$res_time), bg = as.factor(stoich$trophic), 
     ylim = c(-1,1), cex = 1.2, pch = shapes)

plot(log10(stoich$tn_r_mass_aerial)~log10(stoich$tn_in_mass_aerial),
     bg = as.factor(stoich$trophic), 
      cex = 1.2, pch = shapes)
plot(stoich$Rn~log10(stoich$res_time),
     bg = as.factor(stoich$trophic), 
     cex = 1.2, pch = shapes)
plot(log10(stoich$tn_r_mass_aerial[stoich$trophic == "eutrophic"])~log10(stoich$tn_in_mass_aerial[stoich$trophic == "eutrophic"]),
     cex = 1.2, pch = 16, col = "darkgreen")
temp <- stoich[stoich$Rn > 0,]
plot(log10(temp$tn_r_mass_aerial[temp$trophic == "eutrophic"])~log10(temp$tn_in_mass_aerial[temp$trophic == "eutrophic"]))
lm.eutrophic <- lm(log10(temp$tn_r_mass_aerial[temp$trophic == "eutrophic"])~log10(temp$tn_in_mass_aerial[temp$trophic == "eutrophic"]))
lm.mesotrophic <- lm(log10(temp$tn_r_mass_aerial[temp$trophic == "mesotrophic"])~log10(temp$tn_in_mass_aerial[temp$trophic == "mesotrophic"]))
lm.oligotrophic <- lm(log10(temp$tn_r_mass_aerial[temp$trophic == "oligotrophic"])~log10(temp$tn_in_mass_aerial[temp$trophic == "oligotrophic"]))

lm.r.eutrophic <- lm(stoich$Rn[stoich$trophic == "eutrophic"]~log10(stoich$res_time[stoich$trophic == "eutrophic"]))
lm.r.mesotrophic <- lm(stoich$Rn[stoich$trophic == "mesotrophic"]~log10(stoich$res_time[stoich$trophic == "mesotrophic"]))
lm.r.oligotrophic <- lm(stoich$Rn[stoich$trophic == "oligotrophic"]~log10(stoich$res_time[stoich$trophic == "oligotrophic"]))

lm.h.eutrophic <- lm(stoich$Rn[stoich$trophic == "eutrophic"]~log10(stoich$h[stoich$trophic == "eutrophic"]))
lm.h.mesotrophic <- lm(stoich$Rn[stoich$trophic == "mesotrophic"]~log10(stoich$h[stoich$trophic == "mesotrophic"]))
lm.h.oligotrophic <- lm(stoich$Rn[stoich$trophic == "oligotrophic"]~log10(stoich$h[stoich$trophic == "oligotrophic"]))

plot(log10(stoich$tn_r_mass_aerial[stoich$trophic == "mesotrophic"])~log10(stoich$tn_in_mass_aerial[stoich$trophic == "mesotrophic"]),
     cex = 1.2, pch = 16, col = "orange")
plot(log10(stoich$tn_r_mass_aerial[stoich$trophic == "mesotrophic"])~log10(stoich$tn_in_mass_aerial[stoich$trophic == "mesotrophic"]),
     cex = 1.2, pch = 16, col = "darkblue")

##################################################
# Fig plot aerial mass remval vs aerial load
# similar to Finlay plot
##################################################


restime.cols <- brewer.pal(7, "PuBu")[c(2,4,6,7)]
restime.cols.a <- adjustcolor(restime.cols, alpha = 0.7)
pdf("out_in.pdf", height = 5, width = 12)
par(mar=c(5,5,1,.5), mfrow=c(1,3), cex =1)
plot(log10(dat.n.pos$tn_r_mass_areal)~log10(dat.n.pos$tn_in_mass_areal),
     cex = 1.2, xlab = expression(paste("TN in (g ", m^-2," ", y^-1, ")", sep = "")), ylab = expression(paste("TN removed (g ", m^-2, " ", y^-1, ")", sep = "")),
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "white")
points(log10(dat.n.pos$tn_r_mass_areal[dat.n.pos$res_time>=1.2])~log10(dat.n.pos$tn_in_mass_areal[dat.n.pos$res_time>=1.2]),
       col = restime.cols.a[4], pch = 16, cex = 1.2)
points(log10(dat.n.pos$tn_r_mass_areal[dat.n.pos$res_time>=0.3975 & dat.n.pos$res_time<1.2])~log10(dat.n.pos$tn_in_mass_areal[dat.n.pos$res_time>=0.3975 & dat.n.pos$res_time<1.2]),
       col = restime.cols.a[3], pch = 16, cex = 1.2) 
points(log10(dat.n.pos$tn_r_mass_areal[dat.n.pos$res_time>=0.0850 & dat.n.pos$res_time<.3975])~log10(dat.n.pos$tn_in_mass_areal[dat.n.pos$res_time>=0.0850 & dat.n.pos$res_time<.3975]),
       col = restime.cols.a[2], pch = 16,cex = 1.2) 
points(log10(dat.n.pos$tn_r_mass_areal[dat.n.pos$res_time<.085])~log10(dat.n.pos$tn_in_mass_areal[dat.n.pos$res_time<.085]),
       col = restime.cols.a[1], pch = 16, cex = 1.2)                                                                                 
                                                                                
abline(0,1,col = "red", lwd = 2, lty = 2)
abline(-0.53, 0.80, col = restime.cols[1], lwd = 2, lty = 1)
abline(-0.91, 1.14, col = restime.cols[2], lwd = 2, lty = 1)
abline(-0.72, 1.09, col = restime.cols[3], lwd = 2, lty = 1)
abline(-0.41, 1.02, col = restime.cols[4], lwd = 2, lty = 1)
legend("topleft", legend = c("< 1 month", "1-5 months", "0.4-1.2 yrs", "> 1.2 yrs"), 
       title = "Residence time",bty = "n", pch = 16, col = c(restime.cols[1], restime.cols[2], restime.cols[3], restime.cols[4]), cex = 1, pt.cex = 1.4)

plot(log10(dat.p.pos$tp_r_mass_areal)~log10(dat.p.pos$tp_in_mass_areal),
     cex = 1.2, xlab = expression(paste("TP in (g ", m^-2, " ", y^-1, ")", sep = "")), ylab = expression(paste("TP removed (g ", m^-2," ", y^-1, ")", sep = "")),
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "white")
points(log10(dat.p.pos$tp_r_mass_areal[dat.p.pos$res_time>=1.2])~log10(dat.p.pos$tp_in_mass_areal[dat.p.pos$res_time>=1.2]),
       col = restime.cols.a[4], pch = 16, cex = 1.2)
points(log10(dat.p.pos$tp_r_mass_areal[dat.p.pos$res_time>=0.3975 & dat.p.pos$res_time<1.2])~log10(dat.p.pos$tp_in_mass_areal[dat.p.pos$res_time>=0.3975 & dat.p.pos$res_time<1.2]),
       col = restime.cols.a[3], pch = 16, cex = 1.2) 
points(log10(dat.p.pos$tp_r_mass_areal[dat.p.pos$res_time>=0.0850 & dat.p.pos$res_time<.3975])~log10(dat.p.pos$tp_in_mass_areal[dat.p.pos$res_time>=0.0850 & dat.p.pos$res_time<.3975]),
       col = restime.cols.a[2], pch = 16,cex = 1.2) 
points(log10(dat.p.pos$tp_r_mass_areal[dat.p.pos$res_time<.085])~log10(dat.p.pos$tp_in_mass_areal[dat.p.pos$res_time<.085]),
       col = restime.cols.a[1], pch = 16, cex = 1.2)                                                                                 

abline(0,1,col = "red", lwd = 2, lty = 2)
abline(-0.76, 1.08, col = restime.cols[1], lwd = 2, lty = 1)
abline(-0.62, 1.30, col = restime.cols[2], lwd = 2, lty = 1)
abline(-0.37, 1.25, col = restime.cols[3], lwd = 2, lty = 1)
abline(-0.21, 1.09, col = restime.cols[4], lwd = 2, lty = 1)

plot(log10(stoich$np_r)~log10(stoich$np_in),
     cex = 1.2, xlab = "N:P in (moles)", ylab = "N:P removed (moles)",
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "white")
points(log10(stoich$np_r[stoich$res_time>=1.2])~log10(stoich$np_in[stoich$res_time>=1.2]),
       col = restime.cols.a[4], pch = 16, cex = 1.2)
points(log10(stoich$np_r[stoich$res_time>=0.3975 & stoich$res_time<1.2])~log10(stoich$np_in[stoich$res_time>=0.3975 & stoich$res_time<1.2]),
       col = restime.cols.a[3], pch = 16, cex = 1.2) 
points(log10(stoich$np_r[stoich$res_time>=0.0850 & stoich$res_time<.3975])~log10(stoich$np_in[stoich$res_time>=0.0850 & stoich$res_time<.3975]),
       col = restime.cols.a[2], pch = 16,cex = 1.2) 
points(log10(stoich$np_r[stoich$res_time<.085])~log10(stoich$np_in[stoich$res_time<.085]),
       col = restime.cols.a[1], pch = 16, cex = 1.2)                                                                                 
abline(0,1,col = "red", lwd = 2, lty = 2)
abline(-0.8, 1.27, col = restime.cols[1], lwd = 2, lty = 1)
abline(-0.61, 1.27, col = restime.cols[2], lwd = 2, lty = 1)
abline(-0.69, 1.27, col = restime.cols[3], lwd = 2, lty = 1)
abline(-0.54, 1.27, col = restime.cols[4], lwd = 2, lty = 1)

dev.off()

#####################################################
# Figs - removal vs H
#####################################################
pdf("Rn_h_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<0.0872]~log10(stoich$h[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$h[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$h[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= 0.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$Rn[stoich$res_time>=1.2]~log10(stoich$h[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext("Rn", side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext("H", side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

pdf("Rp_h_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp[stoich$res_time<0.0872]~log10(stoich$h[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(stoich$Rp[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$h[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(stoich$Rp[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$h[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= 0.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$Rp[stoich$res_time>=1.2]~log10(stoich$h[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", ylim = c(-1,1), xlim = c(-1,4),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext("Rp", side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext("H", side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()
####################################################
# Figs aerial mass removed vs aerial mass in x panels of res time
####################################################
pdf("N_r_in_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$tn_r_mass_aerial[stoich$res_time<0.0872])~log10(stoich$tn_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
text(x = 1, y= 6.8, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(log10(stoich$tn_r_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = 1, y= 6.8, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(log10(stoich$tn_r_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = 1, y= 6.8, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(log10(stoich$tn_r_mass_aerial[stoich$res_time>=1.2])~log10(stoich$tn_in_mass_aerial[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(2, labels = FALSE)
text(x = 1, y= 6.8, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext(expression(paste("log TN removed (kg ",m^-2," ", y^-1,")")), side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext(expression(paste("log TN in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

stoich$n_out_in <- stoich$tn_out_mass/stoich$tn_in_mass
stoich$p_out_in <- stoich$tp_out_mass/stoich$tp_in_mass

pdf("N_out_in_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$tn_out_mass_aerial[stoich$res_time<0.0872])~log10(stoich$tn_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
text(x = 1, y= 6.8, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(log10(stoich$tn_out_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = 1, y= 6.8, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(log10(stoich$tn_out_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = 1, y= 6.8, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(log10(stoich$tn_out_mass_aerial[stoich$res_time>=1.2])~log10(stoich$tn_in_mass_aerial[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(1,7),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(2, labels = FALSE)
text(x = 1, y= 6.8, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext(expression(paste("log TN out (kg ",m^-2," ", y^-1,")")), side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext(expression(paste("log TN in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()


pdf("P_r_in_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$tp_r_mass_aerial[stoich$res_time<0.0872])~log10(stoich$tp_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,6),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
text(x = -1, y= 5.8, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(log10(stoich$tp_r_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,6),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 5.8, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(log10(stoich$tp_r_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,6),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = -1, y= 5.8, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(log10(stoich$tp_r_mass_aerial[stoich$res_time>=1.2])~log10(stoich$tp_in_mass_aerial[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "TP removed (kg/m2 y)", xlim = c(-1,6), ylim = c(-1,6),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(2, labels = FALSE)
text(x = -1, y= 5.8, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext(expression(paste("log TP removed (kg ",m^-2," ", y^-1,")")), side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext(expression(paste("log TP in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

pdf("P_out_in_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$tp_out_mass_aerial[stoich$res_time<0.0872])~log10(stoich$tp_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,6),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
text(x = -1, y= 5.8, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(log10(stoich$tp_out_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,6),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 5.8, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(log10(stoich$tp_out_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,6),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
text(x = -1, y= 5.8, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(log10(stoich$tp_out_mass_aerial[stoich$res_time>=1.2])~log10(stoich$tp_in_mass_aerial[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "TP removed (kg/m2 y)", xlim = c(-1,6), ylim = c(-1,6),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(2, labels = FALSE)
text(x = -1, y= 5.8, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext(expression(paste("log TP out (kg ",m^-2," ", y^-1,")")), side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext(expression(paste("log TP in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

pdf("NP_r_in_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$np_r[stoich$res_time<0.0872])~log10(stoich$np_in[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1.5,3), ylim = c(-1.5,3),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
plot(log10(stoich$np_r[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(-1.5,3), ylim = c(-1.5,3),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
plot(log10(stoich$np_r[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1.5,3), ylim = c(-1.5,3),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
plot(log10(stoich$np_r[stoich$res_time>=1.2])~log10(stoich$np_in[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "TP removed (kg/m2 y)", xlim = c(-1.5,3), ylim = c(-1.5,3),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1, lty = 2, col = "red", lwd = 2)
axis(2, labels = FALSE)
mtext("N:P retained", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("N:P in", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

###############################################################
# Fig out or R vs TN:TP in x
###############################################################
pdf("P_out_stoichin_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$tp_out_mass_aerial[stoich$res_time<.0872])~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
plot(log10(stoich$tp_out_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
plot(log10(stoich$tp_out_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(log10(stoich$tp_out_mass_aerial[stoich$res_time>=1.2&stoich$res_time<478])~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P out (kg", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input TN:TP (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 0, y= -.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)
legend("topright", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)

dev.off()

pdf("P_r_stoichin_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$tp_r_mass_aerial[stoich$res_time<.0872])~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
plot(log10(stoich$tp_r_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
plot(log10(stoich$tp_r_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= -.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(log10(stoich$tp_r_mass_aerial[stoich$res_time>=1.2&stoich$res_time<478])~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(-1, 6), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("P removed (kg ha-1 y-1)", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input TN:TP (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 0, y= -.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)
legend("topright", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)

dev.off()

pdf("N_r_stoichin_depth.pdf")
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(log10(stoich$tn_r_mass_aerial[stoich$res_time<.0872])~log10(stoich$np_in[stoich$res_time<.0872]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time<.0872], ylim = c(1, 7), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= 1, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(log10(stoich$tn_r_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025])~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=.0872&stoich$res_time<.4025], ylim = c(1, 7), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= 1, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
plot(log10(stoich$tn_r_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2])~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "Change in Stoichiometry",
     pch = 21, bg = stoich$colors[stoich$res_time>=.4025&stoich$res_time<1.2], ylim = c(1, 7), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
text(x = 0, y= 1, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(log10(stoich$tn_r_mass_aerial[stoich$res_time>=1.2&stoich$res_time<478])~log10(stoich$np_in[stoich$res_time>=1.2&stoich$res_time<478]), cex.lab = 1.8, cex = 1.3, 
     xlab = "", ylab = "",
     pch = 21, bg = stoich$colors[stoich$res_time>=1.2&stoich$res_time<478], ylim = c(1, 7), xlim = c(0,4))
#abline(h=0, lty = 2, col = "red", lwd = 2)
mtext("N removed (kg ha-1 y-1)", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log Input TN:TP (m)", side = 1,cex=1.7, outer = TRUE, line=1.5)
text(x = 0, y= 1, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)
legend("topright", title = "Depth Percentiles", legend = c("<20th (<2.7m)", "20-50th (2.7-5.9m)", "50-80th (5.9-13.8m)", ">80th (>13.8m)"), pch = 21, pt.bg = stoich.cols, cex = 1, pt.cex = 1.3)

dev.off()



###############################################################
# Fig R vs aerial loading of same nutrient x panels of res time
###############################################################

pdf("Rp_in_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp[stoich$res_time<0.0872]~log10(stoich$tp_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,6), ylim = c(-1,1), xaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
axis(1, labels = FALSE)
plot(stoich$Rp[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,6), ylim = c(-1,1), xaxt = "n", yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
plot(stoich$Rp[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,6), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)
plot(stoich$Rp[stoich$res_time>=1.2]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=1.2]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,6), ylim = c(-1,1),yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)
axis(2, labels = FALSE)
mtext("Rp", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("P in (kg m-1 y-1)", side = 1,cex=1.7, outer = TRUE, line=1.5)
dev.off()

pdf("Rn_in_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<0.0872]~log10(stoich$tn_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "Rn", xlim = c(1,7), ylim = c(-1,1), xaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = 1, y= -.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
axis(1, labels = FALSE)
plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     cex = 1.2, xlab = "tn in (kg/m2 y)", ylab = "Rn", xlim = c(1,7), ylim = c(-1,1), xaxt = "n", yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x =1, y= -.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "tn in (kg/m2 y)", ylab = "Rn", xlim = c(1,7), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = 1, y= -.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)
plot(stoich$Rn[stoich$res_time>=1.2]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=1.2]),
     cex = 1.2, xlab = "tn in (kg/m2 y)", ylab = "Rn", xlim = c(1,7), ylim = c(-1,1),yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = 1, y= -.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)
axis(2, labels = FALSE)
mtext("Rn", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext(expression(paste("log N in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)
dev.off()

####################################################
# Fig Relationship between  R and loading characteristics
###################################################
pdf("Rp_NPin_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp[stoich$res_time<0.0872]~log10(stoich$np_in[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,3), ylim = c(-1,1), xaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
axis(1, labels = FALSE)
plot(stoich$Rp[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,3), ylim = c(-1,1), xaxt = "n", yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
plot(stoich$Rp[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,3), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)
plot(stoich$Rp[stoich$res_time>=1.2]~log10(stoich$np_in[stoich$res_time>=1.2]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rp", xlim = c(-1,3), ylim = c(-1,1),yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)
axis(2, labels = FALSE)
mtext("Rp", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log TN:TP in", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

pdf("Rn_NPin_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<0.0872]~log10(stoich$np_in[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rn", xlim = c(-1,3), ylim = c(-1,1), xaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)
axis(1, labels = FALSE)
plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rn", xlim = c(-1,3), ylim = c(-1,1), xaxt = "n", yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rn", xlim = c(-1,3), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)
plot(stoich$Rn[stoich$res_time>=1.2]~log10(stoich$np_in[stoich$res_time>=1.2]),
     cex = 1.2, xlab = "TP in (kg/m2 y)", ylab = "Rn", xlim = c(-1,3), ylim = c(-1,1),yaxt = "n",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= -.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)
axis(2, labels = FALSE)
mtext("Rn", side = 2,cex=1.7, outer = TRUE, line=1.5)
mtext("log TN:TP in", side = 1,cex=1.7, outer = TRUE, line=1.5)

dev.off()

pdf("RnR_Pin_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn_res[stoich$res_time<0.0872]~log10(stoich$tp_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,1),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(stoich$Rn_res[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(stoich$Rn_res[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= 0.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$Rn_res[stoich$res_time>=1.2]~log10(stoich$tp_in_mass_aerial[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", xlim = c(-1,6), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext(expression(paste("log TN removed (kg ",m^-2," ", y^-1,")")), side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext(expression(paste("log TP in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

#pdf("RnR_Pin_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rn[stoich$res_time<0.0872]~log10(stoich$tp_in_conc[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-4,4), ylim = c(-1,1),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(stoich$Rn[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tp_in_conc[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(-4,4), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(stoich$Rn[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tp_in_conc[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-4,4), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= 0.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$Rn[stoich$res_time>=1.2]~log10(stoich$tp_in_conc[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", xlim = c(-4,4), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext(expression(paste("log TN removed (kg ",m^-2," ", y^-1,")")), side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext(expression(paste("log TP in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

pdf("RpR_Nin_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp_res[stoich$res_time<0.0872]~log10(stoich$tn_in_mass_aerial[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(-1,1),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(stoich$Rp_res[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(stoich$Rp_res[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= 0.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$Rp_res[stoich$res_time>=1.2]~log10(stoich$tn_in_mass_aerial[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", xlim = c(1,7), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext("Rp", side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext(expression(paste("log TN in (kg ",m^-2," ", y^-1,")")), side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()
pdf("RpR_npin_restime.pdf", height = 8, width = 8)
par(mfrow=c(2,2), mar=c(1.5,1.5,1,1), oma = c(4,4,0,0))
plot(stoich$Rp_res[stoich$res_time<0.0872]~log10(stoich$np_in[stoich$res_time<0.0872]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,4), ylim = c(-1,1),
     xaxt = "n", cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " < 1 month")), col = "red", pos = 4)

plot(stoich$Rp_res[stoich$res_time>=.0872&stoich$res_time<.4025]~log10(stoich$np_in[stoich$res_time>=.0872&stoich$res_time<.4025]),
     xaxt = "n", yaxt = "n",cex = 1.2, xlab = "", ylab = "", xlim = c(-1,4), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(1, labels = FALSE)
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " = 1-5 months")), col = "red", pos = 4)

plot(stoich$Rp_res[stoich$res_time>=.4025&stoich$res_time<1.2]~log10(stoich$np_in[stoich$res_time>=.4025&stoich$res_time<1.2]),
     cex = 1.2, xlab = "", ylab = "", xlim = c(-1,4), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
text(x = -1, y= 0.9, expression(paste(tau, " = 0.4-1.2 years")), col = "red", pos = 4)

plot(stoich$Rp_res[stoich$res_time>=1.2]~log10(stoich$np_in[stoich$res_time>=1.2]),
     yaxt = "n", cex = 1.2, xlab = "", ylab = "", xlim = c(-1,4), ylim = c(-1,1),
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
axis(2, labels = FALSE)
text(x = -1, y= 0.9, expression(paste(tau, " > 1.2 years")), col = "red", pos = 4)

mtext("Rp", side = 2,cex=1.7, outer = TRUE, line=1.7)
mtext("log N:P in", side = 1,cex=1.7, outer = TRUE, line=1.7)

dev.off()

plot(log10(stoich$Rp)~log10(stoich$tp_in_mass_aerial),
     cex = 1.2, ylab = "N:P removal", xlab = "P in",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
plot(log10(stoich$np_r)~log10(stoich$np_in),
     cex = 1.2, ylab = "N:P removal", xlab = "N:P in",
     cex.lab = 1.5, cex.axis = 1.2, pch = 21, col = rgb(160,160,160,200,max=255),bg =rgb(200,200,200,150,max=255))
abline(0,1,col = "red", lwd = 2)
dev.off()
plot(stoich$Rp~log10(stoich$tp_in_mass_aerial), ylim = c(-1,1))
plot(stoich$Rn~log10(stoich$tn_in_mass_aerial), ylim = c(-1,1))

plot(stoich$Rp~log10(stoich$tn_in_mass_aerial), ylim = c(-1,1))
plot(stoich$Rn~log10(stoich$tp_in_mass_aerial), ylim = c(-1,1))

# at high input N:P, it appears P limitation limits P removal, but not N
#
plot(log10(stoich$np_in)~log10(stoich$tp_in_mass_aerial))
plot(log10(stoich$np_in)~log10(stoich$tn_in_mass_aerial))

plot(stoich$Rp~log10(stoich$np_in), ylim = c(-1,1))
abline(0,1,col = "red", lwd = 2)

# create a residuals column for each N and P
stoich$Rp_res <- stoich$Rp - stoich$Rp_predicted
stoich$Rn_res <- stoich$Rn - stoich$Rn_predicted

plot(stoich$Rn_res[stoich$Rp>0] ~ log10(stoich$np_in[stoich$Rp>0]), ylim = c(-1,1))
plot(stoich$Rp_res[stoich$Rp>0] ~ log10(stoich$np_in[stoich$Rp>0]), ylim = c(-1,1))
fit.lm <-  lm(stoich$Rp_res[stoich$Rp>0] ~ log10(stoich$np_in[stoich$Rp>0]))

plot(stoich$Rn_res[stoich$Rn>0] ~ log10(stoich$np_out[stoich$Rn>0]), ylim = c(-1,1))
plot(stoich$Rp_res[stoich$Rp>0] ~ log10(stoich$np_out[stoich$Rp>0]), ylim = c(-1,1))

plot(stoich$Rn_res[stoich$Rn>0] ~ log10(stoich$tp_out_conc[stoich$Rn>0]), ylim = c(-1,1))
plot(stoich$Rp_res[stoich$Rp>0] ~ log10(stoich$np_in[stoich$Rp>0]), ylim = c(-1,1))

abline(fit.lm, col = "red")
abline(h = 0, col = "blue", lty = 2)
abline(v = log10(40), col = "green", lwd = 2)

#########################################################
# T-test for difference between Rn and Rp
#########################################################

Rdiff <- t.test(x = log10(stoich$Rn+1), y = log10(stoich$Rp+1), paired = TRUE)
Rdiffnp <- wilcox.test(x = stoich$Rn, y = stoich$Rp, paired = TRUE,
                       conf.int = TRUE)
Rdiffnpstoich <- t.test(x = log10(stoich$np_in), y = log10(stoich$np_out), paired = TRUE)
Rdiffnpstoichnp <- wilcox.test(x = stoich$np_in, y = stoich$np_out, paired = TRUE, conf.int = TRUE)

###################################
# Fig N:P change vs N:P input
###################################
pdf("ChangeNP_npin_all.pdf")
par(mar = c(5,5,1,1))
plot(stoich$np_change~log10(stoich$np_in), xlab = "log N:P in", ylab = "Change in Stoichiometry",
     cex = 1.2, pch = 16, col = rgb(122,122,122,max = 255,100), cex.lab =1.5)
change.lm <- lm(stoich$np_change~log10(stoich$np_in))
abline(change.lm, col = "red", lwd = 2)
abline(h=0, lty = 2)
abline(v = log10(44), col = col.n, lty = 2, lwd = 2)
abline(v = log10(110), col = col.p, lty = 2, lwd = 2)
text(2.2, 2, "P-limited", col = col.p, pos = 4, cex = 1.5)
text(1.45, 2, "N-limited", col = col.n3, pos = 2, cex = 1.5)
text(4, 3, pos = 2, expression(paste(R^2, " = 0.37")), col = "red")
dev.off

temp <- stoich[log10(stoich$np_in)>0.5& log10(stoich$np_in)<2.5, ]
pdf("ChangeNP_npin_noout.pdf")
par(mar = c(5,5,1,1))
plot(temp$np_change~log10(temp$np_in), xlab = "log N:P in", ylab = "Change in Stoichiometry",
     cex = 1.2, pch = 16, col = rgb(122,122,122,max = 255,100), cex.lab =1.5)
change.lm2 <- lm(temp$np_change~log10(temp$np_in))

abline(change.lm2, col = "red", lwd = 2)
#abline(change.lm, col = "blue", lwd = 2)
abline(h=0, lty = 2)
abline(v = log10(44), col = col.n, lty = 2, lwd = 2)
abline(v = log10(110), col = col.p, lty = 2, lwd = 2)
text(2.1, 1.1, "P-limited", col = col.p, pos = 4, cex = 1.3)
text(1.55, 1.1, "N-limited", col = col.n3, pos = 2, cex = 1.3)
text(4, 3, pos = 2, expression(paste(R^2, " = 0.37")), col = "red")
dev.off()

pdf("P_r_npin_all.pdf")
par(mar = c(5,5,1,1))
plot(log10(stoich$tp_r_mass_aerial)~log10(stoich$np_in), xlab = "log N:P in", ylab = "log P removed (kg/ha y)",
     cex = 1.2, pch = 16, col = rgb(122,122,122,max = 255,100), cex.lab =1.5)
#change.lm <- lm(log10(stoich$tp_r_mass_aerial[stoich$tp_r_mass_aerial>0&stoich$np_in])~log10(stoich$np_in[stoich$tp_r_mass_aerial>0]))
#abline(change.lm, col = "red", lwd = 2)
#abline(h=0, lty = 2)
abline(v = log10(44), col = col.n, lty = 2, lwd = 2)
abline(v = log10(110), col = col.p, lty = 2, lwd = 2)
#text(2.2, 2, "P-limited", col = col.p, pos = 4, cex = 1.5)
#text(1.45, 2, "N-limited", col = col.n3, pos = 2, cex = 1.5)
#text(4, 3, pos = 2, expression(paste(R^2, " = 0.37")), col = "red")
dev.off()

pdf("N_r_npin_all.pdf")
par(mar = c(5,5,1,1))
plot(log10(stoich$tn_r_mass_aerial)~log10(stoich$np_in), xlab = "log N:P in", ylab = "log N removed (kg/ha y)",
     cex = 1.2, pch = 16, col = rgb(122,122,122,max = 255,100), cex.lab =1.5)
#change.lm <- lm(log10(stoich$tp_r_mass_aerial[stoich$tp_r_mass_aerial>0&stoich$np_in])~log10(stoich$np_in[stoich$tp_r_mass_aerial>0]))
#abline(change.lm, col = "red", lwd = 2)
#abline(h=0, lty = 2)
abline(v = log10(44), col = col.n, lty = 2, lwd = 2)
abline(v = log10(110), col = col.p, lty = 2, lwd = 2)
#text(2.2, 2, "P-limited", col = col.p, pos = 4, cex = 1.5)
#text(1.45, 2, "N-limited", col = col.n3, pos = 2, cex = 1.5)
#text(4, 3, pos = 2, expression(paste(R^2, " = 0.37")), col = "red")
dev.off()

#########################################################
# Fig. Create gridded res time ~ depth boxes
#########################################################
n.mod <- function(depth, restime){
  r.out <- 1-exp(-Fit.N.np$par[1]*(restime/depth))
  return(r.out)
}

p.mod <- function(restime){
  r.out <- 1-(1/(1+(Fit.np.Rp$par[1]*restime^(1+Fit.np.Rp$par[2]))))
  return(r.out)
}
depth <- as.numeric(quantile(stoich$mean_depth, probs = c(.25,.5,.75)))
restime <- as.numeric(quantile(stoich$res_time, probs = c(.25,.5,.75)))
depth.max <- log10(max(stoich$mean_depth))
depth.min <- log10(min(stoich$mean_depth))
restime.max <- log10(max(stoich$res_time))
restime.min <- log10(min(stoich$res_time))

depth.1 <- median(stoich$mean_depth[stoich$mean_depth<depth[1]])
depth.2 <- median(stoich$mean_depth[stoich$mean_depth>=depth[1]&stoich$mean_depth<depth[2]])
depth.3 <- median(stoich$mean_depth[stoich$mean_depth>=depth[2]&stoich$mean_depth<depth[3]])
depth.4 <- median(stoich$mean_depth[stoich$mean_depth>=depth[3]])

restime.1 <- median(stoich$res_time[stoich$res_time<restime[1]])
restime.2 <- median(stoich$res_time[stoich$res_time>=restime[1]&stoich$res_time<restime[2]])
restime.3 <- median(stoich$res_time[stoich$res_time>=restime[2]&stoich$res_time<restime[3]])
restime.4 <- median(stoich$res_time[stoich$res_time>=restime[3]])

n.mod(depth, restime)
p.mod(restime)

n.mod(depth.3, restime.4)
p.mod(restime.4)
###############################################################
# Differential retention plotted over depth and residence time
# first plot with points
# second plot as a contour plot
###############################################################
pdf("Diff_ret_depth_restime.pdf")
par(mar=c(5,5,1,1))
plot(log10(stoich$res_time)~log10(stoich$mean_depth), col = "white",
     xlab = "log Mean Depth (m)", ylab = "log Residence Time (y)", cex.lab = 1.5, cex.axis = 1.2)
abline(v = log10(min(stoich$mean_depth)), lty = 2)
abline(v = log10(depth[1]), lty = 2)
abline(v = log10(depth[2]), lty = 2)
abline(v = log10(depth[3]), lty = 2)
abline(v = log10(max(stoich$mean_depth)), lty = 2)

abline(h = log10(min(stoich$res_time)), lty = 2)
abline(h = log10(restime[1]), lty = 2)
abline(h = log10(restime[2]), lty = 2)
abline(h = log10(restime[3]), lty = 2)
abline(h = log10(max(stoich$res_time)), lty = 2)
#n.mod(depth.2, restime.4)
#p.mod(restime.4)
rect(depth.min, restime.min, depth.max, log10(restime[2]), col = col.p, border = NA)
rect(depth.min, log10(restime[2]), log10(depth[1]), log10(restime[3]), col = rgb(122,122,122,max=255,122), border = NA)
rect(log10(depth[1]), log10(restime[2]), depth.max, log10(restime[3]), col =col.p, border = NA)
rect(depth.min, log10(restime[3]), log10(depth[2]), restime.max, col = col.n, border = NA)
rect(log10(depth[2]), log10(restime[3]), depth.max, restime.max, col = col.p, border = NA)
points(log10(stoich$res_time[stoich$R_diff> -0.2&stoich$R_diff<0.2])~log10(stoich$mean_depth[stoich$R_diff> -0.2&stoich$R_diff<0.2]),pch = 21, cex = 1.2, bg = rgb(200,200,200,max=255,200))
points(log10(stoich$res_time[stoich$R_diff< -0.2])~log10(stoich$mean_depth[stoich$R_diff< -0.2]),pch = 21, cex = 1.2, bg = col.p)

points(log10(stoich$res_time[stoich$R_diff>0.2])~log10(stoich$mean_depth[stoich$R_diff>0.2]),pch = 21, cex = 1.2, bg = col.n)
dev.off()

# contour plot of depth, res time, and differential retention
require(akima)
temp.m = interp(x = log10(stoich$mean_depth), y = log10(stoich$res_time), z = stoich$R_diff, linear = TRUE, 
                extrap = FALSE, duplicate = "mean")
pdf("Rdiff_depth_restime_contour.pdf")
par(mar=c(5,5,3,0),cex = 1.2, cex.lab = 1.7, cex.axis = 1.2,oma=c(0,1,0,0))
filled.contour(x = temp.m$x,
               y = temp.m$y, 
               z = temp.m$z,
               levels = c(-1.5,-1,-.5,0,0.5,1,1.5),
               col = c(rev(brewer.pal(6, name = "Spectral"))),
               #color.palette = colorRampPalette(c(rgb(5,48,97,max=255), rgb(255,255,255,max=255),rgb(103,0,31,max=255))), 
               xlab = "log Mean Depth (m)",
               ylab = "log Residence time (y)", 
               key.title = title(main = "Rn - Rp", cex.main = 1.1))
dev.off()

# contour plot with change in stoich as z axis
temp.m = interp(y = log10(stoich$res_time), x = log10(stoich$mean_depth), z = stoich$np_change, linear = TRUE, 
                extrap = FALSE, duplicate = "mean")
pdf("NPchange_depth_restime_contour.pdf")
par(mar=c(5,5,3,0),cex = 1.2, cex.lab = 1.7, cex.axis = 1.2,oma=c(0,1,0,0))
filled.contour(x = temp.m$x,
               y = temp.m$y, 
               z = temp.m$z,
               levels = c(-1.5,-1,-.5,0,0.5,1,1.5,2,2.5,3),
               col = c(rev(brewer.pal(11, name = "Spectral"))[2:11]),
               #color.palette = colorRampPalette(c(rgb(5,48,97,max=255), rgb(255,255,255,max=255),rgb(103,0,31,max=255))), 
               xlab = "log Residence Time (y)",
               ylab = "log Mean Depth (m)", 
               key.title = title(main = "NPout - NPin", cex.main = 1.1))
dev.off()

##############################################
# change in N:P in to out vs residence time
############################################
pdf("NPchange_restime_all.pdf")
par(mar=c(5,5,1,1))
plot(stoich$np_change~log10(stoich$res_time), xlab = "Residence Time (y)",
     ylab = "Change in Stoichiometry", cex = .8, pch = 16, col = "white", xaxt = "n")
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 

# add rectangles that show where doubling or halving of stoichiometry occurs
rect(-3.5, 0, 3, 0.3, col = rgb(200, 200, 200, max = 255, 122), border = NA)
rect(-3.5, 0, 3, -0.3, col = rgb(200, 200, 200, max = 255, 122), border = NA)
points(stoich$np_change~log10(stoich$res_time), cex = .8, pch = 16, col = rgb(122,122,122,max=250,122))
abline(h=0)
abline(h = median(stoich$np_change), col = "red", lty = 2, lwd = 2)
abline(h = quantile(stoich$np_change, 0.90), col = "red", lty = 2)
abline(h = quantile(stoich$np_change, 0.1), col = "red", lty =2)
dev.off()


#######################################################
# Figure showing how lakes changed the limiting nutrient
# from input to output
#######################################################

pdf("Change_limiting.pdf")
dot.vals <- c(19*(453/756), 19*(236/756), 19*(67/756))
dot.vals2 <- c(19*(306/756), 19*(330/756), 19*(119/756))
plot(c(15, 30, 45), c(8,8,8), xlim = c(0, 60), ylim = c(-2,11), xaxt = "n", yaxt = "n", col = "white",
     type = "n", axes = FALSE, ylab = "", xlab = "")
rect(7.5, -6, 22.5, 15, col = col.n, border = NA)
rect(37.5, -6, 52.5, 15, col = col.p, border = NA)
rect(22.5, -6, 37.5, 15, col = rgb(122,122,122,max=255,122), border = NA)
points(c(15, 30, 45), c(8,8,8), xlim = c(0, 40), ylim = c(0,10), cex = dot.vals, pch = 22, bg = "darkgray")
points(c(15, 30, 45), c(1,1,1), xlim = c(0, 40), ylim = c(0,10), cex = dot.vals2, pch = 22, bg = "darkgray")
arrows(15, 6.5, 15, 2.1, lwd = 40*(275/765), col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(15, 6.5, 30, 2.1,lwd = 40*(162/765),col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(15, 6.5, 45, 2.1, lwd = 40*(16/765),col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(30, 6.5, 30, 2.1,lwd = 40*(158/765),col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(30, 6.5, 15, 2.1,lwd = 40*(27/765),col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(30, 6.5, 45, 2.1, lwd = 40*(51/765),col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(45, 6.5, 45, 2.1, lwd = 40*(53/765),col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(45, 6.5, 30, 2.1,lwd = 40*(10/765),col = rgb(80,80,80,max=255,220), length = 0.1)
arrows(45, 6.5, 15, 2.1,lwd = 40*(4/765),col = rgb(80,80,80,max=255,220), length = 0.1)
text(15, 10.5,"60%", cex = 2)
text(30, 10.5,"31%", cex = 2)
text(45, 10.5,"9%", cex = 2)
text(15, -1.5,"40%", cex = 2)
text(30, -1.5,"44%", cex = 2)
text(45, -1.5,"16%", cex = 2)
dev.off()

x.start = log10(stoich$np_in)
x.end = log10(stoich$np_out)
y.start = 1
y.end = -1
p <- .bincode(log10(stoich$np_in), breaks = as.numeric(quantile(log10(stoich$np_in), seq(0,1,by=0.05), na.rm = TRUE)), right = FALSE, include.lowest = TRUE)
np_quart_in = as.numeric(tapply(log10(stoich$np_in), INDEX = c(p), median, na.rm = TRUE))
np_quart_out = as.numeric(tapply(log10(stoich$np_out), INDEX = c(p), median, na.rm = TRUE))

pdf("ChangeNP_lines_percentiles.pdf", height = 7, width = 10)
plot(x = 1, y = 2, xlim = c(-1, 4), ylim = c(-2,2), xaxt = "n", yaxt = "n", col = "white",
     type = "n", axes = FALSE, ylab = "", xlab = "")

rect(-1.1, -2, log10(44), 2, col = col.n, border = NA)
rect(log10(110), -2, 4, 2, col = col.p, border = NA)
rect(log10(44), -2, log10(110), 2, col = rgb(122,122,122,max=255,122), border = NA)
text(-1, 1.2, "Inlet Stoichiometry", pos = 4, cex = 1.5)
text(-1, -1.2, "Outlet Stoichiometry", pos = 4, cex = 1.5)
text(log10(44), 1.2, "N:P < 44", pos = 2, cex = 1.2)
text(log10(110), 1.2, "N:P > 110", pos = 4, cex = 1.2)
text(log10(44), 1.5, "N-limited", pos = 2, cex = 1.2)
text(log10(110), 1.5, "P-limited", pos = 4, cex = 1.2)
arrows(x0 = -1, y0 = -1.8, x1 = 3.8, y1 = -1.8, length = 0, lwd = 2)
arrows(x0 = log10(44), y0 = -1.85, x1 = log10(44), y1 = -1.75, length = 0, lwd = 2)
arrows(x0 = log10(110), y0 = -1.85, x1 = log10(110), y1 = -1.75, length = 0, lwd = 2)
arrows(x0 = log10(500), y0 = -1.85, x1 = log10(500), y1 = -1.75, length = 0, lwd = 2)
arrows(x0 = log10(1000), y0 = -1.85, x1 = log10(1000), y1 = -1.75, length = 0, lwd = 2)
arrows(x0 = log10(20), y0 = -1.85, x1 = log10(20), y1 = -1.75, length = 0, lwd = 2)
arrows(x0 = log10(5), y0 = -1.85, x1 = log10(5), y1 = -1.75, length = 0, lwd = 2)
text(x = log10(c(5,20,44,110,500,1000)), y = -1.65, labels = c("5", "20", "44", "110", "500", "1000"))
text(-1, -1.7, "N:P (moles)", pos = 4)
arrows(x0 = x.start, y0 = y.start, x1 = x.end, y1 = y.end, length = 0,
       col = rgb(250,250,250,max = 255, 122))
arrows(x0 = np_quart_in, y0 = y.start, x1 = np_quart_out, y1 = y.end, lwd=5, length = 0, col = rgb(10,10,10,max=255,122))
dev.off()

# calculate sedimentation coefficients for stoich.pos
stoich.pos <- stoich[stoich$Rn>0&stoich$Rp>0,]
stoich.pos$pcoef <- ((1/(1-stoich.pos$Rp))-1)/(stoich.pos$res_time)

dum.in <- c(1,2,5,7,8,10,12)
dum.r <- c(.5,1,4,4,5,9,11)
dum.e <- c()

####################################
# supplemental figs
####################################



