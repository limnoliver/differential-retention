# This script creates figures for differential retention project
library(RColorBrewer)
# Set colors for all plots
col.n1 <- rgb(.955, .7975, .27625,.7)
col.n2 <- rgb(.94,.73,0.035,.7)
col.n3 <- rgb(.705,.5475,.02625,.7)
col.n4 <- rgb(.47, .365, .0175, .7)
col.p = rgb(.07,.57,.45,0.7)
# gradient of blues for depth where appropriate
new.cols = c(brewer.pal(n = 9, name = "Blues"), "black")


# set model parameters
Vf = 6.68

##################################################################
# Figure 1: N and P retention according to Vollenweider & Harrison
##################################################################
png("R_restime.png", height = 600, width = 800)
# plot N and P lines together 
par(mar=c(5,5,1,1))
curve(1-(1/(1+(1.12*(x^.47)))), 0.001,1000,log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(0, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)

curve(1-(exp((-Vf*x)/1)), .001, 1000, 
      log = "x", ylab="N Retention", xlab = "Residence Time (y)", 
      col = col.n1, add = TRUE, lwd = 4)
curve(1-(exp((-Vf*x)/10)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = col.n2, lwd = 4, add = TRUE)
curve(1-(exp((-Vf*x)/20)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = col.n3, lwd = 4, add = TRUE)
curve(1-(exp((-Vf*x)/50)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = col.n4, lwd = 4, add = TRUE)
abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v=10, col = "gray", lty = 2)
abline(v=100, col = "gray", lty = 2)
legend("topleft", legend = c("Nitrogen (1, 10, 20, 50m)", "Phosphorus"), 
       col = c(col.n2, col.p), lty = 1, lwd = 3, cex = 1.7)
dev.off()

########################################################
# Figure 2: Differential retention according to V & H
#######################################################
png("RnRp_restime.png", height = 600, width = 800)
par(mar=c(5,5,1,1))
curve((1-(exp((-Vf*x)/1)))/(1-(1/(1+(1.12*(x^.47))))), 0.001,1000,log = "x",
      ylab = "Rn:Rp", xlab = "Residence Time (y)", 
      col = new.cols[3], ylim = c(0, 3), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)
curve((1-(exp((-Vf*x)/10)))/(1-(1/(1+(1.12*(x^.47))))), 0.001,1000,
      log = "x",add=TRUE, col = new.cols[5], lwd = 4)
curve((1-(exp((-Vf*x)/20)))/(1-(1/(1+(1.12*(x^.47))))), 0.001,1000,
      log = "x",add=TRUE, col = new.cols[7], lwd = 4)
curve((1-(exp((-Vf*x)/50)))/(1-(1/(1+(1.12*(x^.47))))), 0.001,1000,
      log = "x",add=TRUE, col = new.cols[9], lwd = 4)
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
legend("topleft", legend = c("1m", "10m", "20m", "50m"), 
       col = new.cols[c(3,5,7,9)], lty = 1, lwd = 3, cex = 1.7)
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

# Figure 4: Mass balance lakes - depth vs residence time

# Figure 5: Mass balance lakes - histograms of N and P retention

# Figure 5.123: histograms of broken up by N retention > P retention,
# histogram of size, input concentrations, etc.

# Figure 6: N vs P retention

# Figure 7: Differential retention with deciles for depth and retention time + 
# real N and P retention

# Figure 8: Differential retention as % change in TN:TP vs rank or res time
# - predicted from models

# Figure 9: Differential retention as % change in TN:TP vs rank or res time
# - observed

# Figure 10: Same as Figure 9, but with color = rank or res time and x = start concentration

# figure 11: Same as figure 9, but broken up into 3 - each panel represents low, medium
# high res time/rank, and x = start ratio or start P concentration (to test Finlay hypothesis?)

# Figure 12: calculate Vf and sedimentation coefficient and summarize by lake type. 
# show how sed coef or Vf changes with nutrient input concentration
