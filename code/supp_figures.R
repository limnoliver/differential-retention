## S1
pdf("loading_restime.pdf", height = 4, width = 8)
par(mfrow=c(1,2), mar=c(2,5,1,1), oma=c(2,1,1,1))
plot(log10(stoich$tn_in_mass_areal)~log10(stoich$res_time), pch = 16, col = rgb(122,122,122,max=255,122), 
     xlab = "", ylab  = expression(paste("log N load (g ",m^-2," ",y^-1,")", sep = "")), cex = 1.2,
     cex.lab = 1.5, cex.axis = 1.2)
plot(log10(stoich$tp_in_mass_areal)~log10(stoich$res_time), pch = 16, col = rgb(122,122,122,max=255,122), 
     xlab = "", ylab  = expression(paste("log P load (g ",m^-2," ",y^-1,")", sep = "")), cex = 1.2,
     cex.lab = 1.5, cex.axis = 1.2)
mtext("log Residence time (y)", side = 1, outer = TRUE, cex = 1.4, line = .5)
dev.off()

##############################
## S2
##############################
pdf("Depth_v_Restime.pdf")
par(cex = 1, mar = c(5,5,1,1))

# create a figure that shows depth vs residence time
plot(log10(dat.all$res_time)~log10(dat.all$mean_depth), cex = 1.3,
     pch = 21, bg = rgb(200,200,200,alpha=200, max = 255),
     xlab = "Mean Depth (m)", ylab = "Residence Time", yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(2, labels = c("day", "wk", "mon", "yr", "10 yr", "100 yr"), 
     at = c(log10(1/365), log10(7/365), log10(30/365), 0, 1, 2), cex.axis=1.25)
axis(1, labels = c("1", "5", "10","20", "50", "100"), at = c(0,log10(5), 1, log10(20), log10(50), 2),
     cex.axis=1.25)
text(-.4,2.6, "y = -1.16x + 0.90", col = "red", cex = 1.2, pos=4)
text(-.4,2.2, expression(paste(R^2," = 0.22", sep = "")), col = "red", cex = 1.2, pos=4)

abline(h=log10(1/365), col="gray", lty=2)
# week
abline(h=log10(7/365), col="gray", lty=2)
# month
abline(h=log10(30/365), col="gray", lty=2)
# year
abline(h=0, col = "gray", lty = 2)
abline(h = 1, col = "gray", lty = 2)
abline(h = 2, col = "gray", lty = 2)

x = log10(dat.all$mean_depth[!is.na(dat.all$mean_depth)&!is.na(dat.all$res_time)])
y = log10(dat.all$res_time[!is.na(dat.all$mean_depth)&!is.na(dat.all$res_time)])
mod <- lm(y ~ x)
newx <- seq(min(x), max(x), length.out = 100)
preds <- predict(mod, newdata = data.frame(x=newx), interval = "prediction")
abline(mod, lwd = 2)
polygon(c(rev(newx), newx), c(rev(preds[,3]), preds[,2]), col = rgb(200,200,200, alpha = 100, max = 255), border = NA)

dev.off()

###################
# first, take out eutrophic lakes

d.meso <- dat.p.real[dat.p.real$trophic != "eutrophic" & dat.p.real$tp_r_mass_aerial>0, ]
d.meso$res_time_cat <- "b"
d.meso$res_time_cat[d.meso$res_time < cutoffs[1]] = "a"
d.meso$res_time_cat[d.meso$res_time < cutoffs[3] & d.meso$res_time >= cutoffs[2]] = "c"
d.meso$res_time_cat[d.meso$res_time >= cutoffs[3]] = "d"
d.meso$res_time_cat <- as.factor(d.meso$res_time_cat)

d.eutro <- dat.p.real[dat.p.real$trophic == "eutrophic" & dat.p.real$tp_r_mass_aerial>0, ]
d.eutro$res_time_cat <- "b"
d.eutro$res_time_cat[d.eutro$res_time < cutoffs[1]] = "a"
d.eutro$res_time_cat[d.eutro$res_time < cutoffs[3] & d.eutro$res_time >= cutoffs[2]] = "c"
d.eutro$res_time_cat[d.eutro$res_time >= cutoffs[3]] = "d"
d.eutro$res_time_cat <- as.factor(d.eutro$res_time_cat)

d.trophic <- dat.p.real[dat.p.real$tp_r_mass_aerial>0, ]
d.trophic$res_time_cat <- "b"
d.trophic$res_time_cat[d.trophic$res_time < cutoffs[1]] = "a"
d.trophic$res_time_cat[d.trophic$res_time < cutoffs[3] & d.trophic$res_time >= cutoffs[2]] = "c"
d.trophic$res_time_cat[d.trophic$res_time >= cutoffs[3]] = "d"
d.trophic$res_time_cat <- as.factor(d.trophic$res_time_cat)

test.meso <- aov(log10(d.meso$tp_r_mass_aerial)~log10(d.meso$tp_in_mass_aerial)*d.meso$res_time_cat)
test.meso <- lm(log10(d.meso$tp_r_mass_aerial)~log10(d.meso$tp_in_mass_aerial)+d.meso$res_time_cat)
test.meso.0 <- lm(log10(d.meso$tp_r_mass_aerial)~log10(d.meso$tp_in_mass_aerial))

test.eutro <- aov(log10(d.eutro$tp_r_mass_aerial)~log10(d.eutro$tp_in_mass_aerial)*d.eutro$res_time_cat)
test.eutro <- lm(log10(d.eutro$tp_r_mass_aerial)~log10(d.eutro$tp_in_mass_aerial)*d.eutro$res_time_cat)
test.eutro.0 <- lm(log10(d.eutro$tp_r_mass_aerial)~log10(d.eutro$tp_in_mass_aerial))

test.trophic <- aov(log10(d.trophic$tp_r_mass_aerial)~log10(d.trophic$tp_in_mass_aerial)*d.trophic$trophic)
test.trophic <- lm(log10(d.trophic$tp_r_mass_aerial)~log10(d.trophic$tp_in_mass_aerial)*d.trophic$trophic)
test.trophic <- lm(log10(d.trophic$tp_r_mass_aerial)~d.trophic$trophic/log10(d.trophic$tp_in_mass_aerial)-1)

d.trophic <- dat.p.real[dat.p.real$tp_r_mass_aerial>0 & dat.p.real$res_time>1.2,]
test.trophic <- aov(log10(d.trophic$tp_r_mass_aerial)~log10(d.trophic$tp_in_mass_aerial)*d.trophic$trophic)
summary(lm(log10(d.trophic$tp_r_mass_aerial)~log10(d.trophic$tp_in_mass_aerial)+d.trophic$trophic))
plot(log10(oligo$tp_r_mass_aerial)~log10(oligo$tp_in_mass_aerial))
mod.o <- lm(log10(oligo$tp_r_mass_aerial)~log10(oligo$tp_in_mass_aerial))
