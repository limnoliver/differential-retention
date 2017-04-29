# statistical tests

dat.mod <- dat.p.real.comp[,c(32,33)]
dat.mod <- log10(dat.mod)
dat.mod$res_time_cat <- dat.p.real.comp$res_time_cat
dat.mod <- dat.mod[dat.mod$tp_out_mass_areal != -Inf,]

aov.pin.pout <- aov(tp_out_mass_areal~tp_in_mass_areal*res_time_cat, data = dat.mod)
lm.pin.pout <- lm(tp_out_mass_areal~res_time_cat/tp_in_mass_areal-1, data = dat.mod)
lm.pin.pout.1 <- lm(tp_out_mass_areal~res_time_cat*tp_in_mass_areal, data = dat.mod)

lm.p.1 <- lm(tp_out_mass_areal~tp_in_mass_areal, data = dat.mod[dat.mod$res_time_cat=="a",])
lm.p.2 <- lm(tp_out_mass_areal~tp_in_mass_areal, data = dat.mod[dat.mod$res_time_cat=="b",])
lm.p.3 <- lm(tp_out_mass_areal~tp_in_mass_areal, data = dat.mod[dat.mod$res_time_cat=="c",])
lm.p.4 <- lm(tp_out_mass_areal~tp_in_mass_areal, data = dat.mod[dat.mod$res_time_cat=="d",])
lm.p.all <-  lm(tp_out_mass_areal~tp_in_mass_areal, data = dat.mod)

dat.mod.n <- dat.n.real.comp[,c(29,30)]
dat.mod.n <- log10(dat.mod.n)
dat.mod.n$res_time_cat <- dat.n.real.comp$res_time_cat
dat.mod.n <- dat.mod.n[dat.mod.n$tn_out_mass_areal != -Inf,]


aov.nin.nout <- aov(tn_out_mass_areal~tn_in_mass_areal*res_time_cat, data = dat.mod.n)
aov.nin.nout.i <- aov(tn_out_mass_areal~tn_in_mass_areal + res_time_cat, data = dat.mod.n)
plot(tn_out_mass_areal~tn_in_mass_areal, data = dat.mod.n)
abline(0,1,col = "red")
abline(lm.n.1, col = "lightblue", lwd = 2)
abline(lm.n.2, col = "blue", lwd = 2)
abline(lm.n.3, col = "darkblue", lwd = 2)
abline(lm.n.4, col = "black", lwd = 2)

lm.n.1 <- lm(tn_r_mass_areal~tn_in_mass_areal, data = dat.mod.n[dat.mod.n$res_time_cat=="a",])
lm.n.2 <- lm(tn_r_mass_areal~tn_in_mass_areal, data = dat.mod.n[dat.mod.n$res_time_cat=="b",])
lm.n.3 <- lm(tn_r_mass_areal~tn_in_mass_areal, data = dat.mod.n[dat.mod.n$res_time_cat=="mhigh",])
lm.n.4 <- lm(tn_r_mass_areal~tn_in_mass_areal, data = dat.mod.n[dat.mod.n$res_time_cat=="high",])

lm.n <- lm(tn_out_mass_areal~res_time_cat/tn_in_mass_areal-1, data = dat.mod.n)
lm.n.1 <- lm(tn_out_mass_areal~res_time_cat*tn_in_mass_areal, data = dat.mod.n)

lm.n.all <- lm(tn_r_mass_areal~tn_in_mass_areal, data = dat.mod.n)
lm.nin.nout.low <- lm(tn_r_mass_areal~tn_in_mass_areal, data = dat.mod.n[dat.mod.n$res_time_cat == "low", ])
plot(tn_r_mass_areal~tn_in_mass_areal, data = dat.mod.n, pch = 16,
     col = rgb(122,122,122,max=255,122))
abline(0,1, col = "red", lwd = 2, lty =2)
abline(lm.n$coefficients[1], lm.n$coefficients[2], col = "steelblue", lwd = 3)
abline(lm.n$coefficients[1]+lm.n$coefficients[3], lm.n$coefficients[2], col = "steelblue3", lwd = 3)
abline(lm.n$coefficients[1]+lm.n$coefficients[4], lm.n$coefficients[2], col = "steelblue4", lwd = 3)
abline(lm.n$coefficients[1]+lm.n$coefficients[5], lm.n$coefficients[2], col = "royalblue4", lwd = 3)
 

dat.mod.np <- dat.np.real.comp[,c(35,36)]
dat.mod.np <- log10(dat.mod.np)
dat.mod.np$res_time_cat <- dat.np.real.comp$res_time_cat

aov.npin.npout <- aov(np_out~np_in*res_time_cat, data = dat.mod.np)
lm.np <- lm(np_out~np_in*res_time_cat, data = dat.mod.np)
lm.np <- lm(np_out~res_time_cat/np_in-1, data = dat.mod.np)

lm.np.d <- lm(np_out~np_in, data = dat.mod.np[dat.mod.np$res_time_cat == "d",])
## same thing but with retention
stoich.pos <- stoich[!is.na(stoich$np_in) & !is.na(stoich$np_r)& stoich$np_r != Inf & stoich$np_r != -Inf & stoich$np_r>0, ]
dat.mod.np <- stoich.pos[,c(35,42,12)]
dat.mod.np <- log10(dat.mod.np)

cutoffs <- quantile(stoich$res_time, c(0.25, .5, .75))
dat.mod.np$res_time_cat <- "b"
dat.mod.np$res_time_cat[dat.mod.np$res_time < log10(cutoffs[1])] = "a"
dat.mod.np$res_time_cat[dat.mod.np$res_time < log10(cutoffs[3]) & dat.mod.np$res_time >= log10(cutoffs[2])] = "c"
dat.mod.np$res_time_cat[dat.mod.np$res_time >= log10(cutoffs[3])] = "d"
dat.mod.np$res_time_cat <- as.factor(dat.mod.np$res_time_cat)

aov.npin.npout <- aov(np_r~np_in*res_time_cat, data = dat.mod.np)
lm.np <- lm(np_r ~ res_time_cat + np_in, data = dat.mod.np)
lm.np <- lm(np_r ~ res_time_cat + np_in -1, data = dat.mod.np)
lm.np <- lm(np_r ~  np_in, data = dat.mod.np)


library(interplot)
interplot(m = lm.nin.nout, var1 = 'tn_in_mass_areal', var2 = 'res_time', hist = TRUE) +
  xlab("Residence Time (y)") +
  ylab("Estimated Slope of Removal ~ Load")+
  theme_bw()+
  theme(axis.title=element_text(size=14), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


dat.p.real.comp$trophic <- "mesotrophic"
dat.p.real.comp$trophic[dat.p.real.comp$tp_out_conc < .010] <- "oligotrophic"
dat.p.real.comp$trophic[dat.p.real.comp$tp_out_conc > .030] <- "eutrophic"

dat.mod$trophic <- dat.p.real.comp$trophic

summary(lm(tp_out_mass_areal ~ tp_in_mass_areal*trophic, dat = dat.mod))
