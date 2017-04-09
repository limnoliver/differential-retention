# regression tree for predicting differential retention
library(rpart.plot)
library(rpart)
library(randomForest)
install.packages("ggRandomForests")
library(ggRandomForests)
diff.tree <- rpart(R_diff ~ log10(res_time)+ log10(mean_depth) + log10(surface_area) + log10(np_in), data = stoich)
change.tree <- rpart(np_change ~ log10(res_time)+ log10(mean_depth) + log10(surface_area) + log10(np_in), data = stoich)
prp(change.tree)

dat.forest.p <- stoich[, c(26,8,9,12,13,33,43)]
dat.forest.n <- stoich[, c(27,8,9,12,17,33,42)]
dat.forest.diff <- stoich[,c(52,8,9,12,13,43,17,42,33)]
dat.forest.change <- stoich[,c(40,8,9,12,13,43,17,42,33)]


dat.forest.p[, c(2:7)] <- log10(dat.forest.p[, c(2:7)])
dat.forest.n[, c(2:7)] <- log10(dat.forest.n[, c(2:7)])
dat.forest.diff[,c(2:9)] <- log10(dat.forest.diff[,c(2:9)])
dat.forest.change[,c(2:9)] <- log10(dat.forest.change[,c(2:9)])
dat.forest.change.out <- dat.forest.change[-influential, ]

# rpart tree for Rn, Rp, diff
Rp.tree <- rpart(Rp ~ log10(res_time) + log10(mean_depth)+ log10(surface_area) + log10(np_in) + log10(tp_in_mass_aerial) + log10(tp_in_conc), data = stoich)
Rn.tree <- rpart(Rn ~ log10(res_time) + log10(mean_depth)+ log10(surface_area) + log10(np_in) + log10(tn_in_mass_aerial) + log10(tn_in_conc), data = stoich)
change.tree <- rpart(np_change ~ ., data = dat.forest.change)

diff.forest <- randomForest(stoich$R_diff ~ log10(stoich$res_time)+ log10(stoich$mean_depth) + log10(stoich$surface_area) + log10(stoich$np_in) + log10(stoich$tp_in_mass_aerial) + log10(stoich$tn_in_mass_aerial),
                            na.remove = TRUE, importance = TRUE)
Rp.forest <- randomForest(Rp~., data = dat.forest.p,
                          na.rm = TRUE, importance = TRUE)
Rn.forest <- randomForest(Rn~., data = dat.forest.n,
                          na.rm = TRUE, importance = TRUE)
Rdiff.forest <- randomForest(R_diff~., data = dat.forest.diff,
                          na.rm = TRUE, importance = TRUE)
Rchange.forest <- randomForest(np_change~., data = dat.forest.change,
                             na.rm = TRUE, importance = TRUE)
Rchange.forest.out <- randomForest(np_change~., data = dat.forest.change.out,
                                   na.rm = TRUE, importance = TRUE)

# calculate top variables
rfsrc_Rp <- rfsrc(Rp ~ res_time + mean_depth+ surface_area + np_in + tp_in_mass_aerial + tp_in_conc, data = dat.forest.p)
rfsrc_Rn <- rfsrc(Rn ~ res_time + mean_depth+ surface_area + np_in + tn_in_mass_aerial + tn_in_conc, data = dat.forest.n)
rfsrc_Rdiff <- rfsrc(R_diff ~ ., data = dat.forest.diff)
rfsrc_change <- rfsrc(np_change ~ ., data = dat.forest.change, importance = TRUE)

gg_v_Rp <- gg_variable(rfsrc_Rp)
gg_v_Rn <- gg_variable(rfsrc_Rn)
gg_v_Rdiff <- gg_variable(rfsrc_Rdiff)
gg_v_change <- gg_variable(rfsrc_change)

gg_md_Rp <- gg_minimal_depth(rfsrc_Rp)
gg_md_Rn <- gg_minimal_depth(rfsrc_Rn)
gg_md_Rdiff <- gg_minimal_depth(rfsrc_Rdiff)
gg_md_change <- gg_minimal_depth(rfsrc_change)
plot(gg_minimal_vimp(rfsrc_change))

xvar.Rp <- gg_md_Rp$topvars
xvar.Rn <- gg_md_Rn$topvars
xvar.Rdiff <- gg_md_Rdiff$topvars
xvar.change <- gg_md_change$topvars

plot(gg_v, xvar = xvar, panel = TRUE,  alpha = .4) +
  labs(y=st.labs["medv"], x = "")
plot(gg_vimp(rfsrc_Rp))

plot.variable(rfsrc_Rp, xvar = gg_md$topvars, partial = TRUE, 
              sorted = FALSE, smooth.lines = TRUE)
data(partial_Rp)
gg_p <- gg_partial(partial_Rp)
plot(gg_p, xvar = xvar, panel = TRUE)

varImpPlot(diff.forest)
mar
print(diff.forest)

stoich$R_diff_cat <- "p"
stoich$R_diff_cat[stoich$R_diff>0] <- "n"
stoich$R_diff_cat[stoich$R_diff == 0] <- "b"
stoich$R_diff_cat <- as.factor(stoich$R_diff_cat)
diff.forest <- randomForest(stoich$R_diff_cat ~ log10(stoich$res_time)+ log10(stoich$mean_depth) + log10(stoich$surface_area) + log10(stoich$np_in) + log10(stoich$tp_in_mass_aerial) + log10(stoich$tn_in_mass_aerial),
                            na.remove = TRUE, importance = TRUE, sampsize = c(16,16,16))
varImpPlot(diff.forest)

plot(log10(epa$drainage_area_km2), log10(epa$TP_load_kgyr.1))
abline(p.load, col = "red")
n.load <- lm(log10(epa$TN_load_kgyr.1)~log10(epa$drainage_area_km2))
p.load <- lm(log10(epa$TP_load_kgyr.1)~log10(epa$drainage_area_km2))

test <- nrow(stoich[stoich$np_in<44&stoich$np_out>110, ])
test2 <- nrow(stoich[stoich$np_in>110&stoich$np_out<44, ])
test3 <- nrow(stoich[stoich$np_in>110&stoich$np_out>44&stoich$np_out<110, ])
test4 <- nrow(stoich[stoich$np_in<44&stoich$np_out>44&stoich$np_out<110, ])
test5 <- nrow(stoich[stoich$np_in<44&stoich$np_out<44, ])
test6 <- nrow(stoich[stoich$np_in>44&stoich$np_in<110&stoich$np_out>44&stoich$np_out<110, ])
test7 <- nrow(stoich[stoich$np_in>110&stoich$np_out>110, ])
test8 <- nrow(stoich[stoich$np_in>44&stoich$np_in<110&stoich$np_out>110, ])
test9 <- nrow(stoich[stoich$np_in>44&stoich$np_in<110&stoich$np_out<44, ])

stoich$change <- "no change"
stoich$change[stoich$np_change <- ]
pdf("Rp_partial_plots.pdf")
par(mfrow=c(2,2))
partialPlot(x = Rp.forest, pred.data = dat.forest.p, res_time, main = "", xlab = "log Residence Time (y)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rp.forest, pred.data = dat.forest.p, tp_in_conc, main = "", xlab = "log TP in (ug/L)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rp.forest, pred.data = dat.forest.p, np_in, main = "", xlab = "log N:P in",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rp.forest, pred.data = dat.forest.p, mean_depth, main = "", xlab = "log Mean Depth (m)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
dev.off()

pdf("Rn_partial_plots.pdf")
par(mfrow=c(2,2))
partialPlot(x = Rn.forest, pred.data = dat.forest.n, res_time, main = "", xlab = "log Residence Time (y)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rn.forest, pred.data = dat.forest.n, tn_in_conc, main = "", xlab = "log TN in (ug/L)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rn.forest, pred.data = dat.forest.n, tn_in_mass_aerial, main = "", xlab = "log TN in (kg/ha y)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rn.forest, pred.data = dat.forest.n, np_in, main = "", xlab = "log N:P in",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
dev.off()

pdf("Rdiff_partial_plots.pdf")
par(mfrow=c(2,2))
partialPlot(x = Rdiff.forest, pred.data = dat.forest.diff, np_in, main = "", xlab = "log N:P in",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rdiff.forest, pred.data = dat.forest.diff, tp_in_mass_aerial, main = "", xlab = "log TP in (kg/ha y)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rdiff.forest, pred.data = dat.forest.diff, surface_area, main = "", xlab = "log Surface Area (ha)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rdiff.forest, pred.data = dat.forest.diff, mean_depth, main = "", xlab = "log Mean Depth (m)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
dev.off()

pdf("Change_partial_plots.pdf")
par(mfrow=c(2,2))
partialPlot(x = Rchange.forest, pred.data = dat.forest.change, np_in, main = "", xlab = "log N:P in",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rchange.forest, pred.data = dat.forest.change, tn_in_conc, main = "", xlab = "log TN in (ug/L)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rchange.forest, pred.data = dat.forest.change, tn_in_mass_aerial, main = "", xlab = "log TN in (kg/ha y)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rchange.forest, pred.data = dat.forest.change, surface_area, main = "", xlab = "log Surface Area (ha)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
dev.off()