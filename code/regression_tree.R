# regression tree for predicting differential retention
library(rpart.plot)
library(rpart)
library(randomForest)
install.packages("ggRandomForests")
library(ggRandomForests)
diff.tree <- rpart(R_diff ~ log10(res_time)+ log10(mean_depth) + log10(surface_area) + log10(np_in), data = stoich)
change.tree <- rpart(np_change ~ log10(res_time)+ log10(mean_depth) + log10(surface_area) + log10(np_in), data = stoich)
prp(Rp.tree)

Rp.tree <- rpart(Rp ~ log10(res_time) + log10(mean_depth)+ log10(surface_area) + log10(np_in) + log10(tp_in_mass_aerial) + log10(tp_in_conc), data = stoich)

diff.forest <- randomForest(stoich$R_diff ~ log10(stoich$res_time)+ log10(stoich$mean_depth) + log10(stoich$surface_area) + log10(stoich$np_in) + log10(stoich$tp_in_mass_aerial) + log10(stoich$tn_in_mass_aerial),
                            na.remove = TRUE, importance = TRUE)
Rp.forest <- randomForest(Rp~., data = dat.forest,
                          na.rm = TRUE, importance = TRUE)
pdf("Rp_partial_plots.pdf")
par(mfrow=c(2,2))
partialPlot(x = Rp.forest, pred.data = dat.forest, res_time, main = "", xlab = "log Residence Time (y)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rp.forest, pred.data = dat.forest, tp_in_conc, main = "", xlab = "log TP in (ug/L)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rp.forest, pred.data = dat.forest, np_in, main = "", xlab = "log N:P in",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
partialPlot(x = Rp.forest, pred.data = dat.forest, mean_depth, main = "", xlab = "log Mean Depth (m)",
            ylab = "", cex.lab = 1.5, cex.axis = 1.2)
dev.off()
dat.forest <- stoich[, c(26,8,9,12,13,33,43)]
dat.forest[, c(2:7)] <- log10(dat.forest[, c(2:7)]

rfsrc_Rp <- rfsrc(Rp ~ res_time + mean_depth+ surface_area + np_in + tp_in_mass_aerial + tp_in_conc, data = dat.forest)
gg_v <- gg_variable(rfsrc_Rp)
gg_md <- gg_minimal_depth(rfsrc_Rp)
xvar <- gg_md$topvars
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

