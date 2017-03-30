# regression tree for predicting differential retention

library(rpart)
library(randomForest)
diff.tree <- rpart(R_diff ~ log10(res_time)+ log10(mean_depth) + log10(surface_area) + log10(np_in), data = stoich)
change.tree <- rpart(np_change ~ log10(res_time)+ log10(mean_depth) + log10(surface_area) + log10(np_in), data = stoich)
prp(diff.tree)

diff.forest <- randomForest(stoich$R_diff ~ log10(stoich$res_time)+ log10(stoich$mean_depth) + log10(stoich$surface_area) + log10(stoich$np_in) + log10(stoich$tp_in_mass_aerial) + log10(stoich$tn_in_mass_aerial),
                            na.remove = TRUE, importance = TRUE)
varImpPlot(diff.forest)
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

