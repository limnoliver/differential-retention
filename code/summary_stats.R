############################################
# Calculate summary statistics for Table 2
############################################
vars <- c("volume", "surface_area", "mean_depth", "res_time", "tn_in_mass_areal","tn_out_mass_areal", 
          "Rn","tp_in_mass_areal","tp_out_mass_areal","Rp")
sum.stats <- function(input.dat, percentile) {
  d <- input.dat
  d$RnRpdiff <- d$Rn-d$Rp
  summ <- c()
  for (i in 1:length(vars)){
    summ[i] <- as.numeric(quantile(d[,vars[i]], probs = percentile, na.rm = TRUE))
  }
  return(as.data.frame(t(summ)))
}
r1 <- sum.stats(dat.np, percentile = 0.5)
r2 <- sum.stats(dat.np.real, percentile = 0.5)
r3 <- sum.stats(dat.np.real.comp, percentile = 0.5)

r4 <- sum.stats(dat.n, percentile = .5)
r5 <- sum.stats(dat.n.real, percentile = 0.5)
r6 <- sum.stats(dat.n.rea.comp, percentile = 0.5)

r7 <- sum.stats(dat.p, percentile = .5)
r8 <- sum.stats(dat.p.real, percentile = 0.5)
r9 <- sum.stats(dat.p.real.comp, percentile = 0.5)

dat.sum <- rbind(r4, r5, r6, r7, r8, r9, r1, r2, r3)
names(dat.sum) <- vars

write.csv(dat.sum, "table2.csv")

