# LAGOS integration into differential retention

# step 1: find HUC8's with lots of lakes that have contrasting LULC
library(LAGOS)
dt <- lagos_load("1.087.1", "rds") 

names(dt$epi.nutr)

lagos <- merge(dt$epi.nutr, dt$locus, by = "lagoslakeid", all.x = TRUE)

# calculate TN from tkn and no2no3
lagos$tn_calculated = lagos$tkn + lagos$no2no3
lagos$tn_combined = lagos$tn
lagos$tn_combined[which(is.na(lagos$tn_combined))] = lagos$tn_calculated[which(is.na(lagos$tn_combined))]

# first do exercise by keeping all modern LAGOS data

lagos.mod <- lagos[lagos$sampleyear>1989, ]

# change TP values that are 0 to lowest observed, non-zero TP

lagos.mod$tp[lagos.mod$tp == 0] = 0.01
# create lake-wide averages of TN and TP
lagos.mod$np <- (lagos.mod$tn_combined/14)/(lagos.mod$tp/31)
med.np <- tapply(lagos.mod$np, INDEX = list(lagos.mod$lagoslakeid), FUN = median, na.rm = TRUE)
med.np <- as.data.frame(med.np)
med.np$lagoslakeid <- rownames(med.np)

# step 2: calculate residence time from wsa:la

lakes <- merge(med.np, dt$locus, by = "lagoslakeid", all.x = TRUE)
lakes <- merge(lakes, dt$iws, by = "lagoslakeid", all.x = TRUE)

# get connectivity type in as to get rid of isolated lakes
type <- dt$lakes.geo[,c(1,31)]
lakes <- merge(lakes, type, by ="lagoslakeid", all.x = TRUE)

lakes <- lakes[lakes$lakeconnection == "DR_LakeStream"|lakes$lakeconnection == "DR_LakeStream", ]

# get land use so we can find "similar lakes"
lakes <- merge(lakes, dt$iws.lulc, by = "lagoslakeid", all.x = TRUE)

# create list of hucs that have high ag
lakes$ag <- lakes$iws_nlcd2001_pct_81+lakes$iws_nlcd2001_pct_82
dt$hu8.lulc$ag <- dt$hu8.lulc$hu8_nlcd2001_pct_81 + dt$hu8.lulc$hu8_nlcd2001_pct_82
ag.hucs <- dt$hu8.lulc$hu8_zoneid[which(dt$hu8.lulc$ag>75)]

lakes.ag <- lakes[which(lakes$hu8_zoneid %in% ag.hucs), ]
plot(log10(lakes.ag$med.np)~log10(lakes.ag$res_time_est))

plot(log10(lakes.ag$med.np)~log10(lakes.ag$res_time_est))
hucs <- summary(lakes$hu8_zoneid)
top.hucs <- names(hucs)[c(1:20)]


par(mfrow=c(2,2))
for (i in 1:length(top.hucs)){
  plot(log10(med.np) ~ log10(res_time_est), dat = lakes[lakes$hu8_zoneid == top.hucs[i],],
       xlab = "", ylab = "")
  #legend("bottomright", legend = top.hucs[i], text.col = "red")
}

# step 3: show how lakes with longer residence time diverge from those
# with short residence time in the same region, where they should be getting similar
# input N:P
lakes$res_time_est <- lakes$iws_ha/lakes$lake_area_ha

plot(log10(lakes$med.np) ~ log10(lakes$res_time_est))
# step 4: show how lakes with contrasting input ration (from Sparrow output
# put also using short res time lakes as baseline) 