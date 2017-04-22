require(maps)
require(maptools)
require(rgdal)
require(rworldmap)
require(ggplot2)
data(wrld_simpl)

map(wrld_simpl)

dat.all$longitude[which(dat.all$longitude>0 & dat.all$country == "United States")] = dat.all$longitude[which(dat.all$longitude>0 & dat.all$country == "United States")]*-1
countries <- levels(as.factor(dat.all$country))[c(2:26,28)]
countries[c(27,28)] = c("Zambia", "Zimbabwe")
png("LitReview_locations.png", height = 800, width = 1200)
par(mar=c(1,1,1,1), oma = c(1,1,1,1), cex = 1.3)
plot(wrld_simpl, border = "gray48", lwd=1)
plot(wrld_simpl[which(wrld_simpl$NAME %in% countries), ], add = TRUE, col="gray90",border="gray48", lwd=1)
points(dat.all$longitude[!is.na(dat.all$Rn) & !is.na(dat.all$Rp)], dat.all$latitude[!is.na(dat.all$Rn) & !is.na(dat.all$Rp)], bg = adjustcolor("red", alpha = 0.5),col = adjustcolor("black", alpha = 0.5), cex = .7, pch = 21)
points(dat.all$longitude[is.na(dat.all$Rn) & !is.na(dat.all$Rp)], dat.all$latitude[is.na(dat.all$Rn) & !is.na(dat.all$Rp)], bg = col.p,col = adjustcolor("black", alpha = 0.5), cex = .7, pch = 21)
points(dat.all$longitude[!is.na(dat.all$Rn) & is.na(dat.all$Rp)], dat.all$latitude[!is.na(dat.all$Rn) & is.na(dat.all$Rp)], bg = col.n2, col = adjustcolor("black", alpha = 0.5),cex = .7, pch = 21)

dev.off()