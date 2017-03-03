#import differential retention data and 
#get it all in same columns/units

setwd("data")

###################
# epa data
###################
epa <- read.csv("EPA data extraction.csv", header = TRUE, na.strings = c("", "NA"))

# add volume column
epa$volume <- epa$surface_area_km2*(epa$mean_depth_m/1000)

# add country column
epa$country = "United States"
epa$tp_in_conc = ""
epa$tn_in_conc = ""
epa$Rp_source = ""
epa$Rn_source = ""
epa$latitude = ""
epa$longitude = ""

# convert "impoundment" to "reservoir
levels(epa$type)[1:4] = c("reservoir", "lake", "lake", "lake")

# get rid of all lakes with residence time <1 day

#epa <- epa[epa$retention_time_years>(1/365), ]
#################################################
# rearrange columns, create data frame to work from
dat <- epa[,c(1,2,3,4,22,27,28,6,7,21,8,9,23,14,10,16,24,15,11,17,25,18,26,19, 20)]
names(dat) <- c("source", "waterbody_name", "lake_type", 
                "state", "country", "latitude", "longitude",
                "surface_area", "mean_depth", "volume", "Q", "res_time",
                "tp_in_conc", "tp_in_mass", "tp_out_conc", "tp_out_mass",
                "tn_in_conc", "tn_in_mass", "tn_out_conc", "tn_out_mass",
                "Rp_source", "Rp_calculated", "Rn_source", "Rn_calculated", "notes")
#######################
# harrison et al data
#######################
har <- read.csv("Harrison et al data_with P.csv", header = TRUE,
                na.strings = c("", "NA", "ND"))

har$Rn_calculated = har$N_retention
har$Rp_calculated = har$P_retention
har$Rn_source = ""
har$Rp_source = ""

## need to rework harrison
#convert mass units
for (i in 1:nrow(har)){
  if (har$mass_units[i] %in% "g m-2 y-1" | har$mass_units[i] %in% "g m-2 yr-1"){
    har$tn_out_mass[i] = (har$N_out_mass[i]*(har$Area_km2[i]*1000000))/1000
    har$tp_out_mass[i] = (har$P_out_mass[i]*(har$Area_km2[i]*1000000))/1000
    har$tn_in_mass[i] = (har$N_in_mass[i]*(har$Area_km2[i]*1000000))/1000
    har$tp_in_mass[i] = (har$P_in_mass[i]*(har$Area_km2[i]*1000000))/1000
    
      } else if (har$mass_units[i] %in% "kt y-1"){
        har$tn_out_mass[i] = har$N_out_mass[i]*1000
        har$tp_out_mass[i] = har$P_out_mass[i]*1000
        har$tn_in_mass[i] = har$N_in_mass[i]*1000
        har$tp_in_mass[i] = har$P_in_mass[i]*1000
      } else if (har$mass_units[i] %in% "mg m-2 y-1"){
        har$tn_out_mass[i] = (har$N_out_mass[i]*(har$Area_km2[i]*1000000))/1000000
        har$tp_out_mass[i] = (har$P_out_mass[i]*(har$Area_km2[i]*1000000))/1000000
        har$tn_in_mass[i] = (har$N_in_mass[i]*(har$Area_km2[i]*1000000))/1000000
        har$tp_in_mass[i] = (har$P_in_mass[i]*(har$Area_km2[i]*1000000))/1000000
      } else if (har$mass_units[i] %in% "mg N m-2 d-1"){
        har$tn_out_mass[i] = har$N_out_mass[i]*har$Area_km2[i]*365
        har$tp_out_mass[i] = har$P_out_mass[i]*har$Area_km2[i]*365
        har$tn_in_mass[i] = har$N_in_mass[i]*har$Area_km2[i]*365
        har$tp_in_mass[i] = har$P_in_mass[i]*har$Area_km2[i]*365       
      } else {
        har$tn_out_mass[i] = har$N_out_mass[i]
        har$tp_out_mass[i] = har$P_out_mass[i]
        har$tn_in_mass[i] = har$N_in_mass[i]
        har$tp_in_mass[i] = har$P_in_mass[i]
      }
}

# convert Q from km3 y-1 to m3 s-1                       
        
har$Q <- har$Q_km3.yr*(1/(60*60*24*365))*10^9       
 
har <- har[,c(1,2,4,5,3,6,7,10,8,11,35,9,19,18,21,20,14,13,16,14,30,28,29,27,26)] 
names(har) <- names(dat)

#har <- har[har$res_time>(1/365), ]
#################################
# Maavara data
maav <- read.csv("Maavara et al 2015 data_with N.csv", header = TRUE, 
                 na.strings = c("", "NA"))
maav <- maav[c(1:156), ]
maav$lake_type = "reservoir"
maav$state = ""
maav$country = ""
maav$Rp_calculated = ""
maav$Rn_calculated = ""
maav <- maav[,c(34,1,36,37,38,2,3,8,9,10,12,11,15,17,16,18,19,21,20,22,23,39,24,40,35)]
names(maav) <- names(dat)

#update units to match rest of data

maav$Q = maav$Q*(10^9/(60*60*24*365))     
maav$tn_in_conc = maav$tn_in_conc*(14/1000)
maav$tn_out_conc = maav$tn_out_conc*(14/1000)
maav$tp_in_conc = maav$tp_in_conc*(30.97/1000)
maav$tp_out_conc = maav$tp_out_conc*(30.97/1000)
maav$tn_in_mass = maav$tn_in_mass*(14/1000)
maav$tn_out_mass = maav$tn_out_mass*(14/1000)
maav$tp_in_mass = maav$tp_in_mass*(30.97/1000)
maav$tp_out_mass = maav$tp_out_mass*(30.97/1000)

########################
# Brett & Benjamin data
########################

brett <- read.csv("Brett_with_N.csv", header = TRUE, na.strings = c("", NA))
brett <- brett[,c(2:28, 30)]
brett[,1] = "brett2008"
brett <- brett[,c(1:9, 11:28)] 

names(brett)[c(1:20)] <- c("source", "waterbody_name", "state", 
                  "surface_area", "mean_depth", "Q", "res_time", 
                  "tp_out_conc", "tp_in_mass", 
                   "volume", "tp_in_aerial", 
                   "tpout_tpin", "Rp_calculated", 
                  "Rp_predidcted", "tp_in_conc", "k", "10overz", 
                  "flush_rate", "qs", "qs2")
brett$country = ""
brett$country = brett$state
levels(brett$country)[c(2,3,4,6,8,11,12,14:21,23:30,33:36)] = c("Canada", 
                                                                 rep("United States", 2),
                                                                 "Canada",
                                                                 rep("United States", 4),
                                                                 "Canada",
                                                                 rep("United States", 4), 
                                                              "Canada",
                                                              "United States",
                                                              "Canada",
                                                              rep("United States", 2),
                                                              "Canada",
                                                              "United States",
                                                              "Canada",
                                                              "United States",
                                                              "Canada",
                                                              "United States",
                                                              rep("United States", 3))

                                                              
                                                              
                                                              

levels(brett$state) = c(NA, "British Colombia", "California", "Connecticut",
                        NA, "Ontario", NA, "Florida", NA, NA, NA, "Iowa",
                        NA, "Maine", "Manitoba", "Massachusetts", "Michigan",
                        "Minnesota", "North Carolina", "New Brunswick", "New Hampshire",
                        NA, "Nova Scotia", "New York", "Ohio", "Ontario", "Oregon", "Quebec",
                        "Rhode Island", "Saskatchewan", NA, NA, "Tennessee", "Vermont",
                        "Washington", "Wisconsin")
brett$lake_type = ""
brett$latitude = ""
brett$longitude = ""
brett$Rn_calculated = ""
brett$notes = ""
brett <- brett[,c(1,2,29,3,28,30,31,4,5,10,6,7,15,9,8,22,26,23,27,21,25,13,24,32,33)]
# change units of concentration from mgperm3 to mgperL
brett$tp_in_conc = brett$tp_in_conc/1000
brett$tp_out_conc = brett$tp_out_conc/1000
brett$tn_in_conc = brett$tn_in_conc/1000
brett$tn_out_conc = brett$tn_out_conc/1000

# change units of volume from 10^6m3 to km3
brett$volume <- brett$volume/1000

names(brett) <- names(dat)



#################################
# Donald et al 2015 data
#################################

donald <- read.csv("Donald et al 2015 data.csv", header = TRUE, na.strings = c("NA", ""))
donald$tn_in_conc = ""
donald$tp_in_conc = ""
donald$tn_calculated = ""
donald$tp_calculated = ""
donald$source = "donald2015"
donald$state = ""
donald$notes = ""

donald <- donald[,c(27,1,3,28,2,4,5,8,9,10,12,11,24,19,13,20,23,21,14,22,15,26,17,25,29)]
donald$TP_out_ugperL <- donald$TP_out_ugperL/1000
donald$TN_out_ugperL <- donald$TN_out_ugperL/1000
donald$tp_in_mass <- donald$tp_in_mass*1000
donald$tp_out_mass <- donald$tp_out_mass*1000
donald$tn_in_mass <- donald$tn_in_mass*1000
donald$tn_out_mass <- donald$tn_out_mass*1000
names(donald) <- names(dat)

##############################
# my compiled data
##############################
 
lit <- read.csv("NP_retention_litreview.csv", header = TRUE, na.strings = c("", "NA", "ND"))

#############################
# get all data together
#############################

dat.all <- rbind(dat, brett, har, donald, maav)
dat.all$Rp <- as.numeric(dat.all$Rp_source)
dat.all$Rp[is.na(dat.all$Rp)] <- as.numeric(dat.all$Rp_calculated[is.na(dat.all$Rp)])
dat.all$Rn <- as.numeric(dat.all$Rn_source)
dat.all$Rn[is.na(dat.all$Rn)] <- as.numeric(dat.all$Rn_calculated[is.na(dat.all$Rn)])

#################################
# calculate all input/output variables
###################################
# calculate volume for all lakes
dat.all$volume[which(is.na(dat.all$volume))] = (dat.all$mean_depth[which(is.na(dat.all$volume))]/1000)*dat.all$surface_area[which(is.na(dat.all$volume))]
#calculate Q for all lakes
dat.all$Q[which(is.na(dat.all$Q))] = (dat.all$volume[which(is.na(dat.all$Q))]/dat.all$res_time[which(is.na(dat.all$Q))])*((60*60*24*365)/10^9)
#for lakes that have q = 0, recalculate from volume and residence time to get non-zero answer
dat.all$Q[which(dat.all$Q==0)] = (dat.all$volume[which(dat.all$Q==0)]/dat.all$res_time[which(dat.all$Q==0)])*((60*60*24*365)/10^9)

# calculate in/out nutrients

# tp_out_conc
dat.all$tp_out_conc[is.na(dat.all$tp_out_conc)] = (as.numeric(dat.all$tp_out_mass[is.na(dat.all$tp_out_conc)])/dat.all$Q[is.na(dat.all$tp_out_conc)])*(1/31536)

#tp_in_conc
dat.all$tp_in_conc <- as.numeric(dat.all$tp_in_conc)
dat.all$tp_in_conc[is.na(dat.all$tp_in_conc)] = (as.numeric(dat.all$tp_in_mass[is.na(dat.all$tp_in_conc)])/dat.all$Q[is.na(dat.all$tp_in_conc)])*(1/31536)

# tp mass in
dat.all$tp_in_mass <- as.numeric(dat.all$tp_in_mass)
dat.all$tp_in_mass[is.na(dat.all$tp_in_mass)] <- dat.all$tp_in_conc[is.na(dat.all$tp_in_mass)]*dat.all$Q[is.na(dat.all$tp_in_mass)]*31536

#tp mass out
dat.all$tp_out_mass <- as.numeric(dat.all$tp_out_mass)
dat.all$tp_out_mass[is.na(dat.all$tp_out_mass)] <- dat.all$tp_out_conc[is.na(dat.all$tp_out_mass)]*dat.all$Q[is.na(dat.all$tp_out_mass)]*31536

# tn out conc
dat.all$tn_out_conc[is.na(dat.all$tn_out_conc)] = (as.numeric(dat.all$tn_out_mass[is.na(dat.all$tn_out_conc)])/dat.all$Q[is.na(dat.all$tn_out_conc)])*(1/31536)

#tn_in_conc
dat.all$tn_in_conc <- as.numeric(dat.all$tn_in_conc)
dat.all$tn_in_conc[is.na(dat.all$tn_in_conc)] = (as.numeric(dat.all$tn_in_mass[is.na(dat.all$tn_in_conc)])/dat.all$Q[is.na(dat.all$tn_in_conc)])*(1/31536)

# tn mass in
dat.all$tn_in_mass <- as.numeric(dat.all$tn_in_mass)
dat.all$tn_in_mass[is.na(dat.all$tn_in_mass)] <- dat.all$tn_in_conc[is.na(dat.all$tn_in_mass)]*dat.all$Q[is.na(dat.all$tn_in_mass)]*31536

#tn mass out
dat.all$tn_out_mass <- as.numeric(dat.all$tn_out_mass)
dat.all$tn_out_mass[is.na(dat.all$tn_out_mass)] <- dat.all$tn_out_conc[is.na(dat.all$tn_out_mass)]*dat.all$Q[is.na(dat.all$tn_out_mass)]*31536

# in lagos, max TN = 20.57 max TP = 1.22
summary(dat.all$tn_out_conc)
dat.all[which(dat.all$tn_out_conc>20), ]

summary(dat.all$tp_out_conc)
dat.all[which(dat.all$tp_out_conc>1.2), ]


# create a data frame where lakes have retention estimates for both N and P

dat.np <- dat.all[!is.na(dat.all$Rn)&!is.na(dat.all$Rp),]
dat.np$relret <- dat.np$Rn/dat.np$Rp

dat.np.pos <- dat.np[dat.np$Rn>0 & dat.np$Rp>0,]
dat.np.real <- dat.np[dat.np$Rn>-1 & dat.np$Rp>-1, ]
# rank by depth an residence time
plot(log10(as.numeric(dat.np.pos$res_time))~log10(as.numeric(dat.np.pos$mean_depth)))


# first rank by residence time
dat.np.pos$rank_restime <- rank(dat.np.pos$res_time)
dat.np.pos$rank_depth <- rank(dat.np.pos$mean_depth)
dat.np.pos$rank_sum = dat.np.pos$rank_restime + dat.np.pos$rank_depth

dat.np.real$rank_restime <- rank(dat.np.real$res_time)
dat.np.real$rank_depth <- rank(dat.np.real$mean_depth)
dat.np.real$rank_sum <- dat.np.real$rank_restime + dat.np.real$rank_depth

q1 <- mean(dat.np.pos$relret[dat.np.pos$rank_sum<303])
q2 <- mean(dat.np.pos$relret[dat.np.pos$rank_sum>=303 & dat.np.pos$rank_sum])
q4 <- mean(dat.np.pos$relret[dat.np.pos$rank_sum>647.2])

ngp <- dat.np.pos[dat.np.pos$relret>1, ]
pgn <- dat.np.pos[dat.np.pos$relret<1, ]

dat.np.pos$h <- dat.np.pos$mean_depth/as.numeric(dat.np.pos$res_time)
plot(dat.np.pos$Rn~log10(dat.np.pos$h))

#
dat.np.pos
png("Rn_Rp_extreme.png", height = 800, width = 800)
par(mar=c(5,5,1,1), cex = 1.4)
plot(dat.np.real$Rn~dat.np.real$Rp, pch = 21, 
     bg = rgb(222,222,222,alpha = 200, max = 255), cex = 2, cex.lab = 1.8,
     xlab = "Rp", ylab = "Rn", cex.axis = 1.3)

abline(0,1,col = "blue", lwd = 2)
points(dat.np.real$Rn[dat.np.real$rank_sum<250],
       dat.np.real$Rp[dat.np.real$rank_sum<250], pch = 21, cex = 2,
       bg = rgb(178,34,34,max=255,alpha=200))
points(dat.np.real$Rn[dat.np.real$rank_sum>1260],
       dat.np.real$Rp[dat.np.real$rank_sum>1260], bg = rgb(85,107,47,alpha = 200, max = 255), 
       pch = 21, cex = 2)
dev.off()



hist(epa$Rp[epa$Rp>0])
length(which(epa$Rp<0))
hist(epa)

plot(dat.np.real$Rn~log10(as.numeric(dat.np.real$res_time)), pch = 21, 
     bg = rgb(222,222,222,alpha = 200, max = 255), cex = 2, ylim = c(-1,1))

hist(epa$Rn[epa$Rn>0])
hist(epa$Rn)
length(which(epa$Rn<0))

plot(epa$Rn[epa$Rn>0]~epa$Rp[epa$Rn>0])

##############################################
## figure - Vollenweider vs observed retention
##############################################
pdf("Rpobs_Veq.pdf", height = 6, width = 8)
par(mar=c(5,5,1,1))
#plot retention vs residence time then add curve of Brett & Benjamin
curve(1-(1/(1+(1.12*(x^.47)))), 0.001,1000,log = "x",
      ylab = "Rp", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-.1, 1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(1, labels = c("1 d", "1 wk", "1 mo", "1 yr", "10 yr", "100 yr"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)

points(dat.all$Rp~dat.all$res_time, xlog = TRUE, pch = 21, cex = 1.5,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(1/(1+(1.12*(x^.47)))), 0.001,1000,log = "x",add = TRUE,
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
dev.off()

##############################################
## figure - Harrison vs observed retention
##############################################
pdf("Rnobs_Heq.pdf", height = 6, width = 8)
par(mar=c(5,5,1,1))
#plot retention vs residence time then add curve of Brett & Benjamin
curve(1-(exp((-9.92*x)/9.6)), 0.001,1000,log = "x",
      ylab = "Rn", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-.1, 1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(1, labels = c("1 d", "1 wk", "1 mo", "1 yr", "10 yr", "100 yr"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)

points(dat.all$Rn~dat.all$res_time, xlog = TRUE, pch = 21, cex = 1.5,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(exp((-9.92*x)/9.6)), 0.001,1000,log = "x",add = TRUE,
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
dev.off()

# fit our own model
dat.fit <- data.frame(x = dat.all$res_time[!is.na(dat.all$res_time)&!is.na(dat.all$Rp)], 
                      y = dat.all$Rp[!is.na(dat.all$res_time)&!is.na(dat.all$Rp)])
min.RSS <- function(dat.fit, par) {
  with(dat.fit, sum(((1-(1/(1 + (par[1]*x^par[2])))) - y)^2))
}

result <- optim(par = c(1.12,0.47), min.RSS, data = dat.fit)
################
# n mods with different Vf and depth
##############################
png("RN_Vf_depth_restime.png", height = 800, width = 1000)
par(mar=c(5,5,1,1), cex = 1.4)
curve(1-(exp((-6.83*x)/1)), .001, 1000, 
      log = "x", ylab="N Retention", xlab = "Residence Time (y)",  xaxt = "n",
      col = "deepskyblue", lwd = 4, cex.lab = 1.8, cex.axis = 1.3)
curve(1-(exp((-13.66*x)/1)), .001, 1000, 
      log = "x", xaxt = "n", 
      col = "coral", lwd = 4, cex.lab = 1.8, cex.axis = 1.3, add = TRUE)
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)
curve(1-(exp((-13.66*x)/50)), .001, 1000, 
      log = "x", xaxt = "n",
      col = "coral3", lwd = 4, cex.lab = 1.8, cex.axis = 1.3, add = TRUE)
curve(1-(exp((-6.83*x)/50)), .001, 1000, 
      log = "x", xaxt = "n",
      col = "deepskyblue3", lwd = 4, cex.lab = 1.8, cex.axis = 1.3, add = TRUE)
text(100,.2,"Deep lakes retain less N", col = "deepskyblue3", cex = 1.3)
text(4/365,.8,"Reservoirs retain more N", col = "coral3", cex = 1.3)

abline(v=1/365, col="gray", lty=2)

abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v=10, col = "gray", lty = 2)
abline(v=100, col = "gray", lty = 2)
dev.off()
###############################################
## Figure 1
###############################################
new.cols = c(brewer.pal(n = 9, name = "Blues"), "black")

png("R_restime.png", height = 600, width = 800)
# plot N and P lines together 
par(mar=c(5,5,1,1))
curve(1-(1/(1+(1.12*(x^.47)))), 0.001,1000,log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = "red", ylim = c(0, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
axis(1, labels = c("1 day", "1 week", "1 month", "1 year", "10 years", "100 years"), 
     at = c(1/365, 7/365, 30/365, 1, 10, 100), cex.axis=1.3)

curve(1-(exp((-9.92*x)/1)), .001, 1000, 
      log = "x", ylab="N Retention", xlab = "Residence Time (y)", 
      col = new.cols[3], add = TRUE, lwd = 4)
curve(1-(exp((-9.92*x)/10)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = new.cols[5], lwd = 4, add = TRUE)
curve(1-(exp((-9.92*x)/20)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = new.cols[7], lwd = 4, add = TRUE)
curve(1-(exp((-9.92*x)/50)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = new.cols[9], lwd = 4, add = TRUE)
abline(v=1/365, col="gray", lty=2)
# week
abline(v=7/365, col="gray", lty=2)
# month
abline(v=30/365, col="gray", lty=2)
# year
abline(v=1, col = "gray", lty = 2)
abline(v=10, col = "gray", lty = 2)
abline(v=100, col = "gray", lty = 2)
text(80, .7, "Phosphorus", cex = 1.3, col = "red")
text(5/365, .7, "Nitrogen", cex = 1.3, col = new.cols[5])
text(c(1.7/365,3.5/365,8/365,18/365), .65, c("1m", "10m", "20m", "50m"), cex = 1.3, col = new.cols[c(3,5,7,9)])
dev.off()

# calculate percent change in N:P
x = c(seq(0.001,1,0.002), seq(2,1000,1))
x = 10^as.numeric(tapply(log10(dat.all$res_time), INDEX = c(dat.all$z), median, na.rm = TRUE))
n_outin = exp((-9.92*x)/10)
p_outin = 1/(1+(1.12*(x^.47)))

np_perc_change = 1-(n_outin/p_outin)
plot(np_perc_change~log(x))

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
text(x = 20, y = -30, "Remove more N \nDecrease N:P")
text(x = 20, y = 30, "Remove more P \nIncrease N:P")

dev.off()

#
pdf("Depth_v_Restime.pdf")
par(cex = 1, mar = c(5,5,1,1))

# create a figure that shows depth vs residence time
plot(log10(dat.all$res_time)~log10(dat.all$mean_depth), cex = 2,
     pch = 21, bg = rgb(200,200,200,alpha=200, max = 255),
     xlab = "Mean Depth (m)", ylab = "Residence Time", yaxt = "n",cex.lab = 2, cex.axis = 1.3)
axis(2, labels = c("day", "wk", "mon", "yr", "10 yr", "100 yr"), 
     at = c(log10(1/365), log10(7/365), log10(30/365), 0, 1, 2), cex.axis=1.25)
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
hist(log10(dat.all$res_time))
breaks <- hist(log10(dat.all$mean_depth), breaks = 5)
z <- .bincode(dat.all$mean_depth,breaks = c(0,2,3,5,10,20,50,315), right = FALSE)

png("restime_depth.png", height = 600, width = 800, pointsize = 14)
par(cex = 1.2, mar = c(4,4,1,2))
boxplot(log10(dat.all$res_time)~z, xaxt = "n", yaxt = "n", cex.lab = 1.5)
axis(1, labels = c("<2", "2-3", "3-5", "5-10", "10-20", "20-50", ">50"), 
     at = c(1,2,3,4,5,6,7), main = "Mean Depth (m)", cex.axis = 1.3, mgp = c(1.5,.7,0))
title(xlab = "Mean Depth (m)", ylab = "Residence Time (yr)", mgp = c(2.5,.7,0), cex.lab = 2)
axis(2, labels = c(0.001,0.01, 0.1, 1, 10, 100,1000), 
     at = c(-3,-2,-1,0,1,2,3), cex.axis = 1.3)
text(x = c(1,2,3,4,5,6,7), 
     y= c(1,1.5,1.75,1.5,1.55,1.9,3),
     c("12%", "10%", "17%", "29%",  "20%", "8%", "3%"),col = "red")
text(x = c(1,2,3,4,5,6,7), y = as.numeric(tapply(log10(dat.all$res_time), INDEX = c(dat.all$z), median, na.rm = TRUE)), 
     c("21 d", "50 d", "4 mo", "5 mo", "10 mo", "1.1 yr", "6.6 yr"), col = "blue", pos = 3)


dev.off()

par(cex = 1.2, mar = c(5,5,1,1))

epa$dasa <- epa$drainage_area_km2/epa$surface_area_km2
png()

#create a relationship between drainage area to residence time

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

# use hydrolakes database to look at global distribution of depth vs residence time

library(rgdal)
hl = readOGR(dsn ="/Users/Samantha/Dropbox/Differential Retention/Data/HydroLAKES_points_v10_shp", layer = "HydroLAKES_points_v10")
