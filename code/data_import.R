#import differential retention data and 
#get it all in same columns/units

setwd("data")

# get NLA lake data to show population estimate of depth and 
# residence time
# residence time data from Renee Brooks, weights from EPA NLA online
# repository

library(data.table)

nla.wgts <- fread('https://www.epa.gov/sites/production/files/2014-01/nla2007_sampledlakeinformation_20091113.csv')
names(nla.wgts)
nla.wgts<- as.data.frame(nla.wgts)
nla.wgts <- nla.wgts[,c(1,31)]
nla.wgts <- unique(nla.wgts)
nla.res <- read.csv('Brooks_NLA_data.csv', header = TRUE, skip = 1)

nla.wgts <-data.table(nla.wgts, key = "SITE_ID")
nla.res <-data.table(nla.res, key = "SITE_ID")

nla <- merge(nla.res, nla.wgts)

# get global depth/res time estimates
library(rgdal)
#read in shape file
setwd("C:/Users/Samantha/Dropbox/Differential Retention/Data/HydroLAKES_points_v10_shp")
lakes = readOGR(dsn = ".", layer = "HydroLAKES_points_v10")

##########################
# Finlay data
#########################

finlay <-read.csv("data/Finlay_Pextraction.csv", header = TRUE, skip = 1, na.strings = c("", "NA"))

# exclude status = 0 (already in database), and = 3 (needs more work)
finlay <- finlay[finlay$status == 1, ]

#change units
# convert mass in to kg y-1

finlay$tn_in_mass <- finlay$Nload_gm2y*finlay$Lake_area*10^3
finlay$tn_r_mass <- finlay$Nretention_gm2y*finlay$Lake_area*10^3
finlay$tn_out_mass <- finlay$tn_in_mass - finlay$tn_r_mass

for (i in 1:nrow(finlay)){
  if (finlay$p_mass_units[i] %in% "g m2 y") {
    
    finlay$tp_out_mass[i] = finlay$tp_out_mass[i]*finlay$Lake_area[i]*10^3
    finlay$tp_in_mass[i] = finlay$tp_in_mass[i]*finlay$Lake_area[i]*10^3
    
    } else if (finlay$p_mass_units[i] %in% "g m2 d"){
      finlay$tp_out_mass[i] = finlay$tp_out_mass[i]*finlay$Lake_area[i]*1000*365
      finlay$tp_in_mass[i] = finlay$tp_in_mass[i]*finlay$Lake_area[i]*1000*365
    
    } else if (finlay$p_mass_units[i] %in% "mg m2 d"){
      finlay$tp_out_mass[i] = finlay$tp_out_mass[i]*finlay$Lake_area[i]*365
      finlay$tp_in_mass[i] = finlay$tp_in_mass[i]*finlay$Lake_area[i]*365
    
    } else if (finlay$p_mass_units[i] %in% "mmol m2 y"){
      finlay$tp_out_mass[i] = finlay$tp_out_mass[i]*finlay$Lake_area[i]*14
      finlay$tp_in_mass[i] = finlay$tp_in_mass[i]*finlay$Lake_area[i]*14
      
    } else if (finlay$p_mass_units[i] %in% "kg d") {
      finlay$tp_out_mass[i] = finlay$tp_out_mass[i]*365
      finlay$tp_in_mass[i] = finlay$tp_in_mass[i]*365
   
    } else if (finlay$p_mass_units[i] %in% "kt y") {
      finlay$tp_out_mass[i] = finlay$tp_out_mass[i]*10^6
      finlay$tp_in_mass[i] = finlay$tp_in_mass[i]*10^6
       
    } else if (finlay$p_mass_units[i] %in% "t y" | finlay$p_mass_units[i] %in% "T y") {
      finlay$tp_out_mass[i] = finlay$tp_out_mass[i]*1000
      finlay$tp_in_mass[i] = finlay$tp_in_mass[i]*1000
    }
}

finlay$tp_in_conc = finlay$tp_in_conc/1000  
finlay$tp_out_conc = finlay$tp_out_conc/1000  

finlay$volume <- ""
finlay$Q <- ""
finlay$tn_in_conc <- ""
finlay$tn_out_conc <- ""
finlay$Rp_calculated <- ""
finlay$Rn_calculated <- ""


finlay <- finlay[,c(4,3,5,9,6,7,8,11,22,66,67,17,35,34,37,36,68,63,69,65,38,70,27,71,1)]
names(finlay) <- names(dat)

###################
# epa data
###################
epa <- read.csv("data/EPA data extraction.csv", header = TRUE, na.strings = c("", "NA"))

# add volume column
epa$volume <- epa$surface_area_km2*(epa$mean_depth_m/1000)
epa$name <- toupper(epa$lake)
epa$state.u <- toupper(epa$state_country)

lake.coords <- read.csv("data/NES_lakecoordsid.csv", header = TRUE)
lake.coords <- lake.coords[,c(2,4,7,11,12)]
epa <- merge(epa, lake.coords, by = c("name", "state.u"), all.x = TRUE)

# add country column
epa$country = "United States"
epa$tp_in_conc = ""
epa$tn_in_conc = ""
epa$Rp_source = ""
epa$Rn_source = ""
epa$latitude = epa$Lat
epa$longitude = epa$Long

# convert "impoundment" to "reservoir
levels(epa$type)[1:4] = c("reservoir", "lake", "lake", "lake")

#################################################
# rearrange columns, create data frame to work from
dat <- epa[,c(3,1,5,6,27,32,33,8,9,23,10,11,28,16,12,18,29,17,13,19,30,20,31,21,22)]
names(dat) <- c("source", "waterbody_name", "lake_type", 
                "state", "country", "latitude", "longitude",
                "surface_area", "mean_depth", "volume", "Q", "res_time",
                "tp_in_conc", "tp_in_mass", "tp_out_conc", "tp_out_mass",
                "tn_in_conc", "tn_in_mass", "tn_out_conc", "tn_out_mass",
                "Rp_source", "Rp_calculated", "Rn_source", "Rn_calculated", "notes")
#######################
# harrison et al data
#######################
har <- read.csv("data/Harrison et al data_with P.csv", header = TRUE,
                na.strings = c("", "NA", "ND"))
har <- har[,c(1:26)]
har$Rn_calculated = har$N_retention
har$Rp_calculated = har$P_retention
har$Rn_source = ""
har$Rp_source = ""

#convert mass units
for (i in 1:nrow(har)){
  if (har$mass_units[i] %in% "g m-2 y-1" | har$mass_units[i] %in% "g m-2 yr-1"){
    har$tn_out_mass[i] = (har$N_out_mass[i]*(har$Area_km2[i]*1000000))/1000
    har$tp_out_mass[i] = (har$P_out_mass[i]*(har$Area_km2[i]*1000000))/1000
    har$tn_in_mass[i] = (har$N_in_mass[i]*(har$Area_km2[i]*1000000))/1000
    har$tp_in_mass[i] = (har$P_in_mass[i]*(har$Area_km2[i]*1000000))/1000
    
      } else if (har$mass_units[i] %in% "kt y-1"){
        har$tn_out_mass[i] = har$N_out_mass[i]*1000000
        har$tp_out_mass[i] = har$P_out_mass[i]*1000000
        har$tn_in_mass[i] = har$N_in_mass[i]*1000000
        har$tp_in_mass[i] = har$P_in_mass[i]*1000000
        
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
        
      } else if (har$mass_units[i] %in% "mmol m2 d"){
        har$tn_out_mass[i] = har$N_out_mass[i]*har$Area_km2[i]*365*14
        har$tp_out_mass[i] = har$P_out_mass[i]*har$Area_km2[i]*365*14
        har$tn_in_mass[i] = har$N_in_mass[i]*har$Area_km2[i]*365*14
        har$tp_in_mass[i] = har$P_in_mass[i]*har$Area_km2[i]*365*14
     
      } else if (har$mass_units[i] %in% "t y-1"){
        har$tn_out_mass[i] = har$N_out_mass[i]*1000
        har$tp_out_mass[i] = har$P_out_mass[i]*1000
        har$tn_in_mass[i] = har$N_in_mass[i]*1000
        har$tp_in_mass[i] = har$P_in_mass[i]*1000

      } else {
        har$tn_out_mass[i] = har$N_out_mass[i]
        har$tp_out_mass[i] = har$P_out_mass[i]
        har$tn_in_mass[i] = har$N_in_mass[i]
        har$tp_in_mass[i] = har$P_in_mass[i]
      }
}

# convert Q from km3 y-1 to m3 s-1                       
        
har$Q <- har$Q_km3.yr*(1/(60*60*24*365))*10^9       
 
har <- har[,c(1,2,4,5,3,6,7,10,8,11,35,9,19,34,21,32,14,33,16,31,30,28,29,27,26)] 
names(har) <- names(dat)

#har <- har[har$res_time>(1/365), ]
#################################
# Maavara data
maav <- read.csv("data/Maavara et al 2015 data_with N.csv", header = TRUE, 
                 na.strings = c("", "NA"))
maav$lake_type = "reservoir"
maav$Rp_calculated = ""
maav$Rn_calculated = ""
maav <- maav[,c(35,1,37,4,5,2,3,9,10,11,13,12,16,18,17,19,20,22,21,23,24,38,25,39,36)]
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

brett <- read.csv("data/Brett_with_N.csv", header = TRUE, na.strings = c("", NA))

# fill in missing P loading data, so as to override from Rydin
brett$tp_in_mass[is.na(brett$tp_in_mass)] <- brett$Loading_constant_kgy[is.na(brett$tp_in_mass)]
brett$Rn_calculated = ""
brett$notes = ""
brett <- brett[,c(1,3,4,5,6,7,8,9,10,15,11,12,20,29,13,27,32,28,33,26,31,18,30,34,35)]

names(brett) <- names(dat)
# change units of volume from 10^6m3 to km3
brett$volume <- brett$volume/1000
# change units of concentration from mgperm3 to mgperL
brett$tp_in_conc = brett$tp_in_conc/1000
brett$tp_out_conc = brett$tp_out_conc/1000
brett$tn_in_conc = brett$tn_in_conc/1000
brett$tn_out_conc = brett$tn_out_conc/1000


#################################
# Donald et al 2015 data
#################################

donald <- read.csv("data/Donald et al 2015 data.csv", header = TRUE, na.strings = c("NA", ""))
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
 
lit <- read.csv("data/NP_retention_litreview.csv", header = TRUE, na.strings = c("", "NA", "ND"))
names(lit)[1] <- "paper_id"
meta <- read.csv("data/literature_metadata.csv", header = TRUE)
meta$source <- with(meta, paste(first_author, year))
meta <- meta[,c(1,17)]
lit <- merge(lit, meta, by = "paper_id", all.x = TRUE)

#convert units
# discharge to m3 s-1
for (i in 1:nrow(lit)){
  if (lit$discharge_units[i] %in% "CubicMetersPerDay") {
    lit$Q[i] = lit$discharge[i]*(24*60*60) 
  } else if (lit$discharge_units[i] %in% "CubicMetersPerYear") {
    lit$Q[i] = lit$discharge[i]*(365*24*60*60)
  } else if (lit$discharge_units[i] %in% "1000GigaLiters" | lit$discharge_units[i] %in% "CubicKilometersPerYear") {
    lit$Q[i] = lit$discharge[i]*((365*24*60*60)/1000000000)
  } else {
    lit$Q[i] = lit$discharge[i]
  }
}

# mass to kilograms per year
for (i in 1:nrow(lit)){
  if (lit$p_mass_units[i] %in% "GramsPerMeterSquaredPerYear") {
    lit$tp_in_mass[i] = (lit$p_in_mass[i] * (lit$lake_area_km2[i]*1000000))/1000
    lit$tp_out_mass[i] = (lit$p_out_mass[i] * (lit$lake_area_km2[i]*1000000))/1000
    lit$tp_r_mass[i] = (lit$p_retained_mass[i] * (lit$lake_area_km2[i]*1000000))/1000
    
  } else if (lit$p_mass_units[i] %in% "KilotonnesPerYear") {
    lit$tp_in_mass[i] = lit$p_in_mass[i]*1000000
    lit$tp_out_mass[i] = lit$p_out_mass[i]*1000000
    lit$tp_r_mass[i] = lit$p_retained_mass[i]*1000000
    
  } else if (lit$p_mass_units[i] %in% "TonnesPerYear" | lit$p_mass_units[i] %in% "TonsPerYear") {
    lit$tp_in_mass[i] = lit$p_in_mass[i]*1000
    lit$tp_out_mass[i] = lit$p_out_mass[i]*1000
    lit$tp_r_mass[i] = lit$p_retained_mass[i]*1000
    
  } else {
    lit$tp_in_mass[i] = lit$p_in_mass[i]
    lit$tp_out_mass[i] = lit$p_out_mass[i]
    lit$tp_r_mass[i] = lit$p_retained_mass[i]
    
  }  
}

for (i in 1:nrow(lit)){
  if (lit$n_mass_units[i] %in% "GramsPerMeterSquaredPerYear") {
    lit$tn_in_mass[i] = (lit$n_in_mass[i] * (lit$lake_area_km2[i]*1000000))/1000
    lit$tn_out_mass[i] = (lit$n_out_mass[i] * (lit$lake_area_km2[i]*1000000))/1000
    lit$tn_r_mass[i] = (lit$n_retained_mass[i] * (lit$lake_area_km2[i]*1000000))/1000
    
  } else if (lit$n_mass_units[i] %in% "KilotonnesPerYear") {
    lit$tn_in_mass[i] = lit$n_in_mass[i]*1000000
    lit$tn_out_mass[i] = lit$n_out_mass[i]*1000000
    lit$tn_r_mass[i] = lit$n_retained_mass[i]*1000000
    
  } else if (lit$n_mass_units[i] %in% "TonnesPerYear" | lit$n_mass_units[i] %in% "TonsPerYear") {
    lit$tn_in_mass[i] = lit$n_in_mass[i]*1000
    lit$tn_out_mass[i] = lit$n_out_mass[i]*1000
    lit$tn_r_mass[i] = lit$n_retained_mass[i]*1000
    
  } else {
    lit$tn_in_mass[i] = lit$n_in_mass[i]
    lit$tn_out_mass[i] = lit$n_out_mass[i]
    lit$tn_r_mass[i] = lit$n_retained_mass[i]
    
  }  
}

# fill in mass in/out if missing (from retained)
# only missing from source 51, fill in by using proportion removed

lit$tp_out_mass[is.na(lit$tp_out_mass)] = lit$tp_in_mass[is.na(lit$tp_out_mass)]*(1-lit$p_retained[is.na(lit$tp_out_mass)])
lit$tn_out_mass[is.na(lit$tn_out_mass)] = lit$tn_in_mass[is.na(lit$tn_out_mass)]*(1-lit$n_retained[is.na(lit$tn_out_mass)])

# order data properly
lit$state <- ""
lit$Rp_calculated <- ""
lit$Rn_calculated <- ""
lit <- lit[,c(34,2,4,42,3,5,6,8,9,11,35,12,16,36,17,37,24,38,25,39,31,43,32,44,33 )]

names(lit) <- names(dat)
#############################
# get all data together
#############################

dat.all <- rbind(dat, brett, har, donald, maav, lit, finlay)

dat.all$Rp <- as.numeric(dat.all$Rp_source)
dat.all$Rp_calculated[is.na(dat.all$Rp_calculated)] <- 1-(dat.all$tp_out_mass[is.na(dat.all$Rp_calculated)]/dat.all$tp_in_mass[is.na(dat.all$Rp_calculated)])
dat.all$Rp[is.na(dat.all$Rp)] <- as.numeric(dat.all$Rp_calculated[is.na(dat.all$Rp)])
dat.all$Rn <- as.numeric(dat.all$Rn_source)
dat.all$Rn_calculated[is.na(dat.all$Rn_calculated)] <- 1-(dat.all$tn_out_mass[is.na(dat.all$Rn_calculated)]/dat.all$tn_in_mass[is.na(dat.all$Rn_calculated)])
dat.all$Rn[is.na(dat.all$Rn)] <- as.numeric(dat.all$Rn_calculated[is.na(dat.all$Rn)])

# remove duplicates of long residence time systems (>50 years)
# this includes Taho, Superior, Michigan, Superior
# keep values with most complete data

#dat.all[c(1:648,650:759,761:804,806:1160)]
#################################
# calculate all input/output variables
###################################
# calculate volume for all lakes
dat.all$volume <- as.numeric(dat.all$volume)
dat.all$Q <- as.numeric(dat.all$Q)

dat.all$volume[which(is.na(dat.all$volume))] = (dat.all$mean_depth[which(is.na(dat.all$volume))]/1000)*dat.all$surface_area[which(is.na(dat.all$volume))]
dat.all$surface_area[which(is.na(dat.all$surface_area))] = dat.all$volume[which(is.na(dat.all$surface_area))]/(dat.all$mean_depth[which(is.na(dat.all$surface_area))]/1000)

#calculate Q for all lakes
dat.all$Q[which(is.na(dat.all$Q))] = (dat.all$volume[which(is.na(dat.all$Q))]/dat.all$res_time[which(is.na(dat.all$Q))])*((60*60*24*365)/10^9)
#for lakes that have q = 0, recalculate from volume and residence time to get non-zero answer
dat.all$Q[which(dat.all$Q==0)] = (dat.all$volume[which(dat.all$Q==0)]/dat.all$res_time[which(dat.all$Q==0)])*((60*60*24*365)/10^9)

dat.all$volume[which(is.na(dat.all$volume))] = dat.all$res_time[which(is.na(dat.all$volume))]*dat.all$Q[which(is.na(dat.all$volume))]*((60*60*24*365)/10^9)

# calculate in/out nutrients

# tp_out_conc
dat.all$tp_out_conc <- as.numeric(dat.all$tp_out_conc)
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
dat.all$tn_out_conc <- as.numeric(dat.all$tn_out_conc)
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

# H
dat.all$h <- dat.all$mean_depth/dat.all$res_time

#aerial mass
dat.all$tn_in_mass_areal <- (dat.all$tn_in_mass*1000)/(dat.all$surface_area*1000000)
dat.all$tn_out_mass_areal <- (dat.all$tn_out_mass*1000)/(dat.all$surface_area*1000000)
dat.all$tn_r_mass_areal <- dat.all$tn_in_mass_areal-dat.all$tn_out_mass_areal

dat.all$tp_in_mass_areal <- (dat.all$tp_in_mass*1000)/(dat.all$surface_area*1000000)
dat.all$tp_out_mass_areal <- (dat.all$tp_out_mass*1000)/(dat.all$surface_area*1000000)
dat.all$tp_r_mass_areal <- dat.all$tp_in_mass_areal-dat.all$tp_out_mass_areal


# calculate np in and out
dat.all$np_in <- (dat.all$tn_in_mass/dat.all$tp_in_mass)*(30.97/14)
dat.all$np_out <- (dat.all$tn_out_mass/dat.all$tp_out_mass)*(30.97/14)

# flag outliers based on relationship between areal loading and restime
mod <- lm(log10(tn_in_mass_areal)~log10(res_time), data=dat.all)
cooksd <- cooks.distance(mod)
influential.n <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
#verified all EPA cites, remove those from potential outliers
#influential.n <- influential.n[13:30]
#dat.all$outlier <- 0
#dat.all$outlier[influential.n] = 1
plot(log10(tn_in_mass_areal)~log10(res_time), dat.all)
points(log10(tn_in_mass_areal)~log10(res_time), dat.all[influential.n, ], pch = 16, col = "red")

mod <- lm(log10(tp_in_mass_areal)~log10(res_time), data=dat.all)
cooksd <- cooks.distance(mod)
influential.p <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
#validated epa data
#influential.p <- influential.p[c(18:45)]
#dat.all$outlier[influential.p] = 1
plot(log10(tp_in_mass_areal)~log10(res_time), dat.all)
points(log10(tp_in_mass_areal)~log10(res_time), dat.all[influential.p, ], pch = 16, col = "red")


mod <- lm(log10(tp_in_mass_areal)~log10(tn_in_mass_areal), data=dat.all)
cooksd <- cooks.distance(mod)
influential.np <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
plot(log10(tp_in_mass_areal)~log10(tn_in_mass_areal), dat.all)
points(log10(tp_in_mass_areal)~log10(tn_in_mass_areal), dat.all[influential.np, ], pch = 16, col = "red")

# here, run optimization and return to predicted
# predicted retention
dat.all$Rn_predicted <- 1-(exp((-Fit.N.np$par[1]*(dat.all$res_time/dat.all$mean_depth))))
dat.all$Rp_predicted <- 1-(1/(1+(Fit.np.Rp$par[1]*(dat.all$res_time^(1+Fit.np.Rp$par[2])))))



# make subsets of data
# all lakes with N budgets
dat.p <- dat.all[!is.na(dat.all$Rp)&!is.na(dat.all$res_time), ]
dat.p.real <- dat.p[dat.p$Rp>-1, ]
dat.p.pos <- dat.p[dat.p$Rp>0, ]

dat.n <- dat.all[!is.na(dat.all$Rn)&!is.na(dat.all$res_time)&!is.na(dat.all$mean_depth), ]
dat.n.real <-dat.n[dat.n$Rn>=-1,]
dat.n.pos <-dat.n[dat.n$Rn>=0 & dat.n$tn_r_mass_areal>0,]

# dataframe for lakes with both N and P data
dat.np <- dat.all[!is.na(dat.all$Rp)&!is.na(dat.all$Rn),]
dat.np.real <- dat.all[!is.na(dat.all$Rp)&!is.na(dat.all$Rn) & dat.all$Rn>-1 &
                         dat.all$Rp>-1,]

stoich <- dat.all[!is.na(dat.all$np_in)&
                   !is.na(dat.all$np_out)&
                   !is.na(dat.all$mean_depth)&
                   !is.na(dat.all$res_time)&
                   !is.na(dat.all$tn_in_mass)&
                   !is.na(dat.all$tp_in_mass)&
                   dat.all$Rn>-1 &
                   dat.all$Rp>-1, ]

# calculate stoich change
# calculate log of change - then make numbers with decreasing TN:TP negative, those with
# increasing TN:TP positive
stoich$np_out_predicted <- ((stoich$tn_in_mass*(1-stoich$Rn_predicted))/((stoich$tp_in_mass)*(1-stoich$Rp_predicted)))*(30.97/14)
stoich$np_change <- log10(stoich$np_out) - log10(stoich$np_in) 
stoich$np_change_predicted <- log10(stoich$np_out_predicted) - log10(stoich$np_in)
stoich$np_r <- (stoich$tn_r_mass_areal/14)/(stoich$tp_r_mass_areal/30.97)
stoich$R_diff <- stoich$Rn - stoich$Rp

# in lagos, max TN = 20.57 max TP = 1.22
summary(dat.all$tn_out_conc)
dat.all[which(dat.all$tn_out_conc>20), ]

summary(dat.all$tp_out_conc)
dat.all[which(dat.all$tp_out_conc>1.2), ]

# flag data with impossible out concentrations (from filter above)

################################################
## calculate retenion for each N and P based
## on the N and P empirical equations
################################################

dat.all$Rn_predicted <- 1-(exp((-Fit.N.np$par[1]*(dat.all$res_time/dat.all$mean_depth))))
dat.all$Rp_predicted <- 1-(1/(1+(Fit.np.Rp$par[1]*(dat.all$res_time^(1+Fit.np.Rp$par[2])))))

# calculate Vf for each lake
dat.all$Vf <- (-1*dat.all$mean_depth/dat.all$res_time)*log(1-dat.all$Rn)

plot(dat.all$Rp~dat.all$Rp_predicted, ylim = c(-1,1))
abline(0,1,col="red")
plot(dat.all$Rn~dat.all$Rn_predicted, ylim = c(-1,1))
abline(0,1,col = "red")

abline(0,1,col = "red")
exp((-9.92*x)/9.6)


####################################
# create a data frame where lakes have retention estimates for both N and P
####################################
dat.np <- dat.all[!is.na(dat.all$Rn)&!is.na(dat.all$Rp),]
dat.np.real <- dat.np[dat.np$Rp > -1 & dat.np$Rn > -1, ]
dat.np.complete
dat.np$relret <- dat.np$Rn/dat.np$Rp

# now create input TN:TP and out TN:TP
# make ratios molar

# rank lakes by depth and residence time
dat.np$rank_restime <- rank(dat.np$res_time)
dat.np$rank_depth <- rank(dat.np$mean_depth, )
dat.np$rank_sum = dat.np$rank_restime + (dat.np$rank_depth-796)

# rank by how depth and residence time are used in the predicted Rn/Rp equations

dat.np$rank_rel <- (1-(exp((-9.92*(dat.np$res_time/dat.np$mean_depth)))))/(1-(1/(1+(1.12*(dat.np$res_time^.47)))))

plot(log10(dat.np$res_time)~dat.np$rank_rel)

# calculate the proportion of lakes that are predicted to retain more N than P

length(which(dat.np$Rn_predicted>dat.np$Rp_predicted))/nrow(dat.np)

#calculate the actual number of lakes that have Rn>Rp

length(which(dat.np$Rn>dat.np$Rp))/nrow(dat.np)

#dat.np$rank_diff = dat.np$rank_restime - dat.np$rank_depth

# estimate N:P_out based on model retention estimates

plot(log10(dat.np$res_time)~dat.np$rank_diff)

# calculate Vf for each lake observation

#################################################
## create a figure that shows change in TN:TP
## by rank in res/depth
#################################################
# Goal - arrow heads show direction of TN:TP change
# length of arrow show proportional change



pdf("stoich_change_rank.pdf", height = 8, width = 12)
par(mar=c(5,5,1,1))
plot(log10(stoich$np_in)~stoich$rank_sum, pch = 21,
     bg = rgb(222,222,222,alpha=120,max = 255), cex = 0, cex.lab = 2,
     xlab = "Rank of Depth and Res Time", ylab = "log TN:TP")
arrows(x0 = xin[stoich.up], x1 = xout[stoich.up], y0 = yin[stoich.up], y1 = yout[stoich.up],
       length = 0.0, col = rgb(255,140,0,alpha=200,max =255), lwd = 2)
arrows(x0 = xin[stoich.down], x1 = xout[stoich.down], y0 = yin[stoich.down], y1 = yout[stoich.down],
       length = 0.0, col = rgb(148,0,211,alpha=200,max =255), lwd = 2)
abline(h = log10(16), lty = 2)
text(x = 200, y = -0.2, cex = 1.3,
     "TN:TP decreasing, Rn>Rp",col = rgb(148,0,211,alpha=200,max =255))
text(x = 200, y = -0.5, cex = 1.3,
     "TN:TP increasing, Rn<Rp",col = rgb(255,140,0,alpha=200,max =255))
dev.off()





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


##############################################
## figure - Harrison vs observed retention
##############################################


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







# use hydrolakes database to look at global distribution of depth vs residence time

library(rgdal)
hl = readOGR(dsn ="/Users/Samantha/Dropbox/Differential Retention/Data/HydroLAKES_points_v10_shp", layer = "HydroLAKES_points_v10")
