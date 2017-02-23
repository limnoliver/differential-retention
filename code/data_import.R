#import differential retention data and 
#get it all in same columns/units

setwd("data")

###################
# epa data
###################
epa <- read.csv("EPA data extraction.csv", header = TRUE)

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
har$state = ""
har$latitude = ""
har$longitude = ""
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
        har$tn_out_mass[i] = har$N_out_mass[i]*1000
        har$tp_out_mass[i] = har$P_out_mass[i]*1000
        har$tn_in_mass[i] = har$N_in_mass[i]*1000
        har$tp_in_mass[i] = har$P_in_mass[i]*1000
      } else if (har$mass_units[i] %in% "mg m-2 y-1"){
        har$tn_out_mass[i] = (har$N_out_mass[i]*(har$Area_km2[i]*1000000))/1000000
        har$tp_out_mass[i] = (har$P_out_mass[i]*(har$Area_km2[i]*1000000))/1000000
        har$tn_in_mass[i] = (har$N_in_mass[i]*(har$Area_km2[i]*1000000))/1000000
        har$tp_in_mass[i] = (har$P_in_mass[i]*(har$Area_km2[i]*1000000))/1000000
      } else {
        har$tn_out_mass[i] = har$N_out_mass[i]
        har$tp_out_mass[i] = har$P_out_mass[i]
        har$tn_in_mass[i] = har$N_in_mass[i]
        har$tp_in_mass[i] = har$P_in_mass[i]
      }
}

# convert Q from km3 y-1 to m3 s-1                       
        
har$Q <- har$Q_km3.yr*(1/(60*60*24*365))*10^9       
 
har <- har[,c(1,2,4,24,3,25,26,7,5,8,35,6,16,34,18,32,11,33,13,32,30,28,29,27,23)]
names(har) <- names(dat)
# Maavara data
maav <- read.csv("Maavara et al 2015 data_with N.csv", header = TRUE)

########################
# Brett & Benjamin data
########################

brett <- read.csv("Brett_with_N.csv", header = TRUE)
brett <- brett[,c(2:28)]
brett[,1] = "brett2008"
brett <- brett[,c(1:9, 11:27)] 

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
brett$Rp_source = ""
brett$Rn_calculated = ""
brett$notes = ""
brett <- brett[,c(1,2,28,3,27,29,30,4,5,10,6,7,15,9,8,22,25,23,26,21,31,13,24,32,33)]

# change units of concentration from mgperm3 to mgperL
brett$tp_in_conc = brett$tp_in_conc/1000
brett$tp_out_conc = brett$tp_out_conc/1000
brett$tn_in_conc = brett$tn_in_conc/1000
brett$tn_out_conc = brett$tn_out_conc/1000

# change units of volume from 10^6m3 to km3
brett$volume <- brett$volume/1000

donald <- read.csv("Donald et al 2015 data.csv", header = TRUE)

hist(epa$Rp[epa$Rp>0])
length(which(epa$Rp<0))
hist(epa)

hist(epa$Rn[epa$Rn>0])
hist(epa$Rn)
length(which(epa$Rn<0))

plot(epa$Rn[epa$Rn>0]~epa$Rp[epa$Rn>0])

#get rid of unrealistic values (< -1)
epa.filtered = epa[epa$Rn>-1 & epa$Rp>-1,]
plot(epa.filtered$Rn~epa.filtered$Rp)

#get rid of all negative values
epa.positive = epa[epa$Rn>0 & epa$Rp>0,]
plot(epa.positive$Rn ~ epa.positive$Rp)
abline(0,1, col = "red")
epa.positive$rret = epa.positive$Rn/epa.positive$Rp
epa.positive$h = epa.positive$mean_depth_m/epa.positive$retention_time_years
plot(epa.positive$rret~log10(epa.positive$h))

# show relative retention vs res time
plot(epa.positive$Rp~log10(epa.positive$retention_time_years))
points(epa.positive$Rn~log10(epa.positive$retention_time_years), col = "red", add = TRUE)

# show really short residence time relationships
epa.short = epa.positive[epa$retention_time_years<.5,]
plot(epa.short$rret~log10(epa.short$retention_time_years))
