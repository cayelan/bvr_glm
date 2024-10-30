# lake.csv output file plots for different scenarios

# Load packages, set sim folder, load nml file ####
pacman::p_load(tidyverse,lubridate,ncdf4,glmtools)

#note that output.nc is the same as lake.csv

#baseline
#ln <- get_var("sims/baseline/output/output.nc", "lake_number", reference="surface")
#temp <- get_var("sims/baseline/output/output.nc", "temp", reference="surface")
#surf_temp <- get_var("sims/baseline/output/output.nc", "surface_temp", reference="surface")
#min_temp <- get_var("sims/baseline/output/output.nc", "min_temp", reference="surface")
#max_temp <- get_var("sims/baseline/output/output.nc", "max_temp", reference="surface")
#albedo <- get_var("sims/baseline/output/output.nc", "albedo", reference="surface")
#snow_dens <- get_var("sims/baseline/output/output.nc", "snow_density", reference="surface")
#lake_level <- get_var("sims/baseline/output/output.nc", "lake_level", reference="surface")
#rain <- get_var("sims/baseline/output/output.nc", "rain", reference="surface")
#evap <- get_var("sims/baseline/output/output.nc", "evaporation", reference="surface")
#sa <-  get_var("sims/baseline/output/output.nc", "surface_area", reference="surface")
#lake_vol <-  get_var("sims/baseline/output/output.nc", "lake_volume", reference="surface")
#wind <-  get_var("sims/baseline/output/output.nc", "wind", reference="surface")
#solar <-  get_var("sims/baseline/output/output.nc", "solar", reference="surface")
#precip <-  get_var("sims/baseline/output/output.nc", "precipitation", reference="surface")
#white_ice_thick <-  get_var("sims/baseline/output/output.nc", "white_ice_thickness", reference="surface")
#blue_ice_thick <-  get_var("sims/baseline/output/output.nc", "blue_ice_thickness", reference="surface")
#
##plus 1C
#ln_plus1C <- get_var("sims/plus1/output/output.nc", "lake_number", reference="surface")
#temp_plus1C <- get_var("sims/plus1/output/output.nc", "temp", reference="surface")
#surf_temp_plus1C <- get_var("sims/plus1/output/output.nc", "surface_temp", reference="surface")
#min_temp_plus1C <- get_var("sims/plus1/output/output.nc", "min_temp", reference="surface")
#max_temp_plus1C <- get_var("sims/plus1/output/output.nc", "max_temp", reference="surface")
#albedo_plus1C <- get_var("sims/plus1/output/output.nc", "albedo", reference="surface")
#snow_dens_plus1C <- get_var("sims/plus1/output/output.nc", "snow_density", reference="surface")
#lake_level_plus1C <- get_var("sims/plus1/output/output.nc", "lake_level", reference="surface")
#rain_plus1C <- get_var("sims/plus1/output/output.nc", "rain", reference="surface")
#evap_plus1C <- get_var("sims/plus1/output/output.nc", "evaporation", reference="surface")
#sa_plus1C <-  get_var("sims/plus1/output/output.nc", "surface_area", reference="surface")
#lake_vol_plus1C <-  get_var("sims/plus1/output/output.nc", "lake_volume", reference="surface")
#wind_plus1C <-  get_var("sims/plus1/output/output.nc", "wind", reference="surface")
#solar_plus1C <-  get_var("sims/plus1/output/output.nc", "solar", reference="surface")
#precip_plus1C <-  get_var("sims/plus1/output/output.nc", "precipitation", reference="surface")
#white_ice_thick_plus1C <-  get_var("sims/plus1/output/output.nc", "white_ice_thickness", reference="surface")
#blue_ice_thick_plus1C <-  get_var("sims/plus1/output/output.nc", "blue_ice_thickness", reference="surface")
#
##plus 2C
#ln_plus2C <- get_var("sims/plus2/output/output.nc", "lake_number", reference="surface")
#temp_plus2C <- get_var("sims/plus2/output/output.nc", "temp", reference="surface")
#surf_temp_plus2C <- get_var("sims/plus2/output/output.nc", "surface_temp", reference="surface")
#min_temp_plus2C <- get_var("sims/plus2/output/output.nc", "min_temp", reference="surface")
#max_temp_plus2C <- get_var("sims/plus2/output/output.nc", "max_temp", reference="surface")
#albedo_plus2C <- get_var("sims/plus2/output/output.nc", "albedo", reference="surface")
#snow_dens_plus2C <- get_var("sims/plus2/output/output.nc", "snow_density", reference="surface")
#lake_level_plus2C <- get_var("sims/plus2/output/output.nc", "lake_level", reference="surface")
#rain_plus2C <- get_var("sims/plus2/output/output.nc", "rain", reference="surface")
#evap_plus2C <- get_var("sims/plus2/output/output.nc", "evaporation", reference="surface")
#sa_plus2C <-  get_var("sims/plus2/output/output.nc", "surface_area", reference="surface")
#lake_vol_plus2C <-  get_var("sims/plus2/output/output.nc", "lake_volume", reference="surface")
#wind_plus2C <-  get_var("sims/plus2/output/output.nc", "wind", reference="surface")
#solar_plus2C <-  get_var("sims/plus2/output/output.nc", "solar", reference="surface")
#precip_plus2C <-  get_var("sims/plus2/output/output.nc", "precipitation", reference="surface")
#white_ice_thick_plus2C <-  get_var("sims/plus2/output/output.nc", "white_ice_thickness", reference="surface")
#blue_ice_thick_plus2C <-  get_var("sims/plus2/output/output.nc", "blue_ice_thickness", reference="surface")
#
##plus 3C
#ln_plus3C <- get_var("sims/plus3/output/output.nc", "lake_number", reference="surface")
#temp_plus3C <- get_var("sims/plus3/output/output.nc", "temp", reference="surface")
#surf_temp_plus3C <- get_var("sims/plus3/output/output.nc", "surface_temp", reference="surface")
#min_temp_plus3C <- get_var("sims/plus3/output/output.nc", "min_temp", reference="surface")
#max_temp_plus3C <- get_var("sims/plus3/output/output.nc", "max_temp", reference="surface")
#albedo_plus3C <- get_var("sims/plus3/output/output.nc", "albedo", reference="surface")
#snow_dens_plus3C <- get_var("sims/plus3/output/output.nc", "snow_density", reference="surface")
#lake_level_plus3C <- get_var("sims/plus3/output/output.nc", "lake_level", reference="surface")
#rain_plus3C <- get_var("sims/plus3/output/output.nc", "rain", reference="surface")
#evap_plus3C <- get_var("sims/plus3/output/output.nc", "evaporation", reference="surface")
#sa_plus3C <-  get_var("sims/plus3/output/output.nc", "surface_area", reference="surface")
#lake_vol_plus3C <-  get_var("sims/plus3/output/output.nc", "lake_volume", reference="surface")
#wind_plus3C <-  get_var("sims/plus3/output/output.nc", "wind", reference="surface")
#solar_plus3C <-  get_var("sims/plus3/output/output.nc", "solar", reference="surface")
#precip_plus3C <-  get_var("sims/plus3/output/output.nc", "precipitation", reference="surface")
#white_ice_thick_plus3C <-  get_var("sims/plus3/output/output.nc", "white_ice_thickness", reference="surface")
#blue_ice_thick_plus3C <-  get_var("sims/plus3/output/output.nc", "blue_ice_thickness", reference="surface")
#
##plus 5C
#ln_plus5C <- get_var("sims/plus5/output/output.nc", "lake_number", reference="surface")
#temp_plus5C <- get_var("sims/plus5/output/output.nc", "temp", reference="surface")
#surf_temp_plus5C <- get_var("sims/plus5/output/output.nc", "surface_temp", reference="surface")
#min_temp_plus5C <- get_var("sims/plus5/output/output.nc", "min_temp", reference="surface")
#max_temp_plus5C <- get_var("sims/plus5/output/output.nc", "max_temp", reference="surface")
#albedo_plus5C <- get_var("sims/plus5/output/output.nc", "albedo", reference="surface")
#snow_dens_plus5C <- get_var("sims/plus5/output/output.nc", "snow_density", reference="surface")
#lake_level_plus5C <- get_var("sims/plus5/output/output.nc", "lake_level", reference="surface")
#rain_plus5C <- get_var("sims/plus5/output/output.nc", "rain", reference="surface")
#evap_plus5C <- get_var("sims/plus5/output/output.nc", "evaporation", reference="surface")
#sa_plus5C <-  get_var("sims/plus5/output/output.nc", "surface_area", reference="surface")
#lake_vol_plus5C <-  get_var("sims/plus5/output/output.nc", "lake_volume", reference="surface")
#wind_plus5C <-  get_var("sims/plus5/output/output.nc", "wind", reference="surface")
#solar_plus5C <-  get_var("sims/plus5/output/output.nc", "solar", reference="surface")
#precip_plus5C <-  get_var("sims/plus5/output/output.nc", "precipitation", reference="surface")
#white_ice_thick_plus5C <-  get_var("sims/plus5/output/output.nc", "white_ice_thickness", reference="surface")
#blue_ice_thick_plus5C <-  get_var("sims/plus5/output/output.nc", "blue_ice_thickness", reference="surface")
#
##plots
#plot(ln$DateTime, ln$lake_number, type = "l")
#points(ln_plus1C$DateTime, ln_plus1C$lake_number, col="#5B8E7D", type="l")
#points(ln_plus2C$DateTime, ln_plus2C$lake_number, col="#F4E285", type="l")
#points(ln_plus3C$DateTime, ln_plus3C$lake_number, col="#F4A259", type="l")
#points(ln_plus5C$DateTime, ln_plus5C$lake_number, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)
#
#plot(surf_temp$DateTime, surf_temp$surface_temp, type = "l")
#points(surf_temp_plus1C$DateTime, surf_temp_plus1C$surface_temp, col="#5B8E7D", type="l")
#points(surf_temp_plus2C$DateTime, surf_temp_plus2C$surface_temp, col="#F4E285", type="l")
#points(surf_temp_plus3C$DateTime, surf_temp_plus3C$surface_temp, col="#F4A259", type="l")
#points(surf_temp_plus5C$DateTime, surf_temp_plus5C$surface_temp, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)
#
#plot(albedo$DateTime, albedo$albedo, type = "l")
#points(albedo_plus1C$DateTime, albedo_plus1C$albedo, col="#5B8E7D", type="l")
#points(albedo_plus2C$DateTime, albedo_plus2C$albedo, col="#F4E285", type="l")
#points(albedo_plus3C$DateTime, albedo_plus3C$albedo, col="#F4A259", type="l")
#points(albedo_plus5C$DateTime, albedo_plus5C$albedo, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)
#
#plot(blue_ice_thick$DateTime, blue_ice_thick$blue_ice_thickness, type = "l", ylim = c(0,2))
#points(blue_ice_thick_plus1C$DateTime, blue_ice_thick_plus1C$blue_ice_thickness, col="#5B8E7D", type="l")
#points(blue_ice_thick_plus2C$DateTime, blue_ice_thick_plus2C$blue_ice_thickness, col="#F4E285", type="l")
#points(blue_ice_thick_plus3C$DateTime, blue_ice_thick_plus3C$blue_ice_thickness, col="#F4A259", type="l")
#points(blue_ice_thick_plus5C$DateTime, blue_ice_thick_plus5C$blue_ice_thickness, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)
#
#plot(sa$DateTime, sa$surface_area, type = "l")
#points(sa_plus1C$DateTime, sa_plus1C$surface_area, col="#5B8E7D", type="l")
#points(sa_plus2C$DateTime, sa_plus2C$surface_area, col="#F4E285", type="l")
#points(sa_plus3C$DateTime, sa_plus3C$surface_area, col="#F4A259", type="l")
#points(sa_plus5C$DateTime, sa_plus5C$surface_area, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)
#
#plot(precip$DateTime, precip$precipitation, type = "l")
#points(precip_plus1C$DateTime, precip_plus1C$precipitation, col="#5B8E7D", type="l")
#points(precip_plus2C$DateTime, precip_plus2C$precipitation, col="#F4E285", type="l")
#points(precip_plus3C$DateTime, precip_plus3C$precipitation, col="#F4A259", type="l")
#points(precip_plus5C$DateTime, precip_plus5C$precipitation, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)
#
#plot(evap$DateTime, evap$evaporation, type = "l")
#points(evap_plus1C$DateTime, evap_plus1C$evaporation, col="#5B8E7D", type="l")
#points(evap_plus2C$DateTime, evap_plus2C$evaporation, col="#F4E285", type="l")
#points(evap_plus3C$DateTime, evap_plus3C$evaporation, col="#F4A259", type="l")
#points(evap_plus5C$DateTime, evap_plus5C$evaporation, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)
#
#plot(wind$DateTime, wind$wind, type = "l")
#points(wind_plus1C$DateTime, wind_plus1C$wind, col="#5B8E7D", type="l")
#points(wind_plus2C$DateTime, wind_plus2C$wind, col="#F4E285", type="l")
#points(wind_plus3C$DateTime, wind_plus3C$wind, col="#F4A259", type="l")
#points(wind_plus5C$DateTime, wind_plus5C$wind, col="#BC4B51", type="l")
#legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
#       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
#       lty=1, cex=0.8, bty='n', horiz=T)

#-----------------------------------------------------------------------------#
#plotting vars in lake.csv file
baseline <- read.csv("sims/baseline/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time))
plus1C <- read.csv("sims/plus1/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time))
plus5C <- read.csv("sims/plus5/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time))
plus10C <- read.csv("sims/plus10/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time))

#plot various vars
jpeg('figures/surf_temp_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(baseline$DateTime, baseline$Surface.Temp, 
     type = "l", ylim=c(-10,40), col = "#00603d")
points(plus1C$DateTime, plus1C$Surface.Temp, col="#c6a000", type="l")
points(plus5C$DateTime, plus5C$Surface.Temp, col="#c85b00", type="l")
points(plus10C$DateTime, plus10C$Surface.Temp, col="#680000", type="l")
legend("bottom", legend=c("baseline", "plus1C","plus5C","plus10C"),
       col=c("#00603d", "#c6a000","#c85b00","#680000"), 
       lty=1, cex=0.7, bty='n', horiz=T)
dev.off()

jpeg('figures/sa_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(baseline$DateTime, baseline$Surface.Area, type = "l", ylim = c(250000,400000))
points(plus1C$DateTime, plus1C$Surface.Area, col="#F4E285", type="l")
points(plus3C$DateTime, plus3C$Surface.Area, col="#F4A259", type="l")
points(plus5C$DateTime, plus5C$Surface.Area, col="#BC4B51", type="l")
legend("topleft", legend=c("baseline", "plus1C","plus3C","plus5C"),
       col=c("black", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz = T)
dev.off()

jpeg('figures/depth_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(baseline$DateTime, baseline$Lake.Level, type = "l", ylim = c(9, 14))
points(plus1C$DateTime, plus1C$Lake.Level, col="#F4E285", type="l")
points(plus3C$DateTime, plus3C$Lake.Level, col="#F4A259", type="l")
points(plus5C$DateTime, plus5C$Lake.Level, col="#BC4B51", type="l")
legend("bottom", legend=c("baseline", "plus1C","plus3C","plus5C"),
       col=c("black", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/blueice_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(baseline$DateTime, baseline$Blue.Ice.Thickness, type = "l")
points(plus1C$DateTime, plus1C$Blue.Ice.Thickness, col="#F4E285", type="l")
points(plus3C$DateTime, plus3C$Blue.Ice.Thickness, col="#F4A259", type="l")
points(plus5C$DateTime, plus5C$Blue.Ice.Thickness, col="#BC4B51", type="l")
legend("top", legend=c("baseline", "plus1C","plus3C","plus5C"),
       col=c("black", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

#quick plot of residence time as a check
outflow<-read.csv("sims/baseline/inputs/BVR_spillway_outflow_2015_2022_metInflow.csv", header=T)
outflow$time<-as.POSIXct(strptime(outflow$time, "%Y-%m-%d", tz="EST"))

#get WRT
volume<-glmtools::get_var("sims/baseline/output/output.nc", 
                          "lake_volume", reference="surface") |> 
  filter(DateTime < as.POSIXct("2020-12-31")) #in m3

# Calculate WRT from modeled volume and measured outflow
volume$time<-as.POSIXct(strptime(volume$DateTime, "%Y-%m-%d", tz="EST"))
wrt<-merge(volume, outflow, by='time')
wrt$wrt <- ((wrt$lake_volume)/(wrt$FLOW))*(1/60)*(1/60)*(1/24) #residence time in days
plot(wrt$time,wrt$wrt)
mean(wrt$wrt)
median(wrt$wrt)

