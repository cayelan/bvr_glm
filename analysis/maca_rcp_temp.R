#visualize projected temp in Roanoke/vinton
#CCSM4 max temp projection under RCP 8.5 and 4.5

pacman::p_load(tidyverse)

future_temp <- read.csv("analysis/MACA/maxtemp_CCSM4_rcp85.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_rcp85.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 51.3C in 2098

future_temp_rcp4.5 <- read.csv("analysis/MACA/maxtemp_CCSM4_rcp45.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_rcp45.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_rcp4.5, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 46.2C in 2057

#vs historical max temp from 1950-2005 ESM2M model
historical_temp_ESM2M <- read.csv("analysis/MACA/historical_temp_ESM2M.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2M_historical.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#plot historical max temps
ggplot(historical_temp_ESM2M, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 39.9C in 1955

#vs historical max temp from 1950-2005 ESM2G model
historical_temp_ESM2G <- read.csv("analysis/MACA/historical_temp_ESM2G.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2G_historical.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#plot historical max temps
ggplot(historical_temp_ESM2G, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 39.9C in 1955

#vs historical max temp from 1950-2005 CCSM4 model
historical_temp_CCSM4 <- read.csv("analysis/MACA/historical_temp_CCSM4.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_historical.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#plot historical max temps
ggplot(historical_temp_CCSM4, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 39.9C in 1955

#-----------------------------------------------------------------------------#
# Looking at a different model to confirm results (37.313, -79.816)

# GFDL-ESM2M (USA)

future_temp_esm2m <- read.csv("analysis/MACA/maxtemp_ESM2M_rcp85.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2M_rcp85.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_esm2m, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 49.4C in 2072

future_temp_rcp4.5_esm2m <- read.csv("analysis/MACA/maxtemp_ESM2M_rcp45.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2M_rcp45.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_rcp4.5_esm2m, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 46.1C in 2069


# GFDL-ESM2G (USA)

future_temp_esm2g <- read.csv("analysis/MACA/maxtemp_ESM2G_rcp85.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2G_rcp85.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_esm2g, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 48.3C in 2093

future_temp_rcp4.5_esm2g <- read.csv("analysis/MACA/maxtemp_ESM2G_rcp45.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2G_rcp45.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_rcp4.5_esm2g, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 46.8C in 2037