#visualize projected temp in Roanoke/vinton
#CCSM4 max temp projection under RCP 8.5 and 4.5

pacman::p_load(tidyverse)

rcp_4.5_model_1_10 <- read.csv("analysis/MACA/rcp_4.5_model1-10.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), ~max(. , na.rm = TRUE)))

min(rcp_4.5_model_1_10) # 43.6
max(rcp_4.5_model_1_10) # 48.3

rcp_4.5_model_11_20 <- read.csv("analysis/MACA/rcp_4.5_model11-20.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), ~max(. , na.rm = TRUE)))

min(rcp_4.5_model_11_20) # 41.3
max(rcp_4.5_model_11_20) # 50.1

rcp_8.5_model_1_10 <- read.csv("analysis/MACA/rcp_8.5_model1-10.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), ~max(. , na.rm = TRUE)))

min(rcp_8.5_model_1_10) # 46.1
max(rcp_8.5_model_1_10) # 52.8

rcp_8.5_model_11_20 <- read.csv("analysis/MACA/rcp_8.5_model11-20.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), ~max(. , na.rm = TRUE)))

min(rcp_8.5_model_11_20) # 43.5
max(rcp_8.5_model_11_20) # 56.8


#vs historical max temp from 1950-2005 ESM2M model
historical_temp_ESM2M <- read.csv("analysis/MACA/historical_temp_CCSM4.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_historical.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#plot historical max temp
ggplot(historical_temp_ESM2M, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 39.9C in 1955
