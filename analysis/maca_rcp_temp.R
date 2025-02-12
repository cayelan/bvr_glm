#visualize projected temp in Roanoke/vinton
#CCSM4 max temp projection under RCP 8.5 and 4.5

pacman::p_load(tidyverse)

rcp_4.5_model_1_10 <-  read.csv("analysis/MACA/rcp_4.5_model1-10.csv", skip=26) |> 
  mutate(across(-yyyy.mm.dd, ~ . - 273.15)) |> 
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |> 
  select(-yyyy.mm.dd) |> 
  summarise(across(everything(), list(max = ~max(. , na.rm = TRUE), 
                                      mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(max|mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

min(rcp_4.5_model_1_10[1,c(2:11)]) # 43.6
max(rcp_4.5_model_1_10[1,c(2:11)]) # 48.3

#ggplot(rcp_4.5_model_1_10, aes(x=as.Date(yyyy.mm.dd), 
#                               y=tasmax_bcc.csm1.1_rcp45.K.)) +
#  geom_line() + theme_bw()


rcp_4.5_model_11_20 <- read.csv("analysis/MACA/rcp_4.5_model11-20.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), list(max = ~max(. , na.rm = TRUE), 
                                      mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(max|mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

min(rcp_4.5_model_11_20[1,c(2:11)]) # 41.3
max(rcp_4.5_model_11_20[1,c(2:11)]) # 50.1

rcp_8.5_model_1_10 <- read.csv("analysis/MACA/rcp_8.5_model1-10.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), list(max = ~max(. , na.rm = TRUE), 
                                      mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(max|mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

min(rcp_8.5_model_1_10[1,c(2:11)]) # 46.1
max(rcp_8.5_model_1_10[1,c(2:11)]) # 52.8

rcp_8.5_model_11_20 <- read.csv("analysis/MACA/rcp_8.5_model11-20.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), list(max = ~max(. , na.rm = TRUE), 
                                      mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(max|mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

min(rcp_8.5_model_11_20[1,c(2:11)]) # 43.5
max(rcp_8.5_model_11_20[1,c(2:11)]) # 56.8


#vs historical max temp from 1950-2005 ESM2M model
historical_temp_ESM2M <- read.csv("analysis/MACA/historical_temp_CCSM4.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_historical.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#plot historical max temp
ggplot(historical_temp_ESM2M, aes(date, temp)) + geom_line() +
  theme_bw()

max(historical_temp_ESM2M$temp)
mean(historical_temp_ESM2M$temp) #19.1
#max temp is 39.9C in 1955
