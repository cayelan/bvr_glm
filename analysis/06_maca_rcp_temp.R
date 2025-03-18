# Visualize projected future air temperatures in Roanoke/Vinton, VA, USA
# Temp projections under RCP 8.5 and 4.5
# written by Heather Wander
# 22 August 2024

pacman::p_load(tidyverse)

rcp_4.5_model_1_10 <-  read.csv("analysis/MACA/rcp_4.5_model1-10.csv", skip=26) |> 
  mutate(across(-yyyy.mm.dd, ~ . - 273.15)) |> 
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |> 
  select(-yyyy.mm.dd) |> 
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

rcp_4.5_model_11_20 <- read.csv("analysis/MACA/rcp_4.5_model11-20.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

rcp_8.5_model_1_10 <- read.csv("analysis/MACA/rcp_8.5_model1-10.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

rcp_8.5_model_11_20 <- read.csv("analysis/MACA/rcp_8.5_model11-20.csv", skip=26) |> 
  mutate(across(.cols = -all_of("yyyy.mm.dd"), ~ . - 273.15)) |>
  mutate(yyyy.mm.dd = as.Date(yyyy.mm.dd)) |>
  select(-yyyy.mm.dd) |>
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(), 
               names_to = c("model", "statistic"), 
               names_pattern = "^(.*)_(max|mean)$") |> 
  pivot_wider(names_from = model, values_from = value)

#vs historical max temp from 1950-2005 ESM2M model
historical_temp_ESM2M <- read.csv("analysis/MACA/historical_temp_CCSM4.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_historical.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

mean(historical_temp_ESM2M$temp) # 19.1

#--------------------------------------------------------------------------#
# create table s4

# list model names and remove the last part
model_names <- c(names(rcp_4.5_model_1_10),
                 names(rcp_4.5_model_11_20))

model_names <- model_names[model_names != "statistic"]

# Remove the suffix "_rcp45.k" or "_rcp85.k"
model_names <- sub("_rcp[48]5\\.K.$", "", model_names)

# Remove "tasmax_" from the beginning
model_names <- sub("^tasmax_", "", model_names)

# Compute mean differences
rcp4.5_diff <- c(as.numeric(rcp_4.5_model_1_10[-1]) - 19.1,
                 as.numeric(rcp_4.5_model_11_20[-1]) - 19.1)

rcp8.5_diff <- c(as.numeric(rcp_8.5_model_1_10[-1]) - 19.1,
                 as.numeric(rcp_8.5_model_11_20[-1]) - 19.1)

# Create the final table
result_table <- data.frame(
  model_name = model_names,
  rcp4.5_diff = round(rcp4.5_diff,1),
  rcp8.5_diff = round(rcp8.5_diff,1)
)

#write.csv(result_table, "./analysis/data/maca_temp_diff.csv", row.names = F)
