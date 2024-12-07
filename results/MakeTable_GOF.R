#*****************************************************************
#* TITLE:   BVR GLM script to create goodness-of-fit tables            
#* AUTHORS:  HLW (adapted from ccc fcr glm script)
#* DATE:   23Nov22
#* NOTES:  This script creates output tables needed for both the main 
#*         manuscript and SI 
#*****************************************************************

# devtools::install_github("CareyLabVT/GLMr", force = TRUE) 
# devtools::install_github("CareyLabVT/glmtools", force = TRUE)

# Load in libraries
# pacman::p_load(devtools)
pacman::p_load(zoo,tidyverse,lubridate,hydroGOF,GLMr,glmtools,ggplot2)

# Set working directory
setwd("/Users/heatherwander/Documents/VirginiaTech/research/BVR_glm/bvr_glm")
sim_folder <- getwd()

# Using baseline model data (aka: no oxygenation scenarios)

# Load in nc_file and needed variables
baseline <- file.path(sim_folder, 'output/output.nc')
field <- file.path(sim_folder, 'field_data/CleanedObsTemp.csv')

obs_temp <- read_csv('field_data/CleanedObsTemp.csv') %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(temp_obs = temp)

# Find necessary parameters
print(sim_vars(baseline))

############################# Full year, full water column GOF statistics ############################

# Create empty dataframe for GOF results
# Using all GOF parameters for now - will cull eventually

# Full year, full water column, full period (2013-2019)
all_gof <- setNames(data.frame(matrix(ncol=2,nrow=22)),c("Parameter","Temp"))
all_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","NRMSE%_all","PBIAS%_all","RSR_all","rSD_all","NSE_all","mNSE_all","rNSE_all","d_all","md_all","rd_all","cp_all","r_all",
                       "R2_all","bR2_all","KGE_all","VE_all","r.Spearman","nonparamR2")

# Full year, full water column, calibration period (2013-2018)
all_gof_cal <- setNames(data.frame(matrix(ncol=2,nrow=22)),c("Parameter","Temp"))
all_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal","NSE_cal","mNSE_cal","rNSE_cal","d_cal","md_cal","rd_cal","cp_cal","r_cal",
                           "R2_cal","bR2_cal","KGE_cal","VE_cal","r.Spearman","nonparamR2")

# Full year, full water column, validation period (2019)
all_gof_val <- setNames(data.frame(matrix(ncol=2,nrow=22)),c("Parameter","Temp"))
all_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","NRMSE%_val","PBIAS%_val","RSR_val","rSD_val","NSE_val","mNSE_val","rNSE_val","d_val","md_val","rd_val","cp_val","r_val",
                           "R2_val","bR2_val","KGE_val","VE_val","r.Spearman","nonparamR2")

#### TEMP ####
# 9 m Obs temp
obs_temp$Depth <- as.numeric(obs_temp$Depth)
obs_temp_cal <- obs_temp %>% 
  dplyr::filter(DateTime < "2019-01-01")
obs_temp_val <- obs_temp %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Full water column Model temp
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) 
mod_temp <- get_temp(baseline, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(temp_mod = temp)
mod_temp$Depth <- as.numeric(mod_temp$Depth)
mod_temp_cal <- mod_temp %>% 
  dplyr::filter(DateTime < "2019-01-01")
mod_temp_val <- mod_temp %>% 
  dplyr::filter(DateTime >= "2019-01-01")

# Select dates where we have observations and calculate ALL GOF parameters (can cull later)
# For FULL year
comb_temp <- left_join(obs_temp,mod_temp,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(temp_obs = mean(temp_obs), temp_mod = mean(temp_mod)) %>% 
  ungroup()
all_gof$Temp[1:21] <- gof(comb_temp$temp_mod,comb_temp$temp_obs,do.spearman = TRUE)

#now need to calculate non-parametric (ranked) R2, following Brett et al. 2016
comb_temp_rank <- comb_temp %>% 
  mutate(rank_obs = rank(temp_obs),
         rank_mod = rank(temp_mod)) 
all_gof$Temp[22] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared

#calibration years only
comb_temp_cal <- left_join(obs_temp_cal,mod_temp_cal,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(temp_obs = mean(temp_obs), temp_mod = mean(temp_mod)) %>% 
  ungroup()
all_gof_cal$Temp[1:21] <- gof(comb_temp_cal$temp_mod,comb_temp_cal$temp_obs,do.spearman = TRUE)

comb_temp_cal_rank <- comb_temp_cal %>% 
  mutate(rank_obs = rank(temp_obs),
         rank_mod = rank(temp_mod)) 
all_gof_cal$Temp[22] <- summary(lm(comb_temp_cal_rank$rank_obs ~ comb_temp_cal_rank$rank_mod))$r.squared

#validation years only
comb_temp_val <- left_join(obs_temp_val,mod_temp_val,by=c("DateTime","Depth")) %>% 
  mutate(year = year(DateTime),
         month = month(DateTime)) %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(temp_obs = mean(temp_obs), temp_mod = mean(temp_mod)) %>% 
  ungroup()
all_gof_val$Temp[1:21] <- gof(comb_temp_val$temp_mod,comb_temp_val$temp_obs,do.spearman = TRUE)

comb_temp_val_rank <- comb_temp_val %>% 
  mutate(rank_obs = rank(temp_obs),
         rank_mod = rank(temp_mod)) 
all_gof_val$Temp[22] <- summary(lm(comb_temp_val_rank$rank_obs ~ comb_temp_val_rank$rank_mod))$r.squared

####Cleaning up table ####
## Add NMAE calculation for all parameters
# all_gof
all_gof[nrow(all_gof)+1,] <- NA
all_gof[23,1] <- "NMAE_all"
all_gof$Parameter[21] <- "r.Spearman_all"
all_gof$Temp[23] <- round(all_gof$Temp[2]/mean(comb_temp$temp_obs),digits = 2)

# all_gof_cal
all_gof_cal[nrow(all_gof_cal)+1,] <- NA
all_gof_cal[23,1] <- "NMAE_cal"
all_gof_cal$Parameter[21] <- "r.Spearman_cal"
all_gof_cal$Temp[23] <- round(all_gof_cal$Temp[2]/mean(comb_temp_cal$temp_obs),digits = 2)

# all_gof_val
all_gof_val[nrow(all_gof_val)+1,] <- NA
all_gof_val[23,1] <- "NMAE_val"
all_gof_val$Parameter[21] <- "r.Spearman_val"
all_gof_val$Temp[23] <- round(all_gof_val$Temp[2]/mean(comb_temp_val$temp_obs),digits = 2)

# Select GOF variables for the full year
full_n_all <- c("n_all",length(obs_temp$temp_obs))

full_n_cal <- c("n_cal",length(obs_temp$DateTime[which(obs_temp$DateTime<"2019-01-01")]))

full_n_val <- c("n_val",length(obs_temp$DateTime[which(obs_temp$DateTime>="2019-01-01")]))

full_gof_all_table <- all_gof %>% 
  filter(Parameter == "r.Spearman_all" | Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

full_gof_cal_table <- all_gof_cal %>% 
  filter(Parameter == "r.Spearman_cal" | Parameter == "R2_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NMAE_cal")

full_gof_val_table <- all_gof_val %>% 
  filter(Parameter == "r.Spearman_val" | Parameter == "R2_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NMAE_val")

full_gof_table <- rbind(full_n_all,full_gof_all_table,full_n_cal,full_gof_cal_table,full_n_val,full_gof_val_table)

write_csv(full_gof_table,'Data_Output/table_gof_watercol_bvr_2014-2019.csv')

#Visualize calibration and validation results as a heatmap




