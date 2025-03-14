# Goodness of fit tables for full water column and 0.1m

pacman::p_load(tidyverse, hydroGOF, glmtools)

# note - obs and mod water quality variable outputs are saved in csvs below so no need to run this chunk of code that is commented out!
#rerun var code for baseline condiitons
#nc_file = paste0("sims/spinup/baseline/output/output.nc")  
#
#depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#
## water level
#obs_wl <- read_csv("./inputs/BVR_Daily_WaterLevel_Vol_2015_2022_interp.csv") |>
#  mutate(DateTime = as.POSIXct(strptime(Date, "%Y-%m-%d", tz="EST")),
#         Depth = "0.1") |>
#  filter(Date > as.POSIXct("2015-07-08") & Date < as.POSIXct("2022-05-04")) |>
#  select(-c(vol_m3,Date))
#
#mod_wl <- get_surface_height(nc_file, ice.rm = TRUE, snow.rm = TRUE) |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
#  filter(DateTime > as.POSIXct("2015-07-09"))  |>
#  mutate(Depth = "0.1") |> # to get wl in the same df as 0.1m vars
#  rename(mod_wl = surface_height) |>
#  select(DateTime,Depth,mod_wl)
#
#wl_compare <-mod_wl |>
#  mutate(obs_wl = obs_wl$WaterLevel_m) |>
#  select(DateTime,Depth,mod_wl,obs_wl)
#write.csv(wl_compare, "./analysis/data/wl_compare.csv", row.names = F)
#
## temp
#obstemp<-read_csv('field_data/CleanedObsTemp.csv') |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
#  filter(DateTime >= "2015-07-07")
#
#modtemp <- get_temp(nc_file, reference="surface", z_out=depths) |> 
#  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
#  filter(DateTime >= "2015-07-07")
#
#watertemp<-merge(modtemp, obstemp, by=c("DateTime","Depth")) |> 
#  rename(mod_temp = temp.x, obs_temp = temp.y)
#write.csv(watertemp, "./analysis/data/temp_compare.csv", row.names = F)
#
## DO
#obs_oxy<-read.csv('field_data/CleanedObsOxy.csv') |> 
#  select(-OXY_sat) |>
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))|>
#  filter(DateTime >= "2015-07-07")
#
#mod_oxy <- get_var(nc_file, "OXY_oxy", reference='surface', z_out=depths) |> 
#  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy")  |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
#  filter(DateTime >= "2015-07-07")
#
#oxy_compare <- merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) |> 
#  rename(mod_oxy = OXY_oxy.x, obs_oxy = OXY_oxy.y)
#write.csv(oxy_compare, "./analysis/data/oxy_compare.csv", row.names = F)
#
## NH4
#obs_nh4 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
#  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#  select(DateTime, Depth, NIT_amm) |>
#  filter(DateTime >= "2015-07-07")
#
#mod_nh4 <- get_var(nc_file, "NIT_amm", reference="surface", z_out=depths) |> 
#  pivot_longer(cols=starts_with("NIT_amm_"), names_to="Depth", names_prefix="NIT_amm_", values_to = "NIT_amm") |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))|>
#  filter(DateTime >= "2015-07-07") 
#
#nh4_compare<-merge(mod_nh4, obs_nh4, by=c("DateTime","Depth")) |> 
#  rename(mod_nh4 = NIT_amm.x, obs_nh4 = NIT_amm.y)
#write.csv(nh4_compare, "./analysis/data/nh4_compare.csv", row.names = F)
#
## NO3
#obs_no3 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
#  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#  select(DateTime, Depth, NIT_nit) |>
#  filter(DateTime >= "2015-07-07")
#
#mod_no3 <- get_var(nc_file, "NIT_nit", reference="surface", z_out=depths) |> 
#  pivot_longer(cols=starts_with("NIT_nit_"), names_to="Depth", names_prefix="NIT_nit_", values_to = "NIT_nit") |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
#  filter(DateTime >= "2015-07-07")
#
#no3_compare<-merge(mod_no3, obs_no3, by=c("DateTime","Depth")) |> 
#  rename(mod_no3 = NIT_nit.x, obs_no3 = NIT_nit.y)
#write.csv(nh4_compare, "./analysis/data/no3_compare.csv", row.names = F)
#
## PO4
#obs_po4 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
#  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#  select(DateTime, Depth, PHS_frp) |>
#  filter(DateTime >= "2015-07-07")
#
#mod_po4 <- get_var(nc_file, "PHS_frp", reference="surface", z_out=depths) |> 
#  pivot_longer(cols=starts_with("PHS_frp_"), names_to="Depth", names_prefix="PHS_frp_", values_to = "PHS_frp") |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
#  filter(DateTime >= "2015-07-07")
#
#po4_compare<-merge(mod_po4, obs_po4, by=c("DateTime","Depth")) |> 
#  rename(mod_po4 = PHS_frp.x, obs_po4 = PHS_frp.y)
#write.csv(po4_compare, "./analysis/data/po4_compare.csv", row.names = F)
#
## chl a
#obs_chla <- read.csv('field_data/CleanedObsChla.csv', header=TRUE) |> 
#  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#  select(DateTime, Depth, PHY_tchla) |>
#  filter(DateTime >= "2015-07-07")
#
##read mod chla - calculating this by hand bc something is wrong with the glm aed calc
#cyano <- glmtools::get_var(file = nc_file, var_name = "PHY_cyano", z_out = depths) |>
#  tidyr::pivot_longer(cols = starts_with("PHY_cyano"), 
#                      names_to = "Depth",names_prefix = "PHY_cyano.elv_", 
#                      values_to = "PHY_cyano") |>
#  dplyr::mutate(DateTime = as.Date(DateTime)) |>
#  dplyr::filter(DateTime >= "2015-07-08") |>
#  select(DateTime, Depth, PHY_cyano)
#
#green <- glmtools::get_var(file = nc_file, var_name = "PHY_green", z_out = depths) |>
#  tidyr::pivot_longer(cols = starts_with("PHY_green"), 
#                      names_to = "Depth",names_prefix = "PHY_green.elv_", 
#                      values_to = "PHY_green") |>
#  dplyr::mutate(DateTime = as.Date(DateTime)) |>
#  dplyr::filter(DateTime >= "2015-07-08") |>
#  select(DateTime, Depth, PHY_green)
#
#diatom <- glmtools::get_var(file = nc_file, var_name = "PHY_diatom", z_out = depths) |>
#  tidyr::pivot_longer(cols = starts_with("PHY_diatom"), 
#                      names_to = "Depth",names_prefix = "PHY_diatom.elv_", values_to = "PHY_diatom") |>
#  dplyr::mutate(DateTime = as.Date(DateTime)) |>
#  dplyr::filter(DateTime >= "2015-07-08") |>
#  select(DateTime, Depth, PHY_diatom)
#
##combine into one df 
#phytos <- purrr::reduce(list(cyano, green, diatom), dplyr::full_join) 
#
##convert from wide to long for plotting
#mod_chla <- phytos |> 
#  tidyr::pivot_longer(cols = -c(DateTime,Depth), 
#                      names_pattern = "(...)_(...*)$",
#                      names_to = c("mod", "taxon")) |> 
#  group_by(DateTime,Depth) |>
#  mutate(group_chl = ifelse(taxon=="cyano", (value * 12) / 80,
#                            ifelse(taxon=="green", (value * 12) / 30,
#                                   (value * 12) / 40))) |>
#  summarise(PHY_tchla = sum(group_chl)) |>
#  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 
#
#chla_compare <- merge(mod_chla, obs_chla, by = c("DateTime","Depth")) |> 
#  rename(mod_chla = PHY_tchla.x, obs_chla = PHY_tchla.y) 
#write.csv(chla_compare, "./analysis/data/chla_compare.csv", row.names = F)

#-----------------------------------------------------------------------#
# read in all of the water quality var csvs
wl_compare <- read.csv("./analysis/data/wl_compare.csv")
temp_compare <- read.csv("./analysis/data/temp_compare.csv")
oxy_compare <- read.csv("./analysis/data/oxy_compare.csv")
no3_compare <- read.csv("./analysis/data/no3_compare.csv")
nh4_compare <- read.csv("./analysis/data/nh4_compare.csv")
po4_compare <- read.csv("./analysis/data/po4_compare.csv")
chla_compare <- read.csv("./analysis/data/chla_compare.csv")

# Full water column, full period (2015-2022)
all_gof <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","Temp"))
all_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","ubRMSE_all",
                       "NRMSE%_all","PBIAS%_all","RSR_all","rSD_all",
                       "NSE_all","mNSE_all","rNSE_all","wNSE_all",
                       "wsNSE_all","d_all","dr_all","md_all","rd_all",
                       "cp_all","r_all","R2_all","bR2_all","VE_all", 
                       "KGE_all","KGElf_all","KGEnp_all","KGEkm_all", 
                       "r.Spearman","nonparamR2") 

# Full water column, calibration period (2015-2020)
all_gof_cal <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","Temp"))
all_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","ubRMSE_cal",
                           "NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal",
                           "NSE_cal","mNSE_cal","rNSE_cal","wNSE_cal",
                           "wsNSE_cal","d_cal","dr_cal","md_cal","rd_cal",
                           "cp_cal","r_cal","R2_cal","bR2_cal","VE_cal",
                           "KGE_cal","KGElf_cal","KGEnp_cal","KGEkm_cal", 
                           "r.Spearman","nonparamR2") 

# Full water column, validation period (2021-2022)
all_gof_val <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","Temp"))
all_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","ubRMSE",
                           "NRMSE%_val","PBIAS%_val","RSR_val","rSD_val",
                           "NSE_val","mNSE_val","rNSE_val","wNSE_val",
                           "wsNSE","d_val","dr_val","md_val","rd_val",
                           "cp_val","r_val","R2_val","bR2_val","VE_val",
                           "KGE_val","KGElf_val","KGEnp_val","KGEkm_val",
                           "r.Spearman", "nonparamR2") 

# calculate all gof metrics for full period + all different vars
all_gof$WaterLevel <- c(gof(wl_compare$mod_wl,wl_compare$obs_wl,do.spearman = TRUE), NA)
all_gof$Temp <- c(gof(watertemp$mod_temp,watertemp$obs_temp,do.spearman = TRUE), NA)
all_gof$DO <- c(gof(oxy_compare$mod_oxy,oxy_compare$obs_oxy,do.spearman = TRUE), NA)
all_gof$NH4 <- c(gof(nh4_compare$mod_nh4,nh4_compare$obs_nh4,do.spearman = TRUE), NA)
all_gof$NO3 <- c(gof(no3_compare$mod_no3,no3_compare$obs_no3,do.spearman = TRUE), NA)
all_gof$PO4 <- c(gof(po4_compare$mod_po4,po4_compare$obs_po4,do.spearman = TRUE), NA)
all_gof$Chla <- c(gof(chla_compare$mod_chla,chla_compare$obs_chla,do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_wl_rank <- wl_compare |> 
  mutate(rank_obs = rank(obs_wl),
         rank_mod = rank(mod_wl)) 
comb_temp_rank <- watertemp |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_nh4_rank <- nh4_compare |>  
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_chla_rank <- chla_compare |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof$WaterLevel[29] <- summary(lm(comb_wl_rank$rank_obs ~ comb_wl_rank$rank_mod))$r.squared
all_gof$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof$DO[29] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof$NH4[29] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof$NO3[29] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof$PO4[29] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof$Chla[29] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

# same as above but for CALIBRATION period
all_gof_cal$WaterLevel <- c(gof(wl_compare$mod_wl[wl_compare$DateTime< "2021-01-01"],
                                wl_compare$obs_wl[wl_compare$DateTime< "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_cal$Temp <- c(gof(watertemp$mod_temp[watertemp$DateTime< "2021-01-01"],
                          watertemp$obs_temp[watertemp$DateTime< "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_cal$DO <- c(gof(oxy_compare$mod_oxy[oxy_compare$DateTime< "2021-01-01"],
                        oxy_compare$obs_oxy[oxy_compare$DateTime< "2021-01-01"],
                        do.spearman = TRUE), NA)
all_gof_cal$NH4 <- c(gof(nh4_compare$mod_nh4[nh4_compare$DateTime< "2021-01-01"],
                         nh4_compare$obs_nh4[nh4_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$NO3 <- c(gof(no3_compare$mod_no3[no3_compare$DateTime< "2021-01-01"],
                         no3_compare$obs_no3[no3_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$PO4 <- c(gof(po4_compare$mod_po4[po4_compare$DateTime< "2021-01-01"],
                         po4_compare$obs_po4[po4_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$Chla <- c(gof(chla_compare$mod_chla[chla_compare$DateTime< "2021-01-01"],
                          chla_compare$obs_chla[chla_compare$DateTime< "2021-01-01"],
                          do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_wl_rank <- wl_compare |>  
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_wl),
         rank_mod = rank(mod_wl)) 
comb_temp_rank <- watertemp |>  
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_nh4_rank <- nh4_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_chla_rank <- chla_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof_cal$WaterLevel[29] <- summary(lm(comb_wl_rank$rank_obs ~ comb_wl_rank$rank_mod))$r.squared
all_gof_cal$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_cal$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_cal$DO[29] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof_cal$NH4[29] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof_cal$NO3[29] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof_cal$PO4[29] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof_cal$Chla[29] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

# now VALIDATION period
all_gof_val$WaterLevel <- c(gof(wl_compare$mod_wl[wl_compare$DateTime >= "2021-01-01"],
                                wl_compare$obs_wl[wl_compare$DateTime >= "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_val$Temp <- c(gof(watertemp$mod_temp[watertemp$DateTime >= "2021-01-01"],
                          watertemp$obs_temp[watertemp$DateTime >= "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_val$DO <- c(gof(oxy_compare$mod_oxy[oxy_compare$DateTime >= "2021-01-01"],
                        oxy_compare$obs_oxy[oxy_compare$DateTime >= "2021-01-01"],
                        do.spearman = TRUE), NA)
all_gof_val$NH4 <- c(gof(nh4_compare$mod_nh4[nh4_compare$DateTime >= "2021-01-01"],
                         nh4_compare$obs_nh4[nh4_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$NO3 <- c(gof(no3_compare$mod_no3[no3_compare$DateTime >= "2021-01-01"],
                         no3_compare$obs_no3[no3_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$PO4 <- c(gof(po4_compare$mod_po4[po4_compare$DateTime >= "2021-01-01"],
                         po4_compare$obs_po4[po4_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$Chla <- c(gof(chla_compare$mod_chla[chla_compare$DateTime >= "2021-01-01"],
                          chla_compare$obs_chla[chla_compare$DateTime >= "2021-01-01"],
                          do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_wl_rank <- wl_compare |>  
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_wl),
         rank_mod = rank(mod_wl))
comb_temp_rank <- watertemp |>  
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_nh4_rank <- nh4_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_chla_rank <- chla_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof_val$WaterLevel[29] <- summary(lm(comb_wl_rank$rank_obs ~ comb_wl_rank$rank_mod))$r.squared
all_gof_val$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_val$DO[29] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof_val$NH4[29] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof_val$NO3[29] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof_val$PO4[29] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof_val$Chla[29] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

####Cleaning up table ####
## Add NMAE calculation for all parameters
# all_gof
all_gof[nrow(all_gof)+1,] <- NA
all_gof[30,1] <- "NMAE_all"
all_gof$Parameter[28] <- "r.Spearman_all"
all_gof$WaterLevel[30] <- round(all_gof$WaterLevel[2]/mean(wl_compare$obs_wl, na.rm=T),digits = 2)
all_gof$Temp[30] <- round(all_gof$Temp[2]/mean(watertemp$obs_temp, na.rm=T),digits = 2)
all_gof$DO[30] <- round(all_gof$DO[2]/mean(oxy_compare$obs_oxy, na.rm=T),digits = 2)
all_gof$NH4[30] <- round(all_gof$NH4[2]/mean(nh4_compare$obs_nh4, na.rm=T),digits = 2)
all_gof$NO3[30] <- round(all_gof$NO3[2]/mean(no3_compare$obs_no3, na.rm=T),digits = 2)
all_gof$PO4[30] <- round(all_gof$PO4[2]/mean(po4_compare$obs_po4, na.rm=T),digits = 2)
all_gof$Chla[30] <- round(all_gof$Chla[2]/mean(chla_compare$obs_chla, na.rm=T),digits = 2)

# all_gof_cal
all_gof_cal[nrow(all_gof_cal)+1,] <- NA
all_gof_cal[30,1] <- "NMAE_cal"
all_gof_cal$Parameter[28] <- "r.Spearman_cal"
all_gof_cal$WaterLevel[30] <- round(all_gof_cal$WaterLevel[2]/mean(
  wl_compare$obs_wl[wl_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$Temp[30] <- round(all_gof_cal$Temp[2]/mean(
  watertemp$obs_temp[watertemp$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$DO[30] <- round(all_gof_cal$DO[2]/mean(
  oxy_compare$obs_oxy[oxy_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$NH4[30] <- round(all_gof_cal$NH4[2]/mean(
  nh4_compare$obs_nh4[nh4_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$NO3[30] <- round(all_gof_cal$NO3[2]/mean(
  no3_compare$obs_no3[no3_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$PO4[30] <- round(all_gof_cal$PO4[2]/mean(
  po4_compare$obs_po4[po4_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$Chla[30] <- round(all_gof_cal$Chla[2]/mean(
  chla_compare$obs_chla[chla_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)

# all_gof_val
all_gof_val[nrow(all_gof_val)+1,] <- NA
all_gof_val[30,1] <- "NMAE_val"
all_gof_val$Parameter[28] <- "r.Spearman_val"
all_gof_val$WaterLevel[30] <- round(all_gof_val$WaterLevel[2]/mean(
  wl_compare$obs_wl[wl_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$Temp[30] <- round(all_gof_val$Temp[2]/mean(
  watertemp$obs_temp[watertemp$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$DO[30] <- round(all_gof_val$DO[2]/mean(
  oxy_compare$obs_oxy[oxy_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$NH4[30] <- round(all_gof_val$NH4[2]/mean(
  nh4_compare$obs_nh4[nh4_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$NO3[30] <- round(all_gof_val$NO3[2]/mean(
  no3_compare$obs_no3[no3_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$PO4[30] <- round(all_gof_val$PO4[2]/mean(
  po4_compare$obs_po4[po4_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$Chla[30] <- round(all_gof_val$Chla[2]/mean(
  chla_compare$obs_chla[chla_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)

# Select GOF variables for the full year
full_n_all <- c("n_all", length(obs_wl$WaterLevel_m),
                length(obstemp$temp), length(obs_oxy$OXY_oxy),
                length(obs_nh4$NIT_amm), length(obs_no3$NIT_nit),
                length(obs_po4$PHS_frp), length(obs_chla$PHY_tchla))

full_n_cal <- c("n_cal",length(obs_wl$DateTime[which(obs_wl$DateTime < "2021-01-01")]),
                length(obstemp$DateTime[which(obstemp$DateTime < "2021-01-01")]),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime < "2021-01-01")]),
                length(obs_nh4$DateTime[which(obs_nh4$DateTime < "2021-01-01")]),
                length(obs_no3$DateTime[which(obs_no3$DateTime < "2021-01-01")]),
                length(obs_po4$DateTime[which(obs_po4$DateTime < "2021-01-01")]),
                length(obs_chla$DateTime[which(obs_chla$DateTime < "2021-01-01")]))

full_n_val <- c("n_val",length(obs_wl$DateTime[which(obs_wl$DateTime >= "2021-01-01")]),
                length(obstemp$DateTime[which(obstemp$DateTime >= "2021-01-01")]),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime >= "2021-01-01")]),
                length(obs_nh4$DateTime[which(obs_nh4$DateTime >= "2021-01-01")]),
                length(obs_no3$DateTime[which(obs_no3$DateTime >= "2021-01-01")]),
                length(obs_po4$DateTime[which(obs_po4$DateTime >= "2021-01-01")]),
                length(obs_chla$DateTime[which(obs_chla$DateTime >= "2021-01-01")]))

full_gof_all_table <- all_gof %>% 
  filter(Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

full_gof_cal_table <- all_gof_cal %>% 
  filter(Parameter == "R2_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NMAE_cal")

full_gof_val_table <- all_gof_val %>% 
  filter(Parameter == "R2_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NMAE_val")

full_gof_table <- rbind(full_n_all,full_gof_all_table,full_n_cal,full_gof_cal_table,full_n_val,full_gof_val_table)

write_csv(full_gof_table,'figures/table_gof_watercol_bvr_2015-2022.csv')

#-----------------------------------------------------------------------#
# Surface, full period (2015-2022)
all_gof <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","Temp"))
all_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","ubRMSE_all",
                       "NRMSE%_all","PBIAS%_all","RSR_all","rSD_all",
                       "NSE_all","mNSE_all","rNSE_all","wNSE_all",
                       "wsNSE_all","d_all","dr_all","md_all","rd_all",
                       "cp_all","r_all","R2_all","bR2_all","VE_all", #r2 and r are switched in the documentation order
                       "KGE_all","KGElf_all","KGEnp_all","KGEkm_all", 
                       "r.Spearman","nonparamR2") 

# Full water column, calibration period (2015-2020)
all_gof_cal <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","Temp"))
all_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","ubRMSE_cal",
                           "NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal",
                           "NSE_cal","mNSE_cal","rNSE_cal","wNSE_cal",
                           "wsNSE_cal","d_cal","dr_cal","md_cal","rd_cal",
                           "cp_cal","r_cal","R2_cal","bR2_cal","VE_cal",
                           "KGE_cal","KGElf_cal","KGEnp_cal","KGEkm_cal", 
                           "r.Spearman","nonparamR2") 

# Full water column, validation period (2021-2022)
all_gof_val <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","Temp"))
all_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","ubRMSE",
                           "NRMSE%_val","PBIAS%_val","RSR_val","rSD_val",
                           "NSE_val","mNSE_val","rNSE_val","wNSE_val",
                           "wsNSE","d_val","dr_val","md_val","rd_val",
                           "cp_val","r_val","R2_val","bR2_val","VE_val",
                           "KGE_val","KGElf_val","KGEnp_val","KGEkm_val",
                           "r.Spearman", "nonparamR2") 

# calculate all gof metrics for full period at surface + all vars
all_gof$Temp <- c(gof(watertemp$mod_temp[watertemp$Depth %in% 0.1],
                      watertemp$obs_temp[watertemp$Depth %in% 0.1],do.spearman = TRUE), NA)
all_gof$DO <- c(gof(oxy_compare$mod_oxy[oxy_compare$Depth %in% 0.1],
                    oxy_compare$obs_oxy[oxy_compare$Depth %in% 0.1],do.spearman = TRUE), NA)
all_gof$NH4 <- c(gof(nh4_compare$mod_nh4[nh4_compare$Depth %in% 0.1],
                     nh4_compare$obs_nh4[nh4_compare$Depth %in% 0.1],do.spearman = TRUE), NA)
all_gof$NO3 <- c(gof(no3_compare$mod_no3[no3_compare$Depth %in% 0.1],
                     no3_compare$obs_no3[no3_compare$Depth %in% 0.1],do.spearman = TRUE), NA)
all_gof$PO4 <- c(gof(po4_compare$mod_po4[po4_compare$Depth %in% 0.1],
                     po4_compare$obs_po4[po4_compare$Depth %in% 0.1],do.spearman = TRUE), NA)
all_gof$Chla <- c(gof(chla_compare$mod_chla[chla_compare$Depth %in% 0.1],
                      chla_compare$obs_chla[chla_compare$Depth %in% 0.1],do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_temp_rank <- watertemp |> 
  filter(Depth %in% 0.1) |>
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  filter(Depth %in% 0.1) |>
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_nh4_rank <- nh4_compare |>  
  filter(Depth %in% 0.1) |>
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  filter(Depth %in% 0.1) |>
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  filter(Depth %in% 0.1) |>
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_chla_rank <- chla_compare |> 
  filter(Depth %in% 0.1) |>
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof$DO[29] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof$NH4[29] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof$NO3[29] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof$PO4[29] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof$Chla[29] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

# same as above but for CALIBRATION period
all_gof_cal$Temp <- c(gof(watertemp$mod_temp[watertemp$DateTime< "2021-01-01" & 
                                               watertemp$Depth %in% 0.1],
                          watertemp$obs_temp[watertemp$DateTime< "2021-01-01" &
                                               watertemp$Depth %in% 0.1],
                          do.spearman = TRUE), NA)
all_gof_cal$DO <- c(gof(oxy_compare$mod_oxy[oxy_compare$DateTime< "2021-01-01" &
                                              oxy_compare$Depth %in% 0.1],
                        oxy_compare$obs_oxy[oxy_compare$DateTime< "2021-01-01" &
                                              oxy_compare$Depth %in% 0.1],
                        do.spearman = TRUE), NA)
all_gof_cal$NH4 <- c(gof(nh4_compare$mod_nh4[nh4_compare$DateTime< "2021-01-01" &
                                               nh4_compare$Depth %in% 0.1],
                         nh4_compare$obs_nh4[nh4_compare$DateTime< "2021-01-01" &
                                               nh4_compare$Depth %in% 0.1],
                         do.spearman = TRUE), NA)
all_gof_cal$NO3 <- c(gof(no3_compare$mod_no3[no3_compare$DateTime< "2021-01-01" &
                                               no3_compare$Depth %in% 0.1],
                         no3_compare$obs_no3[no3_compare$DateTime< "2021-01-01" &
                                               no3_compare$Depth %in% 0.1],
                         do.spearman = TRUE), NA)
all_gof_cal$PO4 <- c(gof(po4_compare$mod_po4[po4_compare$DateTime< "2021-01-01" &
                                               po4_compare$Depth %in% 0.1],
                         po4_compare$obs_po4[po4_compare$DateTime< "2021-01-01" &
                                               po4_compare$Depth %in% 0.1],
                         do.spearman = TRUE), NA)
all_gof_cal$Chla <- c(gof(chla_compare$mod_chla[chla_compare$DateTime< "2021-01-01" &
                                                  chla_compare$Depth %in% 0.1],
                          chla_compare$obs_chla[chla_compare$DateTime< "2021-01-01" &
                                                  chla_compare$Depth %in% 0.1],
                          do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_temp_rank <- watertemp |>  
  filter(DateTime < "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  filter(DateTime < "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_nh4_rank <- nh4_compare |> 
  filter(DateTime < "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  filter(DateTime < "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  filter(DateTime < "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_chla_rank <- chla_compare |> 
  filter(DateTime < "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof_cal$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_cal$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_cal$DO[29] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof_cal$NH4[29] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof_cal$NO3[29] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof_cal$PO4[29] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof_cal$Chla[29] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

# now VALIDATION period
all_gof_val$Temp <- c(gof(watertemp$mod_temp[watertemp$DateTime >= "2021-01-01" &
                                               watertemp$Depth %in% 0.1],
                          watertemp$obs_temp[watertemp$DateTime >= "2021-01-01" &
                                               watertemp$Depth %in% 0.1],
                          do.spearman = TRUE), NA)
all_gof_val$DO <- c(gof(oxy_compare$mod_oxy[oxy_compare$DateTime >= "2021-01-01" &
                                              oxy_compare$Depth %in% 0.1],
                        oxy_compare$obs_oxy[oxy_compare$DateTime >= "2021-01-01" &
                                              oxy_compare$Depth %in% 0.1],
                        do.spearman = TRUE), NA)
all_gof_val$NH4 <- c(gof(nh4_compare$mod_nh4[nh4_compare$DateTime >= "2021-01-01" &
                                               nh4_compare$Depth %in% 0.1],
                         nh4_compare$obs_nh4[nh4_compare$DateTime >= "2021-01-01" &
                                               nh4_compare$Depth %in% 0.1],
                         do.spearman = TRUE), NA)
all_gof_val$NO3 <- c(gof(no3_compare$mod_no3[no3_compare$DateTime >= "2021-01-01" &
                                               no3_compare$Depth %in% 0.1],
                         no3_compare$obs_no3[no3_compare$DateTime >= "2021-01-01" &
                                               no3_compare$Depth %in% 0.1],
                         do.spearman = TRUE), NA)
all_gof_val$PO4 <- c(gof(po4_compare$mod_po4[po4_compare$DateTime >= "2021-01-01" &
                                               po4_compare$Depth %in% 0.1],
                         po4_compare$obs_po4[po4_compare$DateTime >= "2021-01-01" &
                                               po4_compare$Depth %in% 0.1],
                         do.spearman = TRUE), NA)
all_gof_val$Chla <- c(gof(chla_compare$mod_chla[chla_compare$DateTime >= "2021-01-01" &
                                                  chla_compare$Depth %in% 0.1],
                          chla_compare$obs_chla[chla_compare$DateTime >= "2021-01-01" &
                                                  chla_compare$Depth %in% 0.1],
                          do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_temp_rank <- watertemp |>  
  filter(DateTime >= "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  filter(DateTime >= "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_nh4_rank <- nh4_compare |> 
  filter(DateTime >= "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  filter(DateTime >= "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  filter(DateTime >= "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_chla_rank <- chla_compare |> 
  filter(DateTime >= "2021-01-01",
         Depth %in% 0.1) |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof_val$Temp[29] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_val$DO[29] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof_val$NH4[29] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof_val$NO3[29] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof_val$PO4[29] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof_val$Chla[29] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

####Cleaning up table ####
## Add NMAE calculation for all parameters
# all_gof
all_gof[nrow(all_gof)+1,] <- NA
all_gof[30,1] <- "NMAE_all"
all_gof$Parameter[28] <- "r.Spearman_all"
all_gof$Temp[30] <- round(all_gof$Temp[2]/mean(watertemp$obs_temp[watertemp$Depth %in% 0.1], 
                                               na.rm=T),digits = 2)
all_gof$DO[30] <- round(all_gof$DO[2]/mean(oxy_compare$obs_oxy[oxy_compare$Depth %in% 0.1],
                                           na.rm=T),digits = 2)
all_gof$NH4[30] <- round(all_gof$NH4[2]/mean(nh4_compare$obs_nh4[nh4_compare$Depth %in% 0.1],
                                             na.rm=T),digits = 2)
all_gof$NO3[30] <- round(all_gof$NO3[2]/mean(no3_compare$obs_no3[no3_compare$Depth %in% 0.1],
                                             na.rm=T),digits = 2)
all_gof$PO4[30] <- round(all_gof$PO4[2]/mean(po4_compare$obs_po4[po4_compare$Depth %in% 0.1],
                                             na.rm=T),digits = 2)
all_gof$Chla[30] <- round(all_gof$Chla[2]/mean(chla_compare$obs_chla[chla_compare$Depth %in% 0.1],
                                               na.rm=T),digits = 2)

# all_gof_cal
all_gof_cal[nrow(all_gof_cal)+1,] <- NA
all_gof_cal[30,1] <- "NMAE_cal"
all_gof_cal$Parameter[28] <- "r.Spearman_cal"
all_gof_cal$Temp[30] <- round(all_gof_cal$Temp[2]/mean(
  watertemp$obs_temp[watertemp$DateTime < "2021-01-01" & 
                       watertemp$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_cal$DO[30] <- round(all_gof_cal$DO[2]/mean(
  oxy_compare$obs_oxy[oxy_compare$DateTime < "2021-01-01" & 
                        oxy_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_cal$NH4[30] <- round(all_gof_cal$NH4[2]/mean(
  nh4_compare$obs_nh4[nh4_compare$DateTime < "2021-01-01" &
                        nh4_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_cal$NO3[30] <- round(all_gof_cal$NO3[2]/mean(
  no3_compare$obs_no3[no3_compare$DateTime < "2021-01-01" &
                        no3_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_cal$PO4[30] <- round(all_gof_cal$PO4[2]/mean(
  po4_compare$obs_po4[po4_compare$DateTime < "2021-01-01" &
                        po4_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_cal$Chla[30] <- round(all_gof_cal$Chla[2]/mean(
  chla_compare$obs_chla[chla_compare$DateTime < "2021-01-01" &
                          chla_compare$Depth %in% 0.1], na.rm=T),digits = 2)

# all_gof_val
all_gof_val[nrow(all_gof_val)+1,] <- NA
all_gof_val[30,1] <- "NMAE_val"
all_gof_val$Parameter[28] <- "r.Spearman_val"
all_gof_val$Temp[30] <- round(all_gof_val$Temp[2]/mean(
  watertemp$obs_temp[watertemp$DateTime >= "2021-01-01" &
                       watertemp$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_val$DO[30] <- round(all_gof_val$DO[2]/mean(
  oxy_compare$obs_oxy[oxy_compare$DateTime >= "2021-01-01" &
                        oxy_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_val$NH4[30] <- round(all_gof_val$NH4[2]/mean(
  nh4_compare$obs_nh4[nh4_compare$DateTime >= "2021-01-01" &
                        nh4_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_val$NO3[30] <- round(all_gof_val$NO3[2]/mean(
  no3_compare$obs_no3[no3_compare$DateTime >= "2021-01-01" &
                        no3_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_val$PO4[30] <- round(all_gof_val$PO4[2]/mean(
  po4_compare$obs_po4[po4_compare$DateTime >= "2021-01-01" &
                        po4_compare$Depth %in% 0.1], na.rm=T),digits = 2)
all_gof_val$Chla[30] <- round(all_gof_val$Chla[2]/mean(
  chla_compare$obs_chla[chla_compare$DateTime >= "2021-01-01" &
                          chla_compare$Depth %in% 0.1], na.rm=T),digits = 2)

# Select GOF variables for the full year
full_n_all <- c("n_all", 
                length(obstemp$temp[obstemp$Depth %in% 0.1]), 
                length(obs_oxy$OXY_oxy[obs_oxy$Depth %in% 0.1]),
                length(obs_nh4$NIT_amm[obs_nh4$Depth %in% 0.1]),
                length(obs_no3$NIT_nit[obs_no3$Depth %in% 0.1]),
                length(obs_po4$PHS_frp[obs_po4$Depth %in% 0.1]),
                length(obs_chla$PHY_tchla[obs_chla$Depth %in% 0.1]))

full_n_cal <- c("n_cal",
                length(obstemp$DateTime[which(obstemp$DateTime < "2021-01-01" &
                                                obstemp$Depth %in% 0.1)]),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime < "2021-01-01" &
                                                obs_oxy$Depth %in% 0.1)]),
                length(obs_nh4$DateTime[which(obs_nh4$DateTime < "2021-01-01" &
                                                obs_nh4$Depth %in% 0.1)]),
                length(obs_no3$DateTime[which(obs_no3$DateTime < "2021-01-01" &
                                                obs_no3$Depth %in% 0.1)]),
                length(obs_po4$DateTime[which(obs_po4$DateTime < "2021-01-01" &
                                                obs_po4$Depth %in% 0.1)]),
                length(obs_chla$DateTime[which(obs_chla$DateTime < "2021-01-01" &
                                                 obs_chla$Depth %in% 0.1)]))

full_n_val <- c("n_val",
                length(obstemp$DateTime[which(obstemp$DateTime >= "2021-01-01" &
                                                obstemp$Depth %in% 0.1)]),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime >= "2021-01-01" &
                                                obs_oxy$Depth %in% 0.1)]),
                length(obs_nh4$DateTime[which(obs_nh4$DateTime >= "2021-01-01" &
                                                obs_nh4$Depth %in% 0.1)]),
                length(obs_no3$DateTime[which(obs_no3$DateTime >= "2021-01-01" &
                                                obs_no3$Depth %in% 0.1)]),
                length(obs_po4$DateTime[which(obs_po4$DateTime >= "2021-01-01" &
                                                obs_po4$Depth %in% 0.1)]),
                length(obs_chla$DateTime[which(obs_chla$DateTime >= "2021-01-01" &
                                                 obs_chla$Depth %in% 0.1)]))

full_gof_all_table <- all_gof %>% 
  filter(Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

full_gof_cal_table <- all_gof_cal %>% 
  filter(Parameter == "R2_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NMAE_cal")

full_gof_val_table <- all_gof_val %>% 
  filter(Parameter == "R2_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NMAE_val")

full_gof_table <- rbind(full_n_all,full_gof_all_table,full_n_cal,full_gof_cal_table,full_n_val,full_gof_val_table)

write_csv(full_gof_table,'figures/table_gof_surface_bvr_2015-2022.csv')

#-------------------------------------------------------------------------#
# GOF table for zoops

nc_file = paste0("sims/spinup/baseline/output/output.nc")  

#save zoop output
var="ZOO_cladoceran"
clad_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit()  

# Function to get zoop data for varying depths
get_zoops <- function(depths, nc_file, var) {
  lapply(depths, function(z) {
    if (z == 0) {
      glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface')
    } else {
      glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface') |>
        dplyr::select(-DateTime)
    }
  }) |> 
    dplyr::bind_cols()
  
}

# Define depth range and call the function
depths <- seq(0, 11, by = 0.5)
clad_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
clad <- clad_full_wc |> 
  dplyr::mutate(ZOO_cladoceran = rowSums(dplyr::across(where(is.numeric)),na.rm=TRUE)) |>
  dplyr::select(DateTime, ZOO_cladoceran) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |>
  dplyr::filter(DateTime >= "2015-07-08")

var="ZOO_copepod"
cope_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

cope_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
cope <- cope_full_wc |> 
  dplyr::mutate(ZOO_copepod = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
  dplyr::select(DateTime, ZOO_copepod) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |>
  dplyr::filter(DateTime >= "2015-07-08")


var="ZOO_rotifer"
rot_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

rot_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
rot <- rot_full_wc |> 
  dplyr::mutate(ZOO_rotifer = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
  dplyr::select(DateTime, ZOO_rotifer) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |>
  dplyr::filter(DateTime >= "2015-07-08")

# Full water column, full period (2015-2022)
zoop_gof <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","clad"))
zoop_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","ubRMSE_all",
                        "NRMSE%_all","PBIAS%_all","RSR_all","rSD_all",
                        "NSE_all","mNSE_all","rNSE_all","wNSE_all",
                        "wsNSE_all","d_all","dr_all","md_all","rd_all",
                        "cp_all","r_all","R2_all","bR2_all","VE_all",
                        "KGE_all","KGElf_all","KGEnp_all","KGEkm_all", 
                        "r.Spearman","nonparamR2") 

#combine obs and mod for each group
clad_final <- inner_join(clad, clad_obs, by = "DateTime") |>
  rename(clad_mod = ZOO_cladoceran.x, clad_obs = ZOO_cladoceran.y)
cope_final <- inner_join(cope, cope_obs, by = "DateTime") |>
  rename(cope_mod = ZOO_copepod.x, cope_obs = ZOO_copepod.y)
rot_final <- inner_join(rot, rot_obs, by = "DateTime") |>
  rename(rot_mod = ZOO_rotifer.x, rot_obs = ZOO_rotifer.y)

# calculate all gof metrics for full period + each zoop
zoop_gof$clad <- c(gof(clad_final$clad_mod, clad_final$clad_obs,do.spearman = TRUE), NA)
zoop_gof$cope <- c(gof(cope_final$cope_mod,cope_final$cope_obs,do.spearman = TRUE), NA)
zoop_gof$rot <- c(gof(rot_final$rot_mod,rot_final$rot_obs,do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_clad_rank <- clad_final |> 
  mutate(rank_obs = rank(clad_obs),
         rank_mod = rank(clad_mod)) 
comb_cope_rank <- cope_final |> 
  mutate(rank_obs = rank(cope_obs),
         rank_mod = rank(cope_mod)) 
comb_rot_rank <- rot_final |> 
  mutate(rank_obs = rank(rot_obs),
         rank_mod = rank(rot_mod)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
zoop_gof$clad[29] <- summary(lm(comb_clad_rank$rank_obs ~ comb_clad_rank$rank_mod))$r.squared
zoop_gof$cope[29] <- summary(lm(comb_cope_rank$rank_obs ~ comb_cope_rank$rank_mod))$r.squared
zoop_gof$rot[29] <- summary(lm(comb_rot_rank$rank_obs ~ comb_rot_rank$rank_mod))$r.squared

####Cleaning up table ####
## Add NMAE calculation for all parameters
zoop_gof[nrow(zoop_gof)+1,] <- NA
zoop_gof[30,1] <- "NMAE_all"
zoop_gof$Parameter[28] <- "r.Spearman_all"
zoop_gof$clad[30] <- round(zoop_gof$clad[2]/mean(clad_final$clad_obs, 
                                                 na.rm=T),digits = 2)
zoop_gof$cope[30] <- round(zoop_gof$cope[2]/mean(cope_final$cope_obs, 
                                                 na.rm=T),digits = 2)
zoop_gof$rot[30] <- round(zoop_gof$rot[2]/mean(rot_final$rot_obs, 
                                               na.rm=T),digits = 2)

# Select GOF variables
full_n_all <- c("n_all",length(na.omit(clad_final$clad_obs)), 
                length(na.omit(cope_final$cope_obs)),
                length(na.omit(rot_final$rot_obs)))


zoop_gof_all_table <- zoop_gof %>% 
  filter(Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

zoop_gof_table <- rbind(full_n_all,zoop_gof_all_table)

write_csv(zoop_gof_table,'figures/table_gof_bvr_zoops_2015-2022.csv')

# calculate total RMSE for all zoop groups for ms result text
sqrt(as.numeric(zoop_gof_table$clad[zoop_gof_table$Parameter=="RMSE_all"])^2 +
       as.numeric(zoop_gof_table$cope[zoop_gof_table$Parameter=="RMSE_all"])^2 +
       as.numeric(zoop_gof_table$rot[zoop_gof_table$Parameter=="RMSE_all"])^2 )* 12.011 / 1000 #to convert to mg/L


