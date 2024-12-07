# Zooplankton modeled + observed zoop figs
# 21 August 2024

#load packages
pacman::p_load(tidyverse)

zoops <- read.csv("field_data/field_zoops.csv") |> 
  pivot_longer(cols = -DateTime,
               names_to = "taxon") |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  na.omit() |> 
  mutate(year = year(DateTime)) |> 
  filter(value < 500)

#plot zoop data
ggplot(zoops, aes(DateTime, value, color = taxon)) +
  geom_point() + geom_line() + theme_bw() + xlab("") +
  ylab(expression("Biomass ("*mu*"g/L)")) +
  facet_wrap(~taxon, scales = "free_y", nrow = 3) +
  scale_color_manual("",values = c("#006d77","#83c5be","#e29578")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.19, 0.2),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_taxa_obs_2015-2022.jpg", width=4, height=3)

#------------------------------------------------------------------#
#zoop modeled vs. observed 

nc_file <- "sims/spinup/baseline/output/output.nc"

var="ZOO_cladoceran"

obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                     reference = 'surface') 
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                     reference = 'surface') |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                   reference = 'surface') |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                     reference = 'surface') |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                   reference = 'surface') |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                     reference = 'surface') |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                   reference = 'surface') |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                     reference = 'surface') |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                   reference = 'surface') |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                     reference = 'surface') |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                   reference = 'surface') |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                     reference = 'surface') |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                   reference = 'surface') |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                     reference = 'surface') |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                   reference = 'surface') |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                     reference = 'surface') |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                   reference = 'surface') |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                     reference = 'surface') |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                   reference = 'surface') |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                     reference = 'surface') |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                    reference = 'surface') |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                      reference = 'surface') |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                    reference = 'surface') |> select(-DateTime)

clads_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                           zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                           zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                           zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                           zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                           zoops_10,zoops_10.5,zoops_11)

#sum all depths
clads_full_wc <- clads_full_wc |> 
  mutate(ZOO_clad = rowSums(across(where(is.numeric)),na.rm=T)) 

mod<- clads_full_wc |>
  select(DateTime, ZOO_clad) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_cladoceran = ZOO_clad)

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime"))

#calculate RMSE
clad <- compare |> 
  rename(Mod_cladoceran = ZOO_cladoceran.x,
         Obs_cladoceran = ZOO_cladoceran.y)

var="ZOO_copepod"

obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) %>%
  na.omit()

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                     reference = 'surface')
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                     reference = 'surface') |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                   reference = 'surface') |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                     reference = 'surface') |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                   reference = 'surface') |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                     reference = 'surface') |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                   reference = 'surface') |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                     reference = 'surface') |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                   reference = 'surface') |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                     reference = 'surface') |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                   reference = 'surface') |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                     reference = 'surface') |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                   reference = 'surface') |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                     reference = 'surface') |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                   reference = 'surface') |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                     reference = 'surface') |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                   reference = 'surface') |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                     reference = 'surface') |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                   reference = 'surface') |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                     reference = 'surface') |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                    reference = 'surface') |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                      reference = 'surface') |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                    reference = 'surface') |> select(-DateTime)

cope_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                          zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                          zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                          zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                          zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                          zoops_10,zoops_10.5,zoops_11)

#sum all depths
cope_full_wc <- cope_full_wc |> 
  mutate(ZOO_cope = rowSums(across(where(is.numeric)),na.rm=T)) 

mod<- cope_full_wc |>
  select(DateTime, ZOO_cope) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_copepod = ZOO_cope)

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime"))

#calculate RMSE
cope <- compare |> 
  rename(Mod_copepod = ZOO_copepod.x,
         Obs_copepod = ZOO_copepod.y)


var="ZOO_rotifer"

obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) 

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                     reference = 'surface') 
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                     reference = 'surface') |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                   reference = 'surface') |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                     reference = 'surface') |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                   reference = 'surface') |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                     reference = 'surface') |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                   reference = 'surface') |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                     reference = 'surface') |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                   reference = 'surface') |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                     reference = 'surface') |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                   reference = 'surface') |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                     reference = 'surface') |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                   reference = 'surface') |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                     reference = 'surface') |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                   reference = 'surface') |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                     reference = 'surface') |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                   reference = 'surface') |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                     reference = 'surface') |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                   reference = 'surface') |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                     reference = 'surface') |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                    reference = 'surface') |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                      reference = 'surface') |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                    reference = 'surface') |> select(-DateTime)

rot_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                         zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                         zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                         zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                         zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                         zoops_10,zoops_10.5,zoops_11)

#sum all depths
rot_full_wc <- rot_full_wc |> 
  mutate(ZOO_rot = rowSums(across(where(is.numeric)),na.rm=T)) 

mod<- rot_full_wc |>
  select(DateTime, ZOO_rot) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_rotifer= ZOO_rot)

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime"))

#calculate RMSE
rot <- compare |> 
  rename(Mod_rotifer = ZOO_rotifer.x,
         Obs_rotifer = ZOO_rotifer.y)

#combine into one df
all_zoops <- reduce(list(clad, cope, rot), full_join)

#add col for calib vs. valid period (2020-12-31)
all_zoops$period <- ifelse(all_zoops$DateTime <= "2020-12-31",
                           "calib", "valid")

#convert from wide to long for plotting
all_zoops_final <- all_zoops |> 
  pivot_longer(cols = -c(DateTime,period), 
               names_pattern = "(...)_(...*)$",
               names_to = c("type", "taxon")) |> 
  na.omit() |> 
  filter(value< 500) #dropping two copepod outliers bc they drown out other values

#plot the zoops - CALIBRATION
ggplot() +
  geom_line(data=subset(all_zoops_final, type %in% "Mod" & 
                          period %in% "calib"),
            aes(DateTime, value, color = "modeled")) +
  geom_point(data=subset(all_zoops_final, type %in% "Obs" & 
                           period %in% "calib"),
             aes(DateTime, value, color="observed")) + 
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("black","red"),
                     guide = guide_legend(override.aes = list(
                       color = c("red","black"),
                       linetype = c("blank", "solid"),
                       shape = c(16, NA)))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.9, 0.17),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoops_calib_mod_vs_obs.jpg", width=6, height=6)

#plot the zoops - VALIDATION
ggplot() +
  geom_line(data=subset(all_zoops_final, type %in% "Mod" & 
                          period %in% "valid"),
            aes(DateTime, value, color = "modeled")) +
  geom_point(data=subset(all_zoops_final, type %in% "Obs" & 
                           period %in% "valid"),
             aes(DateTime, value, color="observed")) + 
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("black","red"),
                     guide = guide_legend(override.aes = list(
                       color = c("red","black"),
                       linetype = c("blank", "solid"),
                       shape = c(16, NA)))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.9, 0.17),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoops_valid_mod_vs_obs.jpg", width=6, height=6)

#-------------------------------------------------------------------#
# GOF table for zoops

# Full water column, full period (2015-2022)
zoop_gof <- setNames(data.frame(matrix(ncol=2,nrow=29)),c("Parameter","clad"))
zoop_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","ubRMSE_all",
                       "NRMSE%_all","PBIAS%_all","RSR_all","rSD_all",
                       "NSE_all","mNSE_all","rNSE_all","wNSE_all",
                       "wsNSE_all","d_all","dr_all","md_all","rd_all",
                       "cp_all","r_all","R2_all","bR2_all","VE_all",
                       "KGE_all","KGElf_all","KGEnp_all","KGEkm_all", 
                       "r.Spearman","nonparamR2") 

# calculate all gof metrics for full period + each zoop
zoop_gof$clad <- c(gof(clad$Mod_cladoceran,clad$Obs_cladoceran,do.spearman = TRUE), NA)
zoop_gof$cope <- c(gof(cope$Mod_copepod,cope$Obs_copepod,do.spearman = TRUE), NA)
zoop_gof$rot <- c(gof(rot$Mod_rotifer,rot$Obs_rotifer,do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_clad_rank <- clad |> 
  mutate(rank_obs = rank(Obs_cladoceran),
         rank_mod = rank(Mod_cladoceran)) 
comb_cope_rank <- cope |> 
  mutate(rank_obs = rank(Obs_copepod),
         rank_mod = rank(Mod_copepod)) 
comb_rot_rank <- rot |> 
  mutate(rank_obs = rank(Obs_rotifer),
         rank_mod = rank(Mod_rotifer)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
zoop_gof$clad[29] <- summary(lm(comb_clad_rank$rank_obs ~ comb_clad_rank$rank_mod))$r.squared
zoop_gof$cope[29] <- summary(lm(comb_cope_rank$rank_obs ~ comb_cope_rank$rank_mod))$r.squared
zoop_gof$rot[29] <- summary(lm(comb_rot_rank$rank_obs ~ comb_rot_rank$rank_mod))$r.squared

####Cleaning up table ####
## Add NMAE calculation for all parameters
zoop_gof[nrow(zoop_gof)+1,] <- NA
zoop_gof[30,1] <- "NMAE_all"
zoop_gof$Parameter[28] <- "r.Spearman_all"
zoop_gof$clad[30] <- round(zoop_gof$clad[2]/mean(clad$Obs_cladoceran, 
                                                 na.rm=T),digits = 2)
zoop_gof$cope[30] <- round(zoop_gof$cope[2]/mean(cope$Obs_copepod, 
                                                 na.rm=T),digits = 2)
zoop_gof$rot[30] <- round(zoop_gof$rot[2]/mean(rot$Obs_rotifer, 
                                               na.rm=T),digits = 2)

# Select GOF variables
full_n_all <- c("n_all",length(na.omit(clad$Obs_cladoceran)), 
                length(na.omit(cope$Obs_copepod)),
                length(na.omit(rot$Obs_rotifer)))


zoop_gof_all_table <- zoop_gof %>% 
  filter(Parameter == "r.Spearman_all" | Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

zoop_gof_table <- rbind(full_n_all,zoop_gof_all_table)

write_csv(zoop_gof_table,'figures/table_gof_bvr_zoops_2015-2022.csv')
