# need to automate this and have the output save in separate folders instead of overwriting

pacman::p_load(ggplot2, dplyr, scales, NatParksPalettes, glmtools)

# change output location "output/cladoceran/output.nc
glm_nml_file <- file.path("./sims/spinup/baseline/glm3.nml")
glm_nml <- glmtools::read_nml(nml_file = glm_nml_file)
glm_nml <- glmtools::set_nml(glm_nml,
                             arg_name = "output::out_dir",
                             arg_val = paste0("output/",taxa[i]))
glmtools::write_nml(glm_nml, file = glm_nml_file)


taxa <- c("clads","copes","rots")
scenario <- c("baseline","plus1","plus5","plus10")

# walk through this to manually change which aed file glm uses
aed_nml_file <- file.path("./sims/spinup/baseline/aed/aed2_4zones.nml")
aed_nml <- glmtools::read_nml(nml_file = aed_nml_file)
aed_nml <- glmtools::set_nml(aed_nml,
  arg_name = "aed_zooplankton::dbase",
  arg_val = "aed/aed_zoop_pars_3groups_4Sep2024.csv")
  glmtools::write_nml(aed_nml, file = aed_nml_file)

  # run the model
  #sim_folder = "./sims/spinup/baseline"
  #GLM3r::run_glm(sim_folder)
  
  
for (i in 1:length(scenario)){
    
    nc_file = paste0("sims/spinup/",scenario[i],"/output/output.nc")  
                      
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


#combine into one df 
all_zoops <- purrr::reduce(list(clad, cope, rot), dplyr::full_join) 

all_zoops_obs <- purrr::reduce(list(clad_obs, cope_obs, rot_obs), dplyr::full_join) |> 
  dplyr::group_by(DateTime) |>
  dplyr::mutate(ZOO_total = sum(ZOO_cladoceran, ZOO_copepod, ZOO_rotifer)) |>
  tidyr::pivot_longer(cols = -c(DateTime), 
                      names_pattern = "(...)_(...*)$",
                      names_to = c("mod", "taxon")) |> 
  na.omit() |> 
  dplyr::mutate(DateTime = as.Date(DateTime)) |>
  dplyr::mutate(value = value * 12.011) |> # convert to ug/L
  dplyr::filter(value < 6000) # just to make the plot look better
  
#convert from wide to long for plotting
all_zoops_final <- all_zoops |> 
  dplyr::group_by(DateTime) |>
  dplyr::mutate(ZOO_total = sum(ZOO_cladoceran, ZOO_copepod, ZOO_rotifer)) |>
  tidyr::pivot_longer(cols = -c(DateTime), 
                      names_pattern = "(...)_(...*)$",
                      names_to = c("mod", "taxon")) |> 
  dplyr::mutate(daily_sum = sum(value),
                year = lubridate::year(DateTime),
                doy = lubridate::yday(DateTime)) |>
  dplyr::ungroup() |>
  dplyr::group_by(year) |>
  dplyr::mutate(annual_sum = sum(value)) |>
  dplyr::ungroup() |>
  dplyr::mutate(annual_prop = (daily_sum / annual_sum) * 100) |>
  na.omit() |>
  dplyr::mutate(value = value * 12.011) # convert to ug/L |>
  dplyr::mutate(scenario = sceanrio[i])
  
  #now create a dynamic df name
  assign(paste0("all_zoops_", scenario[i]), all_zoops_final)
  
  }

#now create a dynamic df name
#assign("all_zoops_rots_last", all_zoops_final)


grz <- glmtools::get_var(file=nc_file,var_name = 'ZOO_grz',z_out=1,
               reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

resp <- glmtools::get_var(file=nc_file,var_name = 'ZOO_resp',z_out=1,
                reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07")) |>
  mutate(ZOO_resp_1 = ZOO_resp_1 * -1)

mort <- glmtools::get_var(file=nc_file,var_name = 'ZOO_mort',z_out=1,
                reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07")) |> 
  mutate(ZOO_mort_1 = ZOO_mort_1 * -1)

#combine diagnostics into 1 df
diag_long <- bind_cols(grz, resp[!colnames(resp) %in% "DateTime"],
                       mort[!colnames(mort) %in% "DateTime"]) |> 
  rename(grz = ZOO_grz_1,
         resp = ZOO_resp_1,
         mort = ZOO_mort_1) |> 
  tidyr::pivot_longer(cols = grz:mort,
               names_to = "variable")

#assign("zoop_diag_rots_last", diag_long)

# reorder the 'taxon' factor levels
all_zoops_final$taxon <- factor(all_zoops_final$taxon, 
                                levels = c("total", "cladoceran", "copepod", "rotifer"))
all_zoops_obs$taxon <- factor(all_zoops_obs$taxon, 
                              levels = c("total", "cladoceran", "copepod", "rotifer"))


# plot zoops
ggplot() +
  geom_line(data=all_zoops_final, aes(DateTime, value)) + 
  geom_point(data=all_zoops_obs,
            aes(DateTime, value, color=taxon)) +
  theme_bw() + xlab("") + guides(color = "none") +
  facet_wrap(~taxon, scales = "free_y", nrow=2) +
  ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) +
  scale_color_manual(values = c("black","#084c61","#db504a","#e3b505"),
                     breaks = c("total","cladoceran","copepod","rotifer"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-10,-10,-10)),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_mod_vs_obs_copes_last.jpg", width=6, height=4)

ggplot() +
  geom_line(data=subset(all_zoops_final, !taxon %in% "total"),
            aes(DateTime, value, color=taxon)) + 
  theme_bw() + xlab("") + guides(color = "none") +
  ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) +
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"),
                     breaks = c("cladoceran","copepod","rotifer"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-10,-10,-10)),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_dynamics_copes_last.jpg", width=6, height=4)

# numbers for results text
mean(all_zoops_final$value)
sd(all_zoops_final$value)

mean(all_zoops_final$value[all_zoops_final$taxon=="cladoceran"])
sd(all_zoops_final$value[all_zoops_final$taxon=="cladoceran"])

mean(all_zoops_final$value[all_zoops_final$taxon=="copepod"])
sd(all_zoops_final$value[all_zoops_final$taxon=="copepod"])

mean(all_zoops_final$value[all_zoops_final$taxon=="rotifer"])
sd(all_zoops_final$value[all_zoops_final$taxon=="rotifer"])


ggplot(diag_long, aes(x = DateTime, y = value)) + 
  geom_area(aes(color = variable, fill = variable),
            position = "stack", stat="identity",
            linewidth=1) +
  scale_color_manual(values = NatParksPalettes::
                       natparks.pals("Volcanoes", 5,  direction = -1))+
  scale_fill_manual(values = NatParksPalettes::
                      natparks.pals("Volcanoes", 5, direction = -1))+
  ylab("diagnostics (mmolC/m3/day)")+ xlab("") +
  scale_x_datetime(expand = c(0,0),labels = 
                     date_format("%Y",tz="EST5EDT")) +
  #scale_y_continuous(expand = c(0,0), limits = c(-5,10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.88,0.83),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
ggsave("figures/BVR_stacked_zoop_diag_copes_last.jpg", width=5, height=4) 

zoop_diags <- 
  purrr::reduce(list(zoop_diag_clads_last |> mutate(taxon = "cladoceran"), 
                     zoop_diag_copes_last |> mutate(taxon = "copepod"), 
                     zoop_diag_rots_last |> mutate(taxon = "rotifer")), 
                     dplyr::full_join) |>
  mutate(variable = case_when(
      variable == "grz" ~ "grazing rate",
      variable == "mort" ~ "mortality rate",
      variable == "resp" ~ "respiration rate"),
      year = lubridate::year(DateTime),
      season = case_when(
        lubridate::month(DateTime) %in% c(12,1,2) ~ "winter",
        lubridate::month(DateTime) %in% c(3,4,5) ~ "spring",
        lubridate::month(DateTime) %in% c(6,7,8) ~ "summer",
        lubridate::month(DateTime) %in% c(9,10,11) ~ "fall")) |>
  filter(year %in% c(2016:2021))

ggplot(zoop_diags, 
              aes(x = as.factor(year), y = value, fill = taxon)) + 
  geom_boxplot() + theme_bw() +
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"),
                     breaks = c("cladoceran","copepod","rotifer"))+
  ylab("rate (mmolC/m3/day)")+ xlab("") +
  facet_wrap(~ paste(variable, season, sep = " - "), scales = "free_y", nrow=3) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        legend.margin = margin(c(-0,-10,-15,-10)),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/taxa_diag_yearly_boxplots_seasons.jpg", width=6, height=6)

zoop_diags_summary <- zoop_diags |>
  group_by(year = lubridate::year(DateTime), taxon, variable) |>
  summarize(mean_rate = mean(value, na.rm = TRUE), .groups = "drop")

ggplot(zoop_diags_summary, aes(x = year, y = mean_rate, color = taxon)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~variable, scales = "free_y", nrow = 3) +
  scale_color_manual(values = c("#084c61", "#db504a", "#e3b505"),
                     breaks = c("cladoceran", "copepod", "rotifer")) +
  theme_bw() +
  ylab("Mean Rate (mmolC/m3/day)") +
  xlab("") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        legend.margin = margin(c(-0,-10,-15,-10)),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/annual_zoop_diag.jpg", width=6, height=6)

#---------------------------------------------------------------------#
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
  filter(Parameter == "r.Spearman_all" | Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

zoop_gof_table <- rbind(full_n_all,zoop_gof_all_table)

write_csv(zoop_gof_table,'figures/table_gof_bvr_zoops_2015-2022.csv')

# calculate total RMSE for all zoop groups for ms result text
sqrt(as.numeric(zoop_gof_table$clad[zoop_gof_table$Parameter=="RMSE_all"])^2 +
as.numeric(zoop_gof_table$cope[zoop_gof_table$Parameter=="RMSE_all"])^2 +
as.numeric(zoop_gof_table$rot[zoop_gof_table$Parameter=="RMSE_all"])^2 )* 12.011 #to convert to ug/L

