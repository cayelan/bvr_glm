# Script for generating BVR glm aed ms figures and tables
# 20 Aug 2024

#install hydroGOF
if (!require(devtools)) install.packages("devtools")
if (!require(pacman)) install.packages("pacman")
library(devtools)
install_github("hzambran/hydroTSM")
install_github("hzambran/hydroGOF")

devtools::install_github("rqthomas/glmtools", force = TRUE)
devtools::install_github("eliocamp/tagger")

#load packages
pacman::p_load(tidyverse,lubridate, hydroTSM,
               glmtools, ggtext, tagger, cowplot)

# create modeled vs. observed df for each variable
#focal depths
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
scenario <- c("baseline","plus1","plus5","plus10")
i <- 1
#for(i in 1:length(scenario)){
  
  nc_file = paste0("sims/spinup/",scenario[i],"/output/output.nc")  

# water level
obs_wl <- read_csv("./inputs/BVR_Daily_WaterLevel_Vol_2015_2022_interp.csv") |>
  mutate(DateTime = as.POSIXct(strptime(Date, "%Y-%m-%d", tz="EST")),
         Depth = "0.1") |>
  filter(Date > as.POSIXct("2015-07-08") & Date < as.POSIXct("2022-05-04")) |>
  select(-c(vol_m3,Date))
  
mod_wl <- get_surface_height(nc_file, ice.rm = TRUE, snow.rm = TRUE) |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime > as.POSIXct("2015-07-09"))  |>
  mutate(Depth = "0.1") |> # to get wl in the same df as 0.1m vars
  rename(mod_wl = surface_height) |>
  select(DateTime,Depth,mod_wl)
  
wl_compare <-mod_wl |>
  mutate(obs_wl = obs_wl$WaterLevel_m) |>
  select(DateTime,Depth,mod_wl,obs_wl)
  
# temp
obstemp<-read_csv('field_data/CleanedObsTemp.csv') |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07")

modtemp <- get_temp(nc_file, reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07")

watertemp<-merge(modtemp, obstemp, by=c("DateTime","Depth")) |> 
  rename(mod_temp = temp.x, obs_temp = temp.y)

# DO
obs_oxy<-read.csv('field_data/CleanedObsOxy.csv') |> 
  select(-OXY_sat) |>
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))|>
  filter(DateTime >= "2015-07-07")

mod_oxy <- get_var(nc_file, "OXY_oxy", reference='surface', z_out=depths) |> 
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy")  |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07")

oxy_compare <- merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) |> 
  rename(mod_oxy = OXY_oxy.x, obs_oxy = OXY_oxy.y)

# NH4
obs_nh4 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, NIT_amm) |>
  filter(DateTime >= "2015-07-07")

mod_nh4 <- get_var(nc_file, "NIT_amm", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("NIT_amm_"), names_to="Depth", names_prefix="NIT_amm_", values_to = "NIT_amm") |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))|>
  filter(DateTime >= "2015-07-07") 

nh4_compare<-merge(mod_nh4, obs_nh4, by=c("DateTime","Depth")) |> 
  rename(mod_nh4 = NIT_amm.x, obs_nh4 = NIT_amm.y)

# NO3
obs_no3 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, NIT_nit) |>
  filter(DateTime >= "2015-07-07")

mod_no3 <- get_var(nc_file, "NIT_nit", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("NIT_nit_"), names_to="Depth", names_prefix="NIT_nit_", values_to = "NIT_nit") |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07")

no3_compare<-merge(mod_no3, obs_no3, by=c("DateTime","Depth")) |> 
  rename(mod_no3 = NIT_nit.x, obs_no3 = NIT_nit.y)

# PO4
obs_po4 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, PHS_frp) |>
  filter(DateTime >= "2015-07-07")

mod_po4 <- get_var(nc_file, "PHS_frp", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("PHS_frp_"), names_to="Depth", names_prefix="PHS_frp_", values_to = "PHS_frp") |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07")

po4_compare<-merge(mod_po4, obs_po4, by=c("DateTime","Depth")) |> 
  rename(mod_po4 = PHS_frp.x, obs_po4 = PHS_frp.y)

# chl a
obs_chla <- read.csv('field_data/CleanedObsChla.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, PHY_tchla) |>
  filter(DateTime >= "2015-07-07")

#read mod chla - calculating this by hand bc something is wrong with the glm aed calc
cyano <- glmtools::get_var(file = nc_file, var_name = "PHY_cyano", z_out = depths) |>
  tidyr::pivot_longer(cols = starts_with("PHY_cyano"), 
                      names_to = "Depth",names_prefix = "PHY_cyano.elv_", 
                      values_to = "PHY_cyano") |>
  dplyr::mutate(DateTime = as.Date(DateTime)) |>
  dplyr::filter(DateTime >= "2015-07-08") |>
  select(DateTime, Depth, PHY_cyano)

green <- glmtools::get_var(file = nc_file, var_name = "PHY_green", z_out = depths) |>
  tidyr::pivot_longer(cols = starts_with("PHY_green"), 
                      names_to = "Depth",names_prefix = "PHY_green.elv_", 
                      values_to = "PHY_green") |>
  dplyr::mutate(DateTime = as.Date(DateTime)) |>
  dplyr::filter(DateTime >= "2015-07-08") |>
  select(DateTime, Depth, PHY_green)

diatom <- glmtools::get_var(file = nc_file, var_name = "PHY_diatom", z_out = depths) |>
  tidyr::pivot_longer(cols = starts_with("PHY_diatom"), 
   names_to = "Depth",names_prefix = "PHY_diatom.elv_", values_to = "PHY_diatom") |>
  dplyr::mutate(DateTime = as.Date(DateTime)) |>
  dplyr::filter(DateTime >= "2015-07-08") |>
  select(DateTime, Depth, PHY_diatom)

#combine into one df 
phytos <- purrr::reduce(list(cyano, green, diatom), dplyr::full_join) 

#convert from wide to long for plotting
mod_chla <- phytos |> 
  tidyr::pivot_longer(cols = -c(DateTime,Depth), 
                      names_pattern = "(...)_(...*)$",
                      names_to = c("mod", "taxon")) |> 
  group_by(DateTime,Depth) |>
  mutate(group_chl = ifelse(taxon=="cyano", (value * 12) / 80,
                            ifelse(taxon=="green", (value * 12) / 30,
                                   (value * 12) / 40))) |>
  summarise(PHY_tchla = sum(group_chl)) |>
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

chla_compare <- merge(mod_chla, obs_chla, by = c("DateTime","Depth")) |> 
  rename(mod_chla = PHY_tchla.x, obs_chla = PHY_tchla.y) 

# DOC
obs_docl <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, OGM_doc) |>
  filter(DateTime >= "2015-07-07")

mod_docl <- get_var(nc_file, "OGM_doc", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("OGM_doc_"), names_to="Depth", names_prefix="OGM_doc_", values_to = "OGM_doc") |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07")

docl_compare<-merge(mod_docl, obs_docl, by=c("DateTime","Depth")) |> 
  rename(mod_docl = OGM_doc.x, obs_docl = OGM_doc.y)

# DOCr
obs_docr <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, OGM_docr) |>
  filter(DateTime >= "2015-07-07")

mod_docr <- get_var(nc_file, "OGM_docr", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("OGM_docr_"), names_to="Depth", names_prefix="OGM_docr_", values_to = "OGM_docr") |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07")

docr_compare<-merge(mod_docr, obs_docr, by=c("DateTime","Depth")) |> 
  rename(mod_docr = OGM_docr.x, obs_docr = OGM_docr.y)

#-------------------------------------------------------------#
#merge all dfs
all_vars <- reduce(list(wl_compare, watertemp, oxy_compare, 
                        nh4_compare, no3_compare,
                        po4_compare, chla_compare, 
                        docl_compare, docr_compare), full_join) |>
  mutate(mod_oxy = mod_oxy * 32 / 1000) |> # convert to mg/L
  mutate(obs_oxy = obs_oxy * 32 / 1000) |> # convert to mg/L
  mutate(mod_nh4 = mod_nh4 * 18.04) |> # convert to ug/L
  mutate(obs_nh4 = obs_nh4 * 18.04) |> # convert to ug/L
  mutate(mod_no3 = mod_no3 * 62.00) |> # convert to ug/L
  mutate(obs_no3 = obs_no3 * 62.00) |> # convert to ug/L
  mutate(mod_po4 = mod_po4 * 94.9714) |> # convert to ug/L
  mutate(obs_po4 = obs_po4 * 94.9714) |>  # convert to ug/L
  group_by(DateTime, Depth) |>
  mutate(obs_din = sum(obs_no3,obs_nh4),
         mod_din = sum(mod_no3,mod_nh4),
         obs_doc = sum(obs_docl,obs_docr),
         mod_doc = sum(mod_docl,mod_docr))
  
mod_vars <- reduce(list(mod_wl, modtemp, mod_oxy,
                        mod_nh4, mod_no3, 
                        mod_po4, mod_chla, 
                        mod_docl, mod_docr), full_join) |>
  mutate(OXY_oxy = OXY_oxy * 32 / 1000) |> # convert to mg/L
  mutate(NIT_amm = NIT_amm * 18.04) |> # convert to ug/L
  mutate(NIT_nit = NIT_nit * 62.00) |> # convert to ug/L
  mutate(PHS_frp = PHS_frp * 94.9714) |> # convert to ug/L
  group_by(DateTime, Depth) |>
  rename('OGM_docl' = 'OGM_doc') |>
  mutate(NIT_din = sum(NIT_amm, NIT_nit),
         OGM_doc = sum(OGM_docl,OGM_docr))
  
#add col for calib vs. valid period (2020-12-31)
all_vars$period <- ifelse(all_vars$DateTime <= "2020-12-31",
                          "calib", "valid")

#convert from wide to long for plotting
all_vars_final <- all_vars |> 
  filter(Depth %in% c(0.1,9)) |> 
  select(-c(mod_nh4, obs_nh4, mod_din, obs_din, mod_docl, 
            obs_docl, mod_docr, obs_docr, mod_doc, obs_doc)) |>
  pivot_longer(cols = -c(DateTime,Depth,period), 
               names_pattern = "(...)_(...*)$",
               names_to = c("type", "var")) |> 
  na.omit()|>
  distinct() |>
  mutate(scenario = scenario[i])

assign(paste0("all_vars_final_", scenario[i]), all_vars_final)

mod_vars_final <- mod_vars |> 
  filter(Depth %in% c(0.1,9) &
         DateTime >= as.POSIXct("2015-07-07")) |> 
  rename("temp_temp" = "temp",
         "NIT_nh4" = "NIT_amm",
         "NIT_no3" = "NIT_nit",
         "PHS_po4" = "PHS_frp",
         "PHY_chla" = "PHY_tchla") |>
  select(-c(NIT_nh4, NIT_din, OGM_doc ,OGM_docl, OGM_docr)) |>
  pivot_longer(cols = -c(DateTime,Depth), 
               names_pattern = "(...)_(...*)$",
               names_to = c("type", "var")) |> 
  na.omit() |>
  mutate(scenario = scenario[i])
assign(paste0("mod_vars_final_", scenario[i]), mod_vars_final)
#}

# write modeled vars to file
#mod_vars <-  mget(c("mod_vars_final_baseline","mod_vars_final_plus1",
#                 "mod_vars_final_plus5", "mod_vars_final_plus10")) |>
#                   setNames(paste0(scenario)) |>
#                   bind_rows(.id = "scenario") |>
#                   relocate(scenario, .after = last_col()) |>
#             filter(DateTime >= as.POSIXct("2015-07-07")) |>
#             select(-type)
#write.csv(mod_vars, "./analysis/data/mod_vars.csv", row.names = F)


#-------------------------------------------------------------#
# Define the labels as expressions
labels <- c(
  expression("Water level (m" [] * ")"),
  expression("Water Temp (" * degree * "C)"),
  expression("DO (mg L" ^-1*")"),
  expression("NO" [3] * " (" * mu * " g L"^-1*")"),
  expression("DRP (" * mu * " g L"^{-1}*")"),
 # expression("DOC (" * mu * " g L"^{-1}*")"),
  expression("Chlorophyll " * italic(a) * " (" * mu * " g L"^{-1}*")")
)

# Ensure factor levels in the data are consistent with the expressions
variable_levels <- c("wl", "temp", "oxy", "no3", "po4", "chla") # Variable keys

all_vars_final_baseline <- all_vars_final_baseline |>
  ungroup() |>
  mutate(
    var = factor(var, levels = variable_levels),
    variable = factor(var, levels = variable_levels, labels = labels) 
  )

mod_vars_final_baseline <- mod_vars_final_baseline |>
  ungroup() |>
  mutate(
    var = factor(var, levels = variable_levels),
    variable = factor(var, levels = variable_levels, labels = labels)
  ) |>
  na.omit()

# Plot vars for 0.1m depth (Figure 1 a-f)
plot1 <- ggplot() + geom_line(
    data = subset(mod_vars_final_baseline, Depth %in% 0.1), 
    aes(DateTime, value, color = "modeled")) +
  geom_point(
    data = subset(all_vars_final_baseline, type %in% "obs" & Depth %in% 0.1), 
    aes(DateTime, value, color = "observed")) + 
  facet_wrap(~ variable, scales = "free_y", 
             nrow = 3, labeller = label_parsed) + 
  geom_vline(xintercept = as.POSIXct("2020-12-31"), linetype = "dashed") +
  theme_bw() + xlab("") + ylab("Value") +
  scale_color_manual(
    name = "",
    values = c("modeled" = "black", "observed" = "red"),
    labels = c("Modeled", "Observed"),
    guide = guide_legend(override.aes = list(
      linetype = c("solid", "blank"),
                       shape = c(NA, 16)))) +
  tag_facets() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.margin = margin(c(-10,-10,-15,-10)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        text = element_text(size = 10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, -1.3, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(fill = "white"),
        panel.spacing.y = unit(0, "lines"),  
        strip.background = element_blank())
#ggsave("figures/allvars_mod_vs_obs_0.1m_spinup.jpg", width=8, height=6)

# read in zoop data
zoop_obs <- read_csv("analysis/data/zoop_obs.csv")
zoop_scenarios <- read_csv("./analysis/data/zoop_scenarios.csv")

# plot zoops (Figure 1 g-j)
plot2 <- ggplot(data=subset(zoop_scenarios, scenario %in% "baseline")) +
  geom_line(aes(DateTime, value)) + 
  geom_point(data=zoop_obs,
             aes(DateTime, value, color=taxon)) +
  theme_bw() + xlab("") + guides(color = "none") +
  facet_wrap(~taxon, scales = "free_y", nrow=2,
             labeller = labeller(taxon = facet_labels)) +
  ylab(expression("Biomass (mg C L"^{-1}*")")) +
  geom_vline(xintercept = as.Date("2020-12-31"), linetype = "dashed") +
  scale_color_manual(values = c(rep("red",4)),
                     breaks = c("cladoceran","copepod","rotifer", "total"))+
  tag_facets(tag_pool = letters[7:10] ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(1, 0.1, 0, 0), "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))
#ggsave("figures/zoop_mod_vs_obs_copes_last.jpg", width=6, height=4)

# Combine the two plots (make sure to run plot2 from zoop_taxa_diagnostics.R)
combined_plot <- plot_grid(
  plot1, plot2, 
  ncol = 1,   # Number of columns
  align = "v" # Align plots vertically
)
#ggsave("figures/ms_fig1.jpg", width=8, height=6)

# plot vars for 9m (Figure S4)
ggplot() +
  geom_line(data = subset(mod_vars_final_baseline, Depth %in% 9), 
            aes(DateTime, value, color = "modeled")) +
  geom_point(data = subset(all_vars_final_baseline, 
                           type %in% "obs" & Depth %in% 9), 
             aes(DateTime, value, color = "observed")) + 
  facet_wrap(~ variable, scales = "free_y", nrow = 3,
             labeller = label_parsed) + 
  geom_vline(xintercept = as.POSIXct("2020-12-31"), linetype = "dashed") +
  theme_bw() + xlab("") +
  scale_color_manual(
    name = "",
    values = c("modeled" = "black", "observed" = "red"),
    labels = c("modeled" = "Modeled", "observed" = "Observed"),
    guide = guide_legend(override.aes = list(
      linetype = c("solid", "blank"),
      shape = c(NA, 16)))) +
  tag_facets() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.8, 0.2),
        text = element_text(size = 10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(fill = "white"),
        panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank())
#ggsave("figures/allvars_mod_vs_obs_9m.jpg", width=8, height=6)

# read in mod_vars
mod_vars <- read_csv("analysis/data/mod_vars.csv") 

# numbers for results text
mod_vars_bl <- mod_vars |>
  filter(scenario %in% "baseline") |>
  mutate(season = ifelse(month(DateTime) %in% c(6,7,8), "summer",
                         ifelse(month(DateTime) %in% c(12,1,2), "winter", "other")))

mean(mod_vars_bl$value[mod_vars_bl$var %in% "temp" & 
                         mod_vars_bl$Depth %in% "0.1" & 
                         mod_vars_bl$season %in%"summer"])
sd(mod_vars_bl$value[mod_vars_bl$var %in% "temp" & 
                       mod_vars_bl$Depth %in% "0.1" & 
                       mod_vars_bl$season %in%"summer"])

mean(mod_vars_bl$value[mod_vars_bl$var %in% "temp" & 
                                     mod_vars_bl$Depth %in% "0.1" & 
                                     mod_vars_bl$season %in%"winter"])
sd(mod_vars_bl$value[mod_vars_bl$var %in% "temp" & 
                       mod_vars_bl$Depth %in% "0.1" & 
                       mod_vars_bl$season %in%"winter"])

mean(mod_vars_bl$value[mod_vars_bl$var %in% "oxy" & 
                                     mod_vars_bl$Depth %in% "0.1" & 
                                     mod_vars_bl$season %in%"summer"])
sd(mod_vars_bl$value[mod_vars_bl$var %in% "oxy" & 
                                   mod_vars_bl$Depth %in% "0.1" & 
                                   mod_vars_bl$season %in%"summer"])

mean(mod_vars_bl$value[mod_vars_bl$var %in% "oxy" & 
                         mod_vars_bl$Depth %in% "0.1" & 
                         mod_vars_bl$season %in%"winter"])
sd(mod_vars_bl$value[mod_vars_bl$var %in% "oxy" & 
                       mod_vars_bl$Depth %in% "0.1" & 
                       mod_vars_bl$season %in%"winter"])

(mean(mod_vars_bl$value[mod_vars_bl$var=="no3" & 
                          mod_vars_bl$Depth==0.1 & 
                          mod_vars_bl$season %in%"winter"]) -
mean(mod_vars_bl$value[mod_vars_bl$var=="no3" & 
                                     mod_vars_bl$Depth==0.1 & 
                                     mod_vars_bl$season %in%"summer"])) /
  (mean(mod_vars_bl$value[mod_vars_bl$var=="no3" & 
                                        mod_vars_bl$Depth==0.1 & 
                                        mod_vars_bl$season %in%"winter"]) +
     mean(mod_vars_bl$value[mod_vars_bl$var=="no3" & 
                              mod_vars_bl$Depth==0.1 & 
                              mod_vars_bl$season %in%"summer"])) /2

mean(mod_vars_bl$value[mod_vars_bl$var=="no3" & 
                                     mod_vars_bl$Depth==0.1 ])
sd(mod_vars_bl$value[mod_vars_bl$var=="no3" & 
                                   mod_vars_bl$Depth==0.1 ])

mean(mod_vars_bl$value[mod_vars_bl$var=="po4" & 
                         mod_vars_bl$Depth==0.1 ])
sd(mod_vars_bl$value[mod_vars_bl$var=="po4" & 
                       mod_vars_bl$Depth==0.1 ])

mean(mod_vars_bl$value[mod_vars_bl$var=="chla" & 
                         mod_vars_bl$Depth==0.1 ])
sd(mod_vars_bl$value[mod_vars_bl$var=="chla" & 
                       mod_vars_bl$Depth==0.1 ])

(mean(mod_vars_bl$value[mod_vars_bl$var=="chla" & 
                                      mod_vars_bl$Depth==0.1 & 
                                      mod_vars_bl$season %in%"winter"]) -
    mean(mod_vars_bl$value[mod_vars_bl$var=="chla" & 
                             mod_vars_bl$Depth==0.1 & 
                             mod_vars_bl$season %in%"summer"])) /
  (mean(mod_vars_bl$value[mod_vars_bl$var=="chla" & 
                                        mod_vars_bl$Depth==0.1 & 
                                        mod_vars_bl$season %in%"winter"]) +
     mean(mod_vars_bl$value[mod_vars_bl$var=="chla" & 
                              mod_vars_bl$Depth==0.1 & 
                              mod_vars_bl$season %in%"summer"])) /2


#modeled vars from 2000-2022 for each scenario
#scenarios_df <- mget(c("mod_vars_final_baseline","mod_vars_final_plus1",
#                       "mod_vars_final_plus5", "mod_vars_final_plus10")) |>
#  setNames(paste0(scenario)) |>
#  bind_rows(.id = "scenario") |>
#  relocate(scenario, .after = last_col()) |>
#  select(-type)
#write.csv(scenarios_df, "./analysis/data/modeled_vars_scenarios.csv", row.names = F)
#-----------------------------------------------------------------------#
  # read in modeled state vars
  summer_mod_vars <- read.csv("./analysis/data/mod_vars.csv") |>
    mutate(year = lubridate::year(DateTime),
           month = lubridate::month(DateTime)) |>
    filter(month %in% c(5:9),
           !var %in% "wl") |>
    group_by(year, month, Depth, scenario, var) |>
    summarise(monthly_mean = mean(value)) 
  
  summer_zoop_scenarios <- read.csv("./analysis/data/zoop_scenarios.csv") |>
    mutate(month = lubridate::month(DateTime),
           Depth = 0.1) |> #just so total biomass plots w/ surf vars
    filter(month %in% c(5:9),
           taxon %in% "total") |>
    group_by(year, month, Depth, scenario, taxon) |>
    summarise(monthly_mean = mean(value)) |>
    rename(var = taxon)
  
  #combine zoops and wq vars
  summer_all_vars <- bind_rows(summer_mod_vars, summer_zoop_scenarios)
  
  #new labels list
  labels <- c(
    expression("Water temp (" * degree * "C)"),
    expression("DO (mg L" ^-1*")"),
    expression("NO" [3] * " (" * mu * " g L"^-1*")"),
    expression("DRP (" * mu * " g L"^{-1}*")"),
    expression("Phyto biomass (" * mu * " g L"^{-1}*")"),
    expression("Zoop biomass (mg L" ^-1*")")
  )
  
  # line plots for each taxa/scenario
  mean_summer_mod_vars <-  summer_all_vars |>
    group_by(var, year, scenario, Depth) |>
    summarise(mean_val = mean(monthly_mean)) |>
    ungroup() |>
    mutate(variable = factor(var, levels = unique(var)[c(5,3,2,4,1,6)],
                            labels = labels))
  
  mean_summer_mod_vars$var <- factor(mean_summer_mod_vars$var, 
                              levels = c("temp", "oxy", "no3" ,
                                         "po4", "chla", "total"))
  
  # Figure 3 boxplot of wq vars
  ggplot(data=subset(mean_summer_mod_vars,Depth==0.1 & 
                       !year %in% c("2015","2022")),
         aes(x = scenario, y = mean_val,  fill = scenario)) +
    geom_boxplot() + xlab("") +
    facet_wrap(~variable, scales = "free",
               labeller = label_parsed) + 
    theme_bw() + ylab("Summer mean value") +
    scale_fill_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    scale_x_discrete(limits = c("baseline", "plus1", "plus5", "plus10"),
                     labels = c(
                       "baseline" = "Baseline",
                       "plus1" = "Plus1",
                       "plus5" = "Plus5",
                       "plus10" = "Plus10")) +
    theme_bw() + guides(fill = "none") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          text = element_text(size=10), 
          axis.text.y = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.text.x = element_text(face = "bold",hjust = 0),
          strip.background.x = element_blank(),
          axis.title.y = element_text(size = 11),
          plot.margin = unit(c(0, 1, 0, 0), "cm"),
          legend.box.margin = margin(0,-10,-10,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
  #ggsave("figures/mod_vars_yearly_summer_0.1m.jpg", width=7, height=4) 

  # Figure S5 boxplots
  ggplot(data=subset(mean_summer_mod_vars,Depth==9 & 
                       !year %in% c("2015","2022")),
         aes(x = scenario, y = mean_val,  fill = scenario)) +
    geom_boxplot() + xlab("") +
    facet_wrap(~variable, scales = "free",
               labeller = label_parsed) + 
    theme_bw() + ylab("Summer mean value") +
    scale_fill_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    scale_x_discrete(limits = c("baseline", "plus1", "plus5", "plus10"),
                     labels = c(
                       "baseline" = "Baseline",
                       "plus1" = "Plus1",
                       "plus5" = "Plus5",
                       "plus10" = "Plus10")) +
    theme_bw() + guides(fill = "none") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          text = element_text(size=10), 
          axis.text.y = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.text.x = element_text(face = "bold",hjust = 0),
          strip.background.x = element_blank(),
          axis.title.y = element_text(size = 11),
          plot.margin = unit(c(0, 1, 0, 0), "cm"),
          legend.box.margin = margin(0,-10,-10,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
  #ggsave("figures/mod_vars_yearly_summer_9m.jpg", width=7, height=4) 
  
  # numbers for results text
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                       mean_summer_mod_vars$scenario=="baseline" &
                                       mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                              mean_summer_mod_vars$scenario=="baseline" &
                              mean_summer_mod_vars$Depth==0.1])
  
  diff(range(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                             mean_summer_mod_vars$scenario=="baseline" &
                                             mean_summer_mod_vars$Depth==0.1]))
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                       mean_summer_mod_vars$scenario=="plus1" &
                                       mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                              mean_summer_mod_vars$scenario=="plus1" &
                              mean_summer_mod_vars$Depth==0.1])
  
  diff(range(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                             mean_summer_mod_vars$scenario=="plus1" &
                                             mean_summer_mod_vars$Depth==0.1]))
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                mean_summer_mod_vars$scenario=="plus5" &
                                mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                     mean_summer_mod_vars$scenario=="plus5" &
                                     mean_summer_mod_vars$Depth==0.1])
  
  diff(range(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                      mean_summer_mod_vars$scenario=="plus5" &
                                      mean_summer_mod_vars$Depth==0.1]))
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                       mean_summer_mod_vars$scenario=="plus10" &
                                       mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                              mean_summer_mod_vars$scenario=="plus10" &
                              mean_summer_mod_vars$Depth==0.1])
  
  diff(range(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                             mean_summer_mod_vars$scenario=="plus10" &
                                             mean_summer_mod_vars$Depth==0.1]))

  #mean temp difff
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                       mean_summer_mod_vars$scenario=="plus10" &
                                       mean_summer_mod_vars$Depth==0.1]) - 
    mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                         mean_summer_mod_vars$scenario=="baseline" &
                                         mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                       mean_summer_mod_vars$scenario=="plus5" &
                                       mean_summer_mod_vars$Depth==0.1]) - 
    mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                  mean_summer_mod_vars$scenario=="baseline" &
                                  mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                       mean_summer_mod_vars$scenario=="plus1" &
                                       mean_summer_mod_vars$Depth==0.1]) - 
    mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="temp" &
                                         mean_summer_mod_vars$scenario=="baseline" &
                                         mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                mean_summer_mod_vars$scenario=="baseline" &
                                mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                     mean_summer_mod_vars$scenario=="baseline" &
                                     mean_summer_mod_vars$Depth==0.1])

  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                mean_summer_mod_vars$scenario=="plus1" &
                                mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                       mean_summer_mod_vars$scenario=="plus5" &
                                       mean_summer_mod_vars$Depth==0.1])
    
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                mean_summer_mod_vars$scenario=="plus10" &
                                mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                     mean_summer_mod_vars$scenario=="plus10" &
                                     mean_summer_mod_vars$Depth==0.1])
  
  #hypo oxy
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                       mean_summer_mod_vars$scenario=="baseline" &
                                       mean_summer_mod_vars$Depth==9])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                     mean_summer_mod_vars$scenario=="baseline" &
                                     mean_summer_mod_vars$Depth==9])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                       mean_summer_mod_vars$scenario=="plus10" &
                                       mean_summer_mod_vars$Depth==9])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="oxy" &
                                     mean_summer_mod_vars$scenario=="plus10" &
                                     mean_summer_mod_vars$Depth==9])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="no3" &
                                mean_summer_mod_vars$scenario=="baseline" &
                                mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="no3" &
                                       mean_summer_mod_vars$scenario=="plus1" &
                                       mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="no3" &
                              mean_summer_mod_vars$scenario=="plus1" &
                              mean_summer_mod_vars$Depth==0.1])
    
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="no3" &
                                       mean_summer_mod_vars$scenario=="plus5" &
                                       mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="no3" &
                                mean_summer_mod_vars$scenario=="plus10" &
                                mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="no3" &
                                     mean_summer_mod_vars$scenario=="plus10" &
                                     mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="po4" &
                                mean_summer_mod_vars$scenario=="baseline" &
                                mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="po4" &
                                       mean_summer_mod_vars$scenario=="plus1" &
                                       mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="po4" &
                              mean_summer_mod_vars$scenario=="plus1" &
                              mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="po4" &
                                       mean_summer_mod_vars$scenario=="plus5" &
                                       mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="po4" &
                                mean_summer_mod_vars$scenario=="plus10" &
                                mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="po4" &
                                     mean_summer_mod_vars$scenario=="plus10" &
                                     mean_summer_mod_vars$Depth==0.1])

  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="chla" &
                                mean_summer_mod_vars$scenario=="baseline" &
                                mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="chla" &
                                       mean_summer_mod_vars$scenario=="plus1" &
                                       mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="chla" &
                                       mean_summer_mod_vars$scenario=="plus5" &
                                       mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="chla" &
                              mean_summer_mod_vars$scenario=="plus5" &
                              mean_summer_mod_vars$Depth==0.1])
  
  mean(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="chla" &
                                       mean_summer_mod_vars$scenario=="plus10" &
                                       mean_summer_mod_vars$Depth==0.1])
  sd(mean_summer_mod_vars$mean_val[mean_summer_mod_vars$var=="chla" &
                              mean_summer_mod_vars$scenario=="plus10" &
                              mean_summer_mod_vars$Depth==0.1])

