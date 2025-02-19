# script to run glm and generate output.nc file
# Note - run spin-up.R first!

# install glmtools
devtools::install_github("rqthomas/glmtools", force = TRUE)
library(purrr)

#list of scenarios
scenario <- c("baseline","plus1","plus5","plus10")

# run and plot each scenario
for (i in 1:length(scenario)){
  
  # run the model
  sim_folder = paste0("./sims/spinup/",scenario[i])
  GLM3r::run_glm(sim_folder)
  
  # set nml file
  nc_file <- file.path(paste0("sims/spinup/",scenario[i],"/output/output.nc")) 
  #nc_file <- file.path(paste0("sims/",scenario[i],"/output/output.nc")) 
  
  # access and plot temperature
  current_temp <- glmtools::get_var(nc_file, var_name = "temp")
  p <- glmtools::plot_var(nc_file, var_name = "temp", reference = "surface", 
                          plot.title = scenario[i])
  plot_filename <- paste0("./figures/waterTemp_",scenario[i],".png")
  ggplot2::ggsave(p, filename = plot_filename, device = "png",
                  height = 6, width = 8, units = "in")
  
}

#----------------------------------------------------------------------#
# plot surface temp for each scenario

baseline <- read.csv("sims/spinup/baseline/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time)) |>
  dplyr::filter(DateTime >="2015-07-07") |>
  dplyr::mutate(scenario = "baseline")
plus1C <- read.csv("sims/spinup/plus1/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time))|>
  dplyr::filter(DateTime >="2015-07-07")|>
  dplyr::mutate(scenario = "plus1")
plus5C <- read.csv("sims/spinup/plus5/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time))|>
  dplyr::filter(DateTime >="2015-07-07")|>
  dplyr::mutate(scenario = "plus5")
plus10C <- read.csv("sims/spinup/plus10/output/lake.csv") |> 
  dplyr::mutate(DateTime = as.Date(time))|>
  dplyr::filter(DateTime >="2015-07-07")|>
  dplyr::mutate(scenario = "plus10")

all_scenarios_output <- reduce(list(baseline, plus1C, plus5C,plus10C), 
                               full_join) |>
  select(time, Surface.Temp, scenario) |>
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))

#plot surf temp
ggplot(all_scenarios_output, aes(time, Surface.Temp, color=as.factor(scenario))) +
  geom_line() + theme_bw() + xlab("") + 
  ylab(expression("Surface temperature ("*degree*C*")")) +
  scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        text = element_text(size=9), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-10,-10,-10)),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/surf_temp_scenarios.jpg", width=4, height=3)

# check on number of ice days
all_scenarios_ice <- reduce(list(baseline, plus1C, plus5C, plus10C), 
                               full_join) |>
  select(time, Vol.Blue.Ice, scenario) |>  
  mutate(date = as.Date(time)) |>
  filter(Vol.Blue.Ice > 0) |>
  group_by(scenario) |>
  summarise(ice_days = n_distinct(date))


# numbers for results text
mean(all_scenarios_output$Surface.Temp[
  all_scenarios_output$scenario=="baseline"])

mean(all_scenarios_output$Surface.Temp[
  all_scenarios_output$scenario=="plus1"])

mean(all_scenarios_output$Surface.Temp[
  all_scenarios_output$scenario=="plus5"])

mean(all_scenarios_output$Surface.Temp[
  all_scenarios_output$scenario=="plus10"])

mean(all_scenarios_output$Surface.Temp[
  all_scenarios_output$scenario=="plus10"]) -
  mean(all_scenarios_output$Surface.Temp[
    all_scenarios_output$scenario=="baseline"])

#plot pH
pH <- get_var(file=nc_file,var_name = 'CAR_pH',z_out=0.1,reference = 'surface') 
plot(pH$DateTime, pH$CAR_pH_0.1)

# thermocline depth

field_file<-file.path('field_data/CleanedObsTemp.csv')
baseline_thermo <- glmtools::compare_to_field(
  "sims/spinup/baseline/output/output.nc", field_file, 
  metric="thermo.depth", precision="days",
  method='interp',as_value=TRUE, na.rm=T)

plus1_thermo <- glmtools::compare_to_field(
  "sims/spinup/plus1/output/output.nc", field_file, 
  metric="thermo.depth", precision="days",
  method='interp',as_value=TRUE, na.rm=T)

plus5_thermo <- glmtools::compare_to_field(
  "sims/spinup/plus5/output/output.nc", field_file, 
  metric="thermo.depth", precision="days",
  method='interp',as_value=TRUE, na.rm=T)

plus10_thermo <- glmtools::compare_to_field(
  "sims/spinup/plus10/output/output.nc", field_file, 
  metric="thermo.depth", precision="days",
  method='interp',as_value=TRUE, na.rm=T)

plot(baseline_thermo$DateTime,baseline_thermo$mod, type="l",
     main = paste0("ThermoclineDepth: Obs=Red, Mod=Black"),
     ylab="Thermocline depth, in m", col = "#00603d", lwd=2)
lines(plus1_thermo$DateTime,plus1_thermo$mod, type="l", col = "#c6a000", lwd=2)
lines(plus5_thermo$DateTime,plus5_thermo$mod, type="l", col = "#c85b00", lwd=2)
lines(plus10_thermo$DateTime,plus10_thermo$mod, type="l", col = "#680000", lwd=2)

thermo_depth <- baseline_thermo |>
  rename("baseline" = "mod") |>
  mutate(plus1 = plus1_thermo$mod[plus1_thermo$DateTime %in% baseline_thermo$DateTime],
         plus5 = plus5_thermo$mod[plus5_thermo$DateTime %in% baseline_thermo$DateTime],
         plus10 = plus10_thermo$mod[plus10_thermo$DateTime %in% baseline_thermo$DateTime]) |>
  tidyr::pivot_longer(cols = -c(DateTime,obs),
                      names_to = "scenario",
                      values_to = "value")


thermo_depth |>
  mutate(year = lubridate::year(DateTime)) |>
  filter(year %in% 2016:2021) |>
  ggplot(aes(x = as.factor(year), y = value, 
             fill = factor(scenario, levels = 
                             c("baseline","plus1","plus5","plus10")))) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_y_reverse() +
  scale_fill_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                    breaks = c("baseline","plus1","plus5","plus10")) +
  theme_bw() + ylab("Thermocline depth (m)") + xlab("") +
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
#ggsave("figures/annual_boxplots_thermo_depth.jpg", width=7, height=4)

# numbers for results text
mean(thermo_depth$value[lubridate::year(thermo_depth$DateTime) %in% "2016" &
                          thermo_depth$scenario=="baseline"])
mean(thermo_depth$value[lubridate::year(thermo_depth$DateTime) %in% "2016" &
                          thermo_depth$scenario=="plus1"])
mean(thermo_depth$value[lubridate::year(thermo_depth$DateTime) %in% "2016" &
                          thermo_depth$scenario=="plus5"])
mean(thermo_depth$value[lubridate::year(thermo_depth$DateTime) %in% "2016" &
                          thermo_depth$scenario=="plus10"])

#calculate stratification duration for each year/scenario using density
mod_dens_bl <- get_var("sims/spinup/baseline/output/output.nc", "dens") |> 
  tidyr::pivot_longer(cols = starts_with('dens.elv_'),
                      names_to = 'depth',
                      values_to = 'density') |>
  dplyr::mutate(depth = as.numeric(gsub('dens.elv_', '', depth))) |>
  dplyr::filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31") |>
  dplyr::arrange(DateTime, depth) |>
  dplyr::group_by(DateTime) |>
  dplyr::summarise(density_top = first(density),  
                   density_bottom = last(density, na_rm = TRUE),
                   density_diff = density_top - density_bottom,
                   Stratified_binary = ifelse(abs(density_diff) > 0.1, 1, 0), 
                   .groups = 'drop') |>
  dplyr::select(DateTime, Stratified_binary) |>
  tidyr::pivot_longer(cols = Stratified_binary,
                      names_to = 'variable',
                      values_to = 'observation') |>
  dplyr::select(-variable) |>
  dplyr::mutate(scenario = "baseline")

mod_dens_plus1 <- get_var("sims/spinup/plus1/output/output.nc", "dens") |> 
  tidyr::pivot_longer(cols = starts_with('dens.elv_'),
                      names_to = 'depth',
                      values_to = 'density') |>
  dplyr::mutate(depth = as.numeric(gsub('dens.elv_', '', depth))) |>
  dplyr::filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31") |>
  dplyr::arrange(DateTime, depth) |>
  dplyr::group_by(DateTime) |>
  dplyr::summarise(density_top = first(density),  
                   density_bottom = last(density, na_rm = TRUE),
                   density_diff = density_top - density_bottom,
                   Stratified_binary = ifelse(abs(density_diff) > 0.1, 1, 0), 
                   .groups = 'drop') |>
  dplyr::select(DateTime, Stratified_binary) |>
  tidyr::pivot_longer(cols = Stratified_binary,
                      names_to = 'variable',
                      values_to = 'observation') |>
  dplyr::select(-variable) |>
  dplyr::mutate(scenario = "plus1")

mod_dens_plus5 <- get_var("sims/spinup/plus5/output/output.nc", "dens") |> 
  tidyr::pivot_longer(cols = starts_with('dens.elv_'),
                      names_to = 'depth',
                      values_to = 'density') |>
  dplyr::mutate(depth = as.numeric(gsub('dens.elv_', '', depth))) |>
  dplyr::filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31") |>
  dplyr::arrange(DateTime, depth) |>
  dplyr::group_by(DateTime) |>
  dplyr::summarise(density_top = first(density),  
                   density_bottom = last(density, na_rm = TRUE),
                   density_diff = density_top - density_bottom,
                   Stratified_binary = ifelse(abs(density_diff) > 0.1, 1, 0), 
                   .groups = 'drop') |>
  dplyr::select(DateTime, Stratified_binary) |>
  tidyr::pivot_longer(cols = Stratified_binary,
                      names_to = 'variable',
                      values_to = 'observation') |>
  dplyr::select(-variable) |>
  dplyr::mutate(scenario = "plus5")

mod_dens_plus10 <- get_var("sims/spinup/plus10/output/output.nc", "dens") |> 
  tidyr::pivot_longer(cols = starts_with('dens.elv_'),
                      names_to = 'depth',
                      values_to = 'density') |>
  dplyr::mutate(depth = as.numeric(gsub('dens.elv_', '', depth))) |>
  dplyr::filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31") |>
  dplyr::arrange(DateTime, depth) |>
  dplyr::group_by(DateTime) |>
  dplyr::summarise(density_top = first(density),  
                   density_bottom = last(density, na_rm = TRUE),
                   density_diff = density_top - density_bottom,
                   Stratified_binary = ifelse(abs(density_diff) > 0.1, 1, 0), 
                   .groups = 'drop') |>
  dplyr::select(DateTime, Stratified_binary) |>
  tidyr::pivot_longer(cols = Stratified_binary,
                      names_to = 'variable',
                      values_to = 'observation') |>
  dplyr::select(-variable) |>
  dplyr::mutate(scenario = "plus10")

scenario_dens <- dplyr::bind_rows(mod_dens_bl, mod_dens_plus1, 
                                  mod_dens_plus5, mod_dens_plus10) |>
  dplyr::mutate(year = lubridate::year(DateTime)) |>
  dplyr::group_by(year, scenario) |>
  dplyr::summarise(stratified_days = sum(observation)) |>
  dplyr::ungroup() 

mean(scenario_dens$stratified_days[scenario_dens$scenario=="baseline"]) # 299
mean(scenario_dens$stratified_days[scenario_dens$scenario=="plus1"])    # 300
mean(scenario_dens$stratified_days[scenario_dens$scenario=="plus5"])    # 310
mean(scenario_dens$stratified_days[scenario_dens$scenario=="plus10"])   # 339

ggplot(scenario_dens, 
       aes(x = factor(scenario, levels = c("baseline", "plus1", "plus5", "plus10")), 
           y = stratified_days, fill = scenario)) +
  geom_boxplot() +
  labs(x = "", y = "Stratified days") +
  theme_bw() + guides(fill="none") +
  scale_fill_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size=10), 
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 11),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.background = element_rect(
          fill = "white"))
#ggsave("figures/strat_days_scenario.jpg", width=7, height=4) 

# quick plot at % sat for all scenarios
for(i in 1:length(scenario)){
  
  nc_file = paste0("sims/spinup/",scenario[i],"/output/output.nc")  
  
psat <- get_var(nc_file, "OXY_sat", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("OXY_sat_"), names_to="Depth", names_prefix="OXY_sat_", values_to = "OXY_sat") |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |>
  filter(DateTime >= "2015-07-07") |>
  mutate(scenario = scenario[i]) |>
  filter(Depth %in% c(0.1,9))

assign(paste0("mod_psat_", scenario[i]), psat)

}

psat_scenarios <- mget(c("mod_psat_baseline","mod_psat_plus1",
                       "mod_psat_plus5", "mod_psat_plus10")) |>
                         setNames(paste0(scenario)) |>
                         bind_rows(.id = "scenario") |>
                         relocate(scenario, .after = last_col()) |>
                         filter(OXY_sat > 1)
                        


ggplot(data=subset(psat_scenarios, Depth %in% 0.1)) +
  geom_line(aes(x = as.POSIXct(DateTime), y = OXY_sat, 
                color = as.factor(scenario))) + xlab("") +
  scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10")) +
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
#ggsave("figures/modeled_psat_scenarios_0.1.jpg", width=7, height=4)

# quick plot of meteorology and inflow across years
met <- read_csv("sims/spinup/baseline/inputs/met.csv") |>
  mutate(DateTime = ymd_hms(time),  
         Year = year(DateTime)) |>
  filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31") |>
  group_by(Year) |>
  summarize(
    Mean_AirTemp = mean(AirTemp, na.rm = TRUE),  # Adjust column names as needed
    Total_Precip = sum(Rain, na.rm = TRUE),
    .groups = "drop"
  )

# Extract year
met_data <- met %>%
  mutate(DateTime = ymd_hms(time),  
         Year = factor(year(DateTime))) |> # Convert year to a factor for boxplots
  filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31")
  
# Plot Air Temperature Boxplot
ggplot(met, aes(x = Year, y = Mean_AirTemp)) +
  geom_point(fill = "#1f78b4", alpha = 0.7) +
  labs(title = "Annual Air Temperature Distribution",
       x = "Year", y = "Air Temperature (°C)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot Precipitation Boxplot
ggplot(met, aes(x = Year, y = Total_Precip)) +
  geom_point(fill = "#33a02c", alpha = 0.7) +
  labs(title = "Annual Precipitation Distribution",
       x = "Year", y = "Precipitation (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# inflow
inflow <- read_csv("sims/spinup/baseline/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv") |>
  mutate(DateTime = ymd(time),  
         Year = factor(year(DateTime))) |> # Convert year to a factor for boxplots
  filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31") |>
  group_by(Year) |>
  summarise(MeanFlow = mean(FLOW, na.rm = TRUE),
            MeanTemp = mean(TEMP, na.rm = TRUE),
            MeanOxy = mean(OXY_oxy, na.rm = TRUE),
            MeanAmm = mean(NIT_amm, na.rm = TRUE),
            MeanNit = mean(NIT_nit, na.rm = TRUE),
            MeanDOC = mean(OGM_doc, na.rm = TRUE)) |>
  pivot_longer(cols = c(MeanFlow, MeanTemp, MeanOxy, MeanAmm, MeanNit, MeanDOC), 
               names_to = "Variable", values_to = "Value")

ggplot(inflow, aes(x = Year, y = Value)) +
  geom_point(aes(fill = Variable), alpha = 0.7) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) + # Free scales for different units
  labs(title = "Annual Distribution of Inflow Variables",
       x = "Year", y = "Value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")

# look at secchi
secchi_obs <- read.csv("./field_data/field_secchi.csv")

nc_file <-  "sims/spinup/plus10/output/output.nc"

#plot Secchi depth & light extinction
lec <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface') |>
  filter(DateTime >= "2015-07-08")
plot(lec$DateTime, 1.7/lec$extc_1, , ylim=c(0,5), type="l")
points(as.POSIXct(secchi_obs$DateTime), secchi_obs$Secchi_m, 
       col="red", type="p", pch=16)
plot(lec$DateTime, lec$extc_1)
