# script to run glm and generate output.nc file
# Note - run spin-up.R first!

# install glmtools
devtools::install_github("rqthomas/glmtools", force = TRUE)
library(purrr)

#list of scenarios
scenario <- c("baseline","plus1","plus5","plus10")

# code to run and plot each scenario (output is saved in the sims folder so not necessary to rerun this)
#for (i in 1:length(scenario)){
#  
#  # run the model
#  sim_folder = paste0("./sims/spinup/",scenario[i])
#  GLM3r::run_glm(sim_folder)
#  
#  # set nml file
#  nc_file <- file.path(paste0("sims/spinup/",scenario[i],"/output/output.nc")) 
#  #nc_file <- file.path(paste0("sims/",scenario[i],"/output/output.nc")) 
#  
#  # access and plot temperature
#  current_temp <- glmtools::get_var(nc_file, var_name = "temp")
#  p <- glmtools::plot_var(nc_file, var_name = "temp", reference = "surface", 
#                          plot.title = scenario[i])
#  plot_filename <- paste0("./figures/waterTemp_",scenario[i],".png")
#  ggplot2::ggsave(p, filename = plot_filename, device = "png",
#                  height = 6, width = 8, units = "in")
#  
#}

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

#plot surf temp (Figure S7)
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

#save density output from all scenarios as a csv
#mod_dens_bl <- get_var("sims/spinup/baseline/output/output.nc", "dens")
#write.csv(mod_dens_bl, "analysis/data/mod_dens_bl.csv", row.names = F)

#mod_dens_plus1 <- get_var("sims/spinup/plus1/output/output.nc", "dens")
#write.csv(mod_dens_plus1, "analysis/data/mod_dens_plus1.csv", row.names = F)

#mod_dens_plus5 <- get_var("sims/spinup/plus5/output/output.nc", "dens")
#write.csv(mod_dens_plus5, "analysis/data/mod_dens_plus5.csv", row.names = F)

#mod_dens_plus10 <- get_var("sims/spinup/plus10/output/output.nc", "dens")
#write.csv(mod_dens_plus10, "analysis/data/mod_dens_plus10.csv", row.names = F)

#calculate stratification duration for each year/scenario using density
mod_dens_bl <- read.csv("analysis/data/mod_dens_bl.csv") |> 
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

mod_dens_plus1 <- read.csv("analysis/data/mod_dens_plus1.csv") |> 
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

mod_dens_plus5 <- read.csv("analysis/data/mod_dens_plus5.csv") |> 
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

mod_dens_plus10 <- read.csv("analysis/data/mod_dens_plus10.csv") |> 
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

# Figure S3
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