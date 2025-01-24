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


