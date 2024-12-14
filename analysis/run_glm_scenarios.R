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
  ylab(expression("Surface Temperature ("*degree*C*")")) +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
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
