#run baseline scenario
GLM3r::run_glm(sim_folder = "./sims/baseline")

# set nml file
nc_file <- file.path('sims/baseline/output/output.nc') 

# install glmtools
library(devtools)
devtools::install_github("rqthomas/glmtools", force = TRUE)

# access and plot temperature
current_temp <- glmtools::get_var(nc_file, var_name = "temp")
glmtools::plot_temp(nc_file, reference = "surface")

# assign names to scenarios
scenario_folder_names <- c("plus1","plus5","plus10")

# create a folder for each scenarios and populate with sim files
glm_files = list.files("./sims/baseline", full.names = TRUE)[1:3]

for (j in 1:length(scenario_folder_names)){
  subdirName <- paste0("./sims/",scenario_folder_names[j])
  folder<-dir.create(subdirName)
  file.copy(from = glm_files, to = subdirName, recursive = TRUE)
  outputdirName <- paste0(subdirName,"/output")
  output_folder<-dir.create(outputdirName)
}

# add corresponding degrees C to met Temp_C column for each scenario
# set temperature increments 
temp_increments <- c(1,5,10)

for (j in 1:length(scenario_folder_names)){
  
  # get met data filepath and read in met data
  met_filepath <- paste0("./sims/",scenario_folder_names[j],"/inputs/met.csv")
  met <- read.csv(met_filepath)
  
  # add temp increments 
  met$AirTemp <- met$AirTemp + temp_increments[j]
  
  # write to file
  new_met_filepath <- paste0("./sims/",scenario_folder_names[j],"/inputs/met_",scenario_folder_names[j],".csv")
  write.csv(met, new_met_filepath, row.names = FALSE, quote = FALSE)
  
  # get inflow data filepath and read in met data
  inflow_filepath <- paste0("./sims/",scenario_folder_names[j],
                            "/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv")
  inflow <- read.csv(inflow_filepath)
  
  # calculate bvr inflow temp based on met air temp
  # step 1: get daily avg met for entire sim period
  met_sub <- met |> dplyr::select(time, AirTemp) |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::group_by(time) |> 
    dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
    dplyr::filter(time<= "2022-05-04")
 
  # step 3: calculate fcr water temp using: weir temp = (0.75 * air temp) + 2.4
  fcr_inflow_temp <- (0.75 * met_sub$mean_airtemp) + 2.4
  
  # step 4: calculate bvr inflow temp using: BVR temp = (1.5 * FCR temp) - 9.21
  inflow$TEMP <- (1.5 * fcr_inflow_temp) - 9.21
  
  # write to file
  new_inflow_filepath <- paste0("./sims/",scenario_folder_names[j],
                             "/inputs/inflow_",scenario_folder_names[j],".csv")
  write.csv(inflow, new_inflow_filepath, row.names = FALSE, quote = FALSE)
  
  # set nml to use scenario met data
  scenario_nml_file <- file.path(paste0("./sims/",scenario_folder_names[j],"/glm3.nml"))
  scenario_nml <- glmtools::read_nml(nml_file = scenario_nml_file)
  scenario_nml <- glmtools::set_nml(scenario_nml, arg_list =
                                    list("meteo_fl" = paste0("inputs/met_",scenario_folder_names[j],".csv"),
                                    "inflow_fl" = paste0("inputs/inflow_",scenario_folder_names[j],".csv")))
  glmtools::write_nml(scenario_nml, file = scenario_nml_file)
}


# run and plot each scenario

for (j in 1:length(scenario_folder_names)){
  
  # run the model
  sim_folder = paste0("./sims/",scenario_folder_names[j])
  GLM3r::run_glm(sim_folder)
  
  # set nml file
  nc_file <- file.path(paste0("sims/",scenario_folder_names[j],"/output/output.nc")) 
  
  # access and plot temperature
  current_temp <- glmtools::get_var(nc_file, var_name = "temp")
  p <- glmtools::plot_var(nc_file, var_name = "temp", reference = "surface", 
                          plot.title = scenario_folder_names[j])
  plot_filename <- paste0("./figures/waterTemp_",scenario_folder_names[j],".png")
  ggplot2::ggsave(p, filename = plot_filename, device = "png",
                  height = 6, width = 8, units = "in")
  
}

#-------------------------------------------------------------------------#
#quick plots of inflow temp to make sure above code is doing what I want it to
inflow_baseline <- read.csv("sims/baseline/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv")
inflow_plus1 <- read.csv("sims/plus1/inputs/inflow_plus1.csv")
inflow_plus5 <- read.csv("sims/plus5/inputs/inflow_plus5.csv")
inflow_plus10 <- read.csv("sims/plus10/inputs/inflow_plus10.csv")

plot(as.Date(inflow_baseline$time), inflow_baseline$TEMP, ylim = c(-15,40), type="l")
points(as.Date(inflow_plus1$time), inflow_plus1$TEMP, col = "#F4E285", type="l")
points(as.Date(inflow_plus5$time), inflow_plus5$TEMP, col = "#F4A259", type="l")
points(as.Date(inflow_plus10$time), inflow_plus10$TEMP, col = "#BC4B51", type="l")
legend("top", legend=c("baseline", "plus1C","plus3C","plus5C"),
       col=c("black", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
max(inflow_plus1$TEMP) # 27.6
max(inflow_plus5$TEMP) # 32.1
max(inflow_plus10$TEMP) # 37.8

#now read in met
met_baseline <- read.csv("sims/baseline/inputs/met.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

met_plus1 <- read.csv("sims/plus1/inputs/met_plus1.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

met_plus5 <- read.csv("sims/plus5/inputs/met_plus5.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

met_plus10 <- read.csv("sims/plus10/inputs/met_plus10.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

plot(as.Date(met_baseline$time), met_baseline$mean_airtemp, ylim = c(-15,40), type="l")
points(as.Date(met_plus1$time), met_plus1$mean_airtemp, col = "#F4E285", type="l")
points(as.Date(met_plus5$time), met_plus5$mean_airtemp, col = "#F4A259", type="l")
points(as.Date(met_plus10$time), met_plus10$mean_airtemp, col = "#BC4B51", type="l")
legend("top", legend=c("baseline", "plus1C","plus3C","plus5C"),
       col=c("black", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

max(met_plus1$mean_airtemp) # 29.5
max(met_plus5$mean_airtemp) # 33.5
max(met_plus10$mean_airtemp) # 38.5
