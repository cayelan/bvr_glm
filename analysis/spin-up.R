# script to prep glm nml file and met/inflow files for 15-year spin-up

#load packages
pacman::p_load(glmtools)

# create a folder for each scenarios and populate with sim files
glm_files = list.files("./sims/baseline", full.names = TRUE)[1:3] 

#list of scenarios
scenario <- c("baseline","plus1","plus5","plus10")

for (i in 1:length(scenario)){
  subdirName <- paste0("./sims/spinup/",scenario[i])
  folder<-dir.create(subdirName)
  file.copy(from = glm_files, to = subdirName, recursive = TRUE)
  outputdirName <- paste0(subdirName,"/output")
  output_folder<-dir.create(outputdirName)
}

# assign names to scenarios
scenario_folder_names <- c("plus1","plus5","plus10")

# add corresponding degrees C to met Temp_C column for each scenario
temp_increments <- c(1,5,10)

for (i in 1:length(scenario_folder_names)){
  
  # get met data filepath and read in met data
  met_filepath <- paste0("./sims/",scenario_folder_names[i],"/inputs/met.csv")
  met <- read.csv(met_filepath)
  
  # add temp increments 
  met$AirTemp <- met$AirTemp + temp_increments[i]
  
  # write to file
  new_met_filepath <- paste0("./sims/spinup/",scenario_folder_names[i],"/inputs/met_",scenario_folder_names[i],".csv")
  write.csv(met, new_met_filepath, row.names = FALSE, quote = FALSE)
  
  # get inflow data filepath and read in data
  inflow_filepath <- paste0("./sims/",scenario_folder_names[i],
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
  new_inflow_filepath <- paste0("./sims/spinup/",scenario_folder_names[i],
                                "/inputs/inflow_",scenario_folder_names[i],".csv")
  write.csv(inflow, new_inflow_filepath, row.names = FALSE, quote = FALSE)
  
  # set nml to use scenario met data
  scenario_nml_file <- file.path(paste0("./sims/spinup/",scenario_folder_names[i],"/glm3.nml"))
  scenario_nml <- glmtools::read_nml(nml_file = scenario_nml_file)
  scenario_nml <- glmtools::set_nml(scenario_nml, arg_list =
                                      list("meteo_fl" = paste0("inputs/met_",scenario_folder_names[i],".csv"),
                                           "inflow_fl" = paste0("inputs/inflow_",scenario_folder_names[i],".csv")))
  glmtools::write_nml(scenario_nml, file = scenario_nml_file)
}

# Set start and end dates
start_date <- as.POSIXct("2000-07-07 20:00:00", tz = "UTC")
end_date <- as.POSIXct("2015-07-06 00:00:00", tz = "UTC")
total_hours <- as.numeric(difftime(end_date, start_date, units = "hours")) + 1
total_days <- ceiling(as.numeric(difftime(end_date, start_date, units = "days")) + 1)

# Iterate through each scenario
for (i in 1:length(scenario)) {
  # step 1: Set file paths and update start date in the .nml file
  scenario_nml_file <- file.path(paste0("./sims/", scenario[i], "/glm3.nml"))
  scenario_nml <- glmtools::read_nml(nml_file = scenario_nml_file)
  scenario_nml <- glmtools::set_nml(scenario_nml, arg_name = "start", 
                                    arg_val = format(start_date, "%Y-%m-%d %H:%M:%S"))
  glmtools::write_nml(scenario_nml, file = scenario_nml_file)
  
  # step 2: adjust the met files - adding random noise + cycling through met obs
  if(scenario[i]=="baseline"){
    met <- read.csv(paste0("sims/",scenario[i],"/inputs/met.csv")) |>
      dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M") )
  } else{
    met <- read.csv(paste0("sims/",scenario[i],"/inputs/met_",scenario[i],".csv")) |>
      dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M") )
  }
  
  #list of columns that will be adjusted
  cols <- names(met[,c(2:7)])
  
  #number of times to repeat based on hourly data
  num_repeats <- ceiling(21 / (nrow(met) / (365 * 24)))  
  
  set.seed(42)  # For reproducibility
  
  met_expanded <- do.call(rbind, replicate(num_repeats, met, simplify = FALSE)) 
  
  # Trim to the correct length and set time
  met_expanded <- met_expanded[1:total_hours, ]
  met_expanded$time <- seq(start_date, by = "hour", length.out = total_hours) 
  
  # Append original observations
  met_final <- rbind(met_expanded, met[met$time > end_date, ]) |>
    na.omit()
  
  # Save met file
  if(scenario[i]=="baseline"){
    write.csv(met_final, paste0("sims/spinup/",scenario[i],"/inputs/met.csv"),
              row.names = FALSE)
  } else{
    write.csv(met_final, paste0("sims/spinup/",scenario[i],"/inputs/met_",scenario[i],".csv"),
              row.names = FALSE)
  }
  
  # step 3: adjust the inflow file (same random noise and cycling approach)
  if(scenario[i]=="baseline"){
    inflow <- read.csv(paste0("sims/",scenario[i],"/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv")) |>
      dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%d") )
  } else{
    inflow <- read.csv(paste0("sims/",scenario[i],"/inputs/inflow_",scenario[i],".csv")) |>
      dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%d") )
  }
  
  #list of columns that will be adjusted
  cols <- names(inflow[,c(2:21)])

  #number of times to repeat based on hourly data
  num_repeats <- ceiling(21 / (nrow(inflow) / (365)))  
  
  set.seed(42)  # For reproducibility
  
  inflow_expanded <- do.call(rbind, replicate(num_repeats, inflow, simplify = FALSE))
  
  # Trim to the correct length and set time
  inflow_expanded <- inflow_expanded[1:total_days, ]
  inflow_expanded$time <- seq(start_date, by = "day", length.out = total_days)
  
  # Append original observations
  inflow_final <- rbind(inflow_expanded, inflow[inflow$time > end_date, ]) |>
    na.omit()
  
  # Save inflow file
  if(scenario[i]=="baseline"){
    write.csv(inflow_final, paste0("sims/spinup/",scenario[i],"/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv"),
              row.names = FALSE)
  } else{
    write.csv(inflow_final, paste0("sims/spinup/",scenario[i],"/inputs/inflow_",scenario[i],".csv"),
              row.names = FALSE)
  }
  
  # step 4: adjust outflow file (same as above)
  outflow <- read.csv(paste0("sims/",scenario[i],"/inputs/BVR_spillway_outflow_2015_2022_metInflow.csv")) |>
    dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%d") )
  
  #list of columns that will be adjusted
  cols <- names(outflow[2])

  #number of times to repeat based on hourly data
  num_repeats <- ceiling(21 / (nrow(outflow) / (365)))  
  
  set.seed(42)  # For reproducibility
  
  outflow_expanded <- do.call(rbind, replicate(num_repeats, outflow, simplify = FALSE))
  
  # Trim to the correct length and set time
  outflow_expanded <- outflow_expanded[1:total_days, ]
  outflow_expanded$time <- seq(start_date, by = "day", length.out = total_days)
  
  # Append original observations
  outflow_final <- rbind(outflow_expanded, outflow[inflow$time > end_date, ]) |>
    na.omit()
  
  # Save the file
  write.csv(outflow_final, paste0("sims/spinup/",scenario[i],"/inputs/BVR_spillway_outflow_2015_2022_metInflow.csv"),
            row.names = FALSE)
}

#-----------------------------------------------------------------------------#
# now run each of these scenarios and save output
# run and plot each scenario
for (i in 1:length(scenario)){
  
  # run the model
  sim_folder = paste0("./sims/spinup/",scenario[i])
  GLM3r::run_glm(sim_folder)
  
  # set nml file
  nc_file <- file.path(paste0("sims/spinup/",scenario[i],"/output/output.nc")) 
  
  # access and plot temperature
  current_temp <- glmtools::get_var(nc_file, var_name = "temp")
  p <- glmtools::plot_var(nc_file, var_name = "temp", reference = "surface", 
                          plot.title = scenario[i])
  plot_filename <- paste0("./figures/waterTemp_",scenario[i],".png")
  ggplot2::ggsave(p, filename = plot_filename, device = "png",
                  height = 6, width = 8, units = "in")
  
}

#-------------------------------------------------------------------------#
#quick plots of inflow temp to make sure above code is doing what I want it to
inflow_baseline <- read.csv("sims/spinup/baseline/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv")
inflow_plus1 <- read.csv("sims/spinup/plus1/inputs/inflow_plus1.csv")
inflow_plus5 <- read.csv("sims/spinup/plus5/inputs/inflow_plus5.csv")
inflow_plus10 <- read.csv("sims/spinup/plus10/inputs/inflow_plus10.csv")

plot(as.Date(inflow_baseline$time), inflow_baseline$TEMP, ylim = c(-15,39), 
     col = "#00603d", type="l")
points(as.Date(inflow_plus1$time), inflow_plus1$TEMP, 
       col = "#c6a000", type="l")
points(as.Date(inflow_plus5$time), inflow_plus5$TEMP, 
       col = "#c85b00", type="l")
points(as.Date(inflow_plus10$time), inflow_plus10$TEMP, 
       col = "#680000", type="l")
legend("bottom", legend=c("baseline", "plus1C","plus5C","plus10C"),
       col=c("#00603d","#c6a000","#c85b00","#680000"), 
       lty=1, cex=0.8, bty='n', horiz=T)

max(inflow_baseline$TEMP) # 29.8 (but mean is lower so okay I think)
max(inflow_plus1$TEMP) # 27.6
max(inflow_plus5$TEMP) # 32.1
max(inflow_plus10$TEMP) # 37.7

#now read in met
met_baseline <- read.csv("sims/spinup/baseline/inputs/met.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

met_plus1 <- read.csv("sims/spinup/plus1/inputs/met_plus1.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

met_plus5 <- read.csv("sims/spinup/plus5/inputs/met_plus5.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

met_plus10 <- read.csv("sims/spinup/plus10/inputs/met_plus10.csv") |>
  dplyr::select(time, AirTemp) |>
  dplyr::mutate(time = as.Date(time)) |>
  dplyr::group_by(time) |> 
  dplyr::summarise(mean_airtemp = mean(AirTemp)) |>
  dplyr::filter(time<= "2022-05-04")

plot(as.Date(met_baseline$time), met_baseline$mean_airtemp, 
     ylim = c(-15,39), col = "#00603d", type="l")
points(as.Date(met_plus1$time), met_plus1$mean_airtemp, 
       col = "#c6a000", type="l")
points(as.Date(met_plus5$time), met_plus5$mean_airtemp, 
       col = "#c85b00", type="l")
points(as.Date(met_plus10$time), met_plus10$mean_airtemp,
       col = "#680000", type="l")
legend("bottom", legend=c("baseline", "plus1C","plus5C","plus10C"),
       col=c("#00603d","#c6a000","#c85b00","#680000"), 
       lty=1, cex=0.8, bty='n', horiz=T)

max(met_baseline$mean_airtemp) # 28.7
max(met_plus1$mean_airtemp) # 29.7
max(met_plus5$mean_airtemp) # 33.7
max(met_plus10$mean_airtemp) # 38.7

#--------------------------------------------------------------------#
# calculate Schmidt stability
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

#download bvr bathy
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1, timeout = max(300, getOption("timeout"))))

bathymetry <- readr::read_csv(infile1, show_col_types = F)  |>
  dplyr::select(Reservoir, Depth_m, SA_m2) |>
  dplyr::filter(Reservoir == "BVR") |>
  dplyr::select(-Reservoir)

#initialize ss df
# Initialize ss matrix with dimensions based on the length of dates and scenarios
scenario <- c("baseline","plus1","plus5","plus10") 
num_days <- length(unique(current_temp$DateTime))
num_scenarios <- length(scenario)
unique_dates <- as.Date(unique(current_temp$DateTime))
ss <- matrix(NA, nrow=num_days, ncol=num_scenarios)

# Define a function to compute Schmidt stability for a single scenario
compute_schmidt_for_scenario <- function(i) {
  # Load and prepare temperature data once per scenario
  temp_data <- glmtools::get_var(paste0("sims/spinup/", scenario[i], "/output/output.nc"),
                                 "temp", reference = 'surface', z_out = depths) |>
    tidyr::pivot_longer(cols = starts_with("temp"), names_to = "Depth",
                        names_prefix = "temp_", values_to = "temp") |>
    dplyr::mutate(Depth = as.numeric(Depth), DateTime = as.Date(DateTime)) |>
    na.omit()
  
  # Initialize a vector to store Schmidt stability for each day in this scenario
  scenario_ss <- numeric(num_days)
  
  # Loop over each unique date
  for (j in 1:num_days) {
    current_date <- unique_dates[j]
    
    # Filter temperature data for the current date
    temp <- dplyr::filter(temp_data, DateTime == current_date)
    
    # Calculate Schmidt stability if data is available for this date
    if (nrow(temp) > 0) {
      scenario_ss[j] <- rLakeAnalyzer::schmidt.stability(
        wtr = temp$temp,
        depths = temp$Depth,
        bthA = bathymetry$SA_m2,
        bthD = bathymetry$Depth_m
      )
    } else {
      scenario_ss[j] <- NA  # Handle missing data
    }
  }
  return(scenario_ss)
}

# Use parallel processing for each scenario
num_cores <- parallel::detectCores() - 1  # Leave one core free
results <- parallel::mclapply(1:num_scenarios, compute_schmidt_for_scenario, mc.cores = num_cores)

# Combine results into the matrix
for (i in 1:num_scenarios) {
  ss[, i] <- results[[i]]
}

# Convert results to a data frame
ss_df <- as.data.frame(ss)
colnames(ss_df) <- scenario
ss_df$DateTime <- unique_dates

#convert from wide to long
ss_long <- ss_df |>
  tidyr::pivot_longer(cols = -c(DateTime), 
                      names_to = c("scenario"),
                      values_to = c("ss"))  |>
  dplyr::arrange()

#load ggplot
library(ggplot2)

#plot ss for each scenario
ggplot(ss_long, aes(x=DateTime, y=ss, color=scenario)) + geom_line() +
  theme_bw() + xlab("") + ylab("Schmidt stability") + ylim(c(-7,325)) +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(-10,-10,-10,-10),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/ss_scenarios.jpg", width=6, height=4)