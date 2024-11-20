# script to prep glm nml file and met/inflow files for 15-year spin-up

#load packages
pacman::p_load(glmtools, zoo)

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
start_date <- as.POSIXct("2000-07-08 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2022-05-04 00:00:00", tz = "UTC")
total_hours <- as.numeric(difftime(end_date, start_date, units = "hours")) + 1
total_days <- ceiling(as.numeric(difftime(end_date, start_date, units = "days")) + 1)

# Define the observation period for cycling
obs_start_year <- 2015
obs_end_year <- 2022
obs_years <- seq(obs_start_year, obs_end_year)

# Function to map simulation date to observation date
map_to_obs_date <- function(sim_date, obs, hourly = TRUE) {
  # Get simulation year, month, day, and time
  sim_year <- as.numeric(format(sim_date, "%Y"))
  month_day <- format(sim_date, "%m-%d")
  time <- if (hourly) format(sim_date, "%H:%M:%S") else "00:00:00"
  
  # Determine observation year within 2015-2022 range
  if (sim_year < 2015) {
    year_offset <- (sim_year - 2000) %% 8
    obs_year <- 2015 + year_offset
  } else {
    obs_year <- sim_year
  }
  
  # Adjust leap year dates
  if ((obs_year == 2016 || obs_year == 2020) && month_day == "02-29") {
    month_day <- "02-28"
  }
  
  # Construct observation date
  obs_date <- as.POSIXct(paste0(obs_year, "-", month_day, " ", time),
                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # If obs_date exceeds the final obs date (2022-05-04), adjust it
  if (!is.na(obs_date) && obs_date > as.POSIXct("2022-05-04 23:59:59", tz = "UTC")) {
    days_offset <- as.numeric(difftime(obs_date, as.POSIXct("2022-05-04", tz = "UTC"), units = "days"))
    obs_date <- as.POSIXct("2016-05-05", tz = "UTC") + days_offset * 86400
  }
  
  # Apply correction for 2015 dates up to mid-2015
  if (!is.na(obs_date) && obs_year == 2015 && obs_date >= as.POSIXct("2015-01-01", tz = "UTC") &&
      obs_date <= as.POSIXct("2015-07-06 23:00:00", tz = "UTC")) {
    obs_date <- as.POSIXct(paste0(2016, "-", month_day, " ", time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  
  # Check if obs_date is in obs data and return; if not, return NA
  if (!is.na(obs_date) && obs_date %in% obs$time) {
    return(obs_date)
  } else {
    if (hourly) {
      cat("No match for obs_date:", sim_date, "\n")
    }
    return(NA)
  }
}

# Iterate through each scenario
for (i in 1:length(scenario)) {
  
  # Step 1: Set file paths and update start date in the .nml file
  scenario_nml_file <- file.path(paste0("./sims/", scenario[i], "/glm3.nml"))
  scenario_nml <- glmtools::read_nml(nml_file = scenario_nml_file)
  scenario_nml <- glmtools::set_nml(scenario_nml, arg_name = "start", 
                                    arg_val = format(start_date, "%Y-%m-%d %H:%M:%S"))
  glmtools::write_nml(scenario_nml, file = scenario_nml_file)
  
  # Step 2: Adjust the met files
  met_file <- if (scenario[i] == "baseline") {
    paste0("sims/", scenario[i], "/inputs/met.csv")
  } else {
    paste0("sims/", scenario[i], "/inputs/met_", scenario[i], ".csv")
  }
  met <- read.csv(met_file) |> 
    dplyr::mutate(time = as.POSIXct(paste0(time, ":00"), 
                                    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>
    dplyr::filter(time >= as.POSIXct("2015-07-08"))
  
  # Generate the sequence of simulation dates
  expanded_times <- seq(start_date, by = "hour", length.out = total_hours)
  
  # Map simulation times to observation times
  mapped_times <- vapply(expanded_times, function(sim_date) {
    map_to_obs_date(sim_date, met)
  }, FUN.VALUE = as.POSIXct(NA, tz = "UTC"))
  
  mapped_times <- as.POSIXct(mapped_times, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Replace NA values with the previous valid datetime
  mapped_times <- na.locf(mapped_times, na.rm = FALSE)
  
  # Create the expanded met data frame
  expanded_met <- data.frame(
    time = expanded_times,
    obs_time = mapped_times
  )
  
  # Join with met based on obs_time to get corresponding observations
  expanded_met <- merge(expanded_met, met, by.x = "obs_time", 
                        by.y = "time", all.x = TRUE) |>
    dplyr::select(-obs_time)|>
    arrange(time)
  
  # Save met file
  if(scenario[i]=="baseline"){
    write.csv(expanded_met, paste0("sims/spinup/",scenario[i],"/inputs/met.csv"),
              row.names = FALSE)
  } else{
    write.csv(expanded_met, paste0("sims/spinup/",scenario[i],"/inputs/met_",scenario[i],".csv"),
              row.names = FALSE)
  }
  
  # Step 3: Adjust the inflow file similarly
  inflow_file <- if (scenario[i] == "baseline") {
    "sims/baseline/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv"
  } else {
    paste0("sims/", scenario[i], "/inputs/inflow_", scenario[i], ".csv")
  }
  inflow <- read.csv(inflow_file) |> 
    dplyr::mutate(time = as.POSIXct(
      paste(time,"00:00:00"),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  # Generate the sequence of simulation dates
  expanded_times <- seq(as.Date(start_date), by = "day", 
                        length.out = total_days)
  
  # Map simulation times to observation times
  mapped_days_inflow <- vapply(expanded_times, function(sim_date) {
    map_to_obs_date(sim_date, inflow, hourly = FALSE)
  }, FUN.VALUE = as.POSIXct(NA, tz = "UTC"))
  
  mapped_days_inflow <- as.POSIXct(mapped_days_inflow, format = "%Y-%m-%d", tz = "UTC")
  
  # Replace NA values with the previous valid datetime
  mapped_days_inflow <- na.locf(mapped_days_inflow, na.rm = FALSE)
  
  # Create the expanded inflow data frame
  expanded_inflow <- data.frame(
    time = expanded_times,
    obs_time = mapped_days_inflow
  )
  
  # Join with met based on obs_time to get corresponding observations
  expanded_inflow <- merge(expanded_inflow, inflow, by.x = "obs_time", 
                        by.y = "time", all.x = TRUE) |>
    dplyr::select(-obs_time) |>
    arrange(time)
  
  # Save inflow file
  if(scenario[i]=="baseline"){
    write.csv(expanded_inflow, paste0("sims/spinup/",scenario[i],"/inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv"),
              row.names = FALSE)
  } else{
    write.csv(expanded_inflow, paste0("sims/spinup/",scenario[i],"/inputs/inflow_",scenario[i],".csv"),
              row.names = FALSE)
  }
  
  # Step 4: Adjust the outflow file
  outflow_file <- "sims/baseline/inputs/BVR_spillway_outflow_2015_2022_metInflow.csv"
  outflow <- read.csv(outflow_file) |> 
    dplyr::mutate(time = as.POSIXct(
      paste(time,"00:00:00"),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  # Generate the sequence of simulation dates
  expanded_times <- seq(as.Date(start_date), by = "day", 
                        length.out = total_days)
  
  # Map simulation times to observation times
  mapped_days_outflow <- vapply(expanded_times, function(sim_date) {
    map_to_obs_date(sim_date, outflow, hourly = FALSE)
  }, FUN.VALUE = as.POSIXct(NA, tz = "UTC"))
  
  mapped_days_outflow <- as.POSIXct(mapped_days_outflow,
                                    format = "%Y-%m-%d", tz = "UTC")
  
  # Replace NA values with the previous valid datetime
  mapped_days_outflow <- na.locf(mapped_days_outflow, na.rm = FALSE)
  
  # Create the expanded met data frame
  expanded_outflow <- data.frame(
    time = expanded_times,
    obs_time = mapped_days_outflow
  )
  
  # Join with met based on obs_time to get corresponding observations
  expanded_outflow <- merge(expanded_outflow, outflow, by.x = "obs_time", 
                           by.y = "time", all.x = TRUE) |>
    dplyr::select(-obs_time) |>
    arrange(time)
  
  # Save the file
  write.csv(expanded_outflow, paste0("sims/spinup/",scenario[i],"/inputs/BVR_spillway_outflow_2015_2022_metInflow.csv"),
            row.names = FALSE)
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

max(met_baseline$mean_airtemp) # 28.5
max(met_plus1$mean_airtemp) # 29.5
max(met_plus5$mean_airtemp) # 33.5
max(met_plus10$mean_airtemp) # 38.5

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

#read in temp data
current_temp <- glmtools::get_var("sims/spinup/baseline/output/output.nc", 
                                  var_name = "temp")

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