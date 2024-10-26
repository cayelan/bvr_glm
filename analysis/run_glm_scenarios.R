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
  temp_data <- glmtools::get_var(paste0("sims/", scenario[i], "/output/output.nc"),
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
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51"),
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
