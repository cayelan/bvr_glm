# script to prep glm nml file and met/inflow files for 15-year spin-up

#load packages
pacman::p_load(glmtools)

#list of scenarios
scenario <- c("baseline","plus1","plus5","plus10")

# step 1: adjust the nml file startdate for each scenario

for(i in 1:length(scenario)){
scenario_nml_file <- file.path(paste0("./sims/",scenario[i],"/glm3.nml"))
scenario_nml <- glmtools::read_nml(nml_file = scenario_nml_file)
scenario_nml <- glmtools::set_nml(scenario_nml, arg_name = "start", 
                                  arg_val = '2000-07-07 00:00:00')
glmtools::write_nml(scenario_nml, file = scenario_nml_file)

# step 2: adjust the met files - adding random noise + cycling through met obs

met <- read.csv(paste0("sims/",scenario[i],"/inputs/met.csv")) |>
  dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M") )

# Set start and end dates
start_date <- as.POSIXct(scenario_nml$time$start, tz="UTC")
end_date <- as.POSIXct(scenario_nml$time$stop, tz="UTC")
total_hours <- as.numeric(difftime(end_date, start_date, units = "hours")) + 1

#list of columns that will be adjusted
cols <- names(met[,c(2:7)])

# Calculate standard deviations for each column and store them in a named vector
sds <- sapply(met[cols], sd, na.rm = TRUE)

#number of times to repeat based on hourly data
num_repeats <- ceiling(21 / (nrow(met) / (365 * 24)))  

set.seed(42)  # For reproducibility

# Repeat and adjust data
met_expanded <- do.call(rbind, lapply(1:num_repeats, function(i) {
  met_copy <- met
  # Apply offsets based on each column's standard deviation
  for (col in cols) {
    offset <- rnorm(n = nrow(met), mean = 0, sd = sds[col])
    met_copy[[col]] <- met_copy[[col]] + offset
  }
  return(met_copy)
}))

# Trim to the exact number of desired hours
met_expanded <- met_expanded[1:total_hours, ]

# Set the Date column to range from start_date to end_date
met_expanded$time <- seq(start_date, by = "hour", length.out = total_hours)

# Save the file
write.csv(met_expanded, paste0("sims/",scenario[i],"/inputs/met.csv"),
          row.names = FALSE)

# step 3: 

}
