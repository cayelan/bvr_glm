# need to automate this and have the output save in separate folders instead of overwriting

pacman::p_load(ggplot2, dplyr, scales, NatParksPalettes, 
               glmtools, tagger, cowplot, tidyr)

# zoop diags for all taxa and scenarios
taxa <- c("copes","clads","rots")

for(i in 1:length(scenario)){
for(j in 1:length(taxa)){

# change output location "output/cladoceran/output.nc
glm_nml_file <- file.path(paste0("./sims/spinup/",scenario[i],"/glm3.nml"))
glm_nml <- glmtools::read_nml(nml_file = glm_nml_file)
glm_nml <- glmtools::set_nml(glm_nml,
                             arg_name = "output::out_dir",
                             arg_val = paste0("output/",taxa[j]))
glmtools::write_nml(glm_nml, file = glm_nml_file)

zoop_files <- c("aed/aed_zoop_pars_3groups_4Sep2024.csv",
                "aed/aed_zoop_pars_3groups_4Sep2024_clads.csv",
                "aed/aed_zoop_pars_3groups_4Sep2024_rots.csv")

# walk through this to manually change which aed file glm uses
aed_nml_file <- file.path(paste0("./sims/spinup/",scenario[i],"/aed/aed2_4zones.nml"))
aed_nml <- glmtools::read_nml(nml_file = aed_nml_file)
aed_nml <- glmtools::set_nml(aed_nml,
                             arg_name = "aed_zooplankton::dbase",
                             arg_val = zoop_files[j])
glmtools::write_nml(aed_nml, file = aed_nml_file)

# run the model
sim_folder = paste0("./sims/spinup/",scenario[i])
GLM3r::run_glm(sim_folder)
  
}
}

library(ncdf4)
library(dplyr)

# Specify the zip file path
zip_file <- "./sims/spinup/baseline/output/baseline_taxa.zip"
zip_file <- "./sims/spinup/plus1/output/plus1_taxa.zip"
zip_file <- "./sims/spinup/plus5/output/plus5_taxa.zip"
zip_file <- "./sims/spinup/plus10/output/plus10_taxa.zip"

# Create a temporary directory to extract the files
temp_dir <- tempdir()
unzip(zip_file, exdir = temp_dir)

# Function to extract data from an .nc file
read_nc_data <- function(nc_file) {
  nc <- nc_open(nc_file)
  
  # Extract variables, assuming the 500 is depth and 7969 is time
  time <- ncvar_get(nc, "time")
  grz <- ncvar_get(nc, "ZOO_grz") 
  resp <- ncvar_get(nc, "ZOO_resp")
  mort <- ncvar_get(nc, "ZOO_mort")
  
  origin_date <- as.Date("2000-07-08")  
  time_in_days <- time / 24  # Convert time to days
  
  # Calculate the actual dates by adding the number of days to the origin
  dates <- origin_date + time_in_days
  
  # Convert to POSIXct (if you need precise time information, e.g., midnight each day)
  time <- as.POSIXct(dates, tz = "UTC")
  
  # Flatten the variables (use only time dimension)
  # Assuming that depth is the first dimension (500) and time is the second (7969)
  grz <- as.vector(grz[1, ])  # Select the first depth slice for grazing
  resp <- as.vector(resp[1, ])  # Select the first depth slice for respiration
  mort <- as.vector(mort[1, ])  # Select the first depth slice for mortality
  
  # Close the NetCDF file
  nc_close(nc)
  
  # Extract taxon name from file path (assuming the folder name is the taxon)
  taxon_name <- basename(dirname(nc_file))  # Extract folder name as taxon
  
  # Combine into a data frame
  diags <- data.frame(
    time = time,
    grz = grz,
    resp = resp,
    mort = mort,
    taxon = taxon_name, 
    file = basename(nc_file)  
  )
  
  return(diags) 
}

# List all .nc files in the extracted folders
nc_files <- list.files(temp_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)

# Initialize individual taxa data frames
clads <- NULL
copes <- NULL
rots <- NULL

# Loop through all .nc files and read data for each taxon
for (file in nc_files) {
  taxon_name <- basename(dirname(file))  # Get the folder name (taxon)
  
  # Read data only for a specific taxon
  if (taxon_name == "clads") {
    clads <- read_nc_data(file)
  } else if (taxon_name == "copes") {
    copes <- read_nc_data(file)
  } else if (taxon_name == "rots") {
    rots <- read_nc_data(file)
  }
}

# Combine all taxa data frames into one
plus5_diags <- bind_rows(clads, copes, rots)|>
  mutate(mort = mort * -1,
         resp = resp * -1) |>
  pivot_longer(cols = c(grz, resp, mort),  
               names_to = "diag",         
               values_to = "value") |>
  select(-file) |>
  mutate(scenario = "baseline")

#----------------------------------------------------------------------#
# write modeled vars to file
taxa_diags_scenarios <-  mget(c("baseline_diags","plus1_diags",
                 "plus5_diags", "plus10_diags")) |>
                   setNames(paste0(scenario)) |>
                   bind_rows(.id = "scenario") |>
                   relocate(scenario, .after = last_col()) |>
             filter(time >= as.POSIXct("2015-07-07")) 
#write.csv(taxa_diags_scenarios, "./analysis/data/taxa_diags_scenarios.csv", row.names = F)

