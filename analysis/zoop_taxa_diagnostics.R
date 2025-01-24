# need to automate this and have the output save in separate folders instead of overwriting

pacman::p_load(ggplot2, dplyr, scales, NatParksPalettes, glmtools)

scenario <- c("baseline","plus1","plus5","plus10")
  
for (i in 1:length(scenario)){
    
    nc_file = paste0("sims/spinup/",scenario[i],"/output/output.nc")  
                      
#save zoop output
var="ZOO_cladoceran"
clad_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit()  

# Function to get zoop data for varying depths
get_zoops <- function(depths, nc_file, var) {
  lapply(depths, function(z) {
    if (z == 0) {
      glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface')
    } else {
      glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface') |>
        dplyr::select(-DateTime)
    }
  }) |> 
    dplyr::bind_cols()
  
}

# Define depth range and call the function
depths <- seq(0, 11, by = 0.5)
clad_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
clad <- clad_full_wc |> 
  dplyr::mutate(ZOO_cladoceran = rowSums(dplyr::across(where(is.numeric)),na.rm=TRUE)) |>
  dplyr::select(DateTime, ZOO_cladoceran) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |>
  dplyr::filter(DateTime >= "2015-07-08")

var="ZOO_copepod"
cope_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

cope_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
cope <- cope_full_wc |> 
  dplyr::mutate(ZOO_copepod = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
  dplyr::select(DateTime, ZOO_copepod) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |>
  dplyr::filter(DateTime >= "2015-07-08")


var="ZOO_rotifer"
rot_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

rot_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
rot <- rot_full_wc |> 
  dplyr::mutate(ZOO_rotifer = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
  dplyr::select(DateTime, ZOO_rotifer) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |>
  dplyr::filter(DateTime >= "2015-07-08")


#combine into one df 
all_zoops <- purrr::reduce(list(clad, cope, rot), dplyr::full_join) 

all_zoops_obs <- purrr::reduce(list(clad_obs, cope_obs, rot_obs), dplyr::full_join) |> 
  dplyr::group_by(DateTime) |>
  dplyr::mutate(ZOO_total = sum(ZOO_cladoceran, ZOO_copepod, ZOO_rotifer, na.rm=T)) |>
  tidyr::pivot_longer(cols = -c(DateTime), 
                      names_pattern = "(...)_(...*)$",
                      names_to = c("mod", "taxon")) |> 
  na.omit() |> 
  dplyr::mutate(DateTime = as.Date(DateTime)) |>
  dplyr::mutate(value = value * 12.011 / 1000) |> # convert to mg/L
  dplyr::filter(value < 6) # just to make the plot look better
  
#convert from wide to long for plotting
all_zoops_final <- all_zoops |> 
  dplyr::group_by(DateTime) |>
  dplyr::mutate(ZOO_total = sum(ZOO_cladoceran, ZOO_copepod, ZOO_rotifer)) |>
  tidyr::pivot_longer(cols = -c(DateTime), 
                      names_pattern = "(...)_(...*)$",
                      names_to = c("mod", "taxon")) |> 
  dplyr::mutate(daily_sum = sum(value),
                year = lubridate::year(DateTime),
                doy = lubridate::yday(DateTime)) |>
  dplyr::ungroup() |>
  na.omit() |>
  dplyr::mutate(value = value * 12.011 / 1000) |># convert to mg/L 
  dplyr::mutate(scenario = scenario[i])
  
  #now create a dynamic df name
  assign(paste0("all_zoops_", scenario[i]), all_zoops_final)
  
  }

#create a combined zoop df with all scenarios
#zoop_scenarios <-  mget(c("all_zoops_baseline","all_zoops_plus1",
#                 "all_zoops_plus5", "all_zoops_plus10")) |>
#                   setNames(paste0(scenario)) |>
#                   bind_rows(.id = "scenario") |>
#                   relocate(scenario, .after = last_col()) |>
#  filter(DateTime >= as.Date("2015-07-07"))
#  write.csv(zoop_scenarios, "./analysis/data/zoop_scenarios.csv", row.names = F)


# reorder the 'taxon' factor levels
facet_labels <- c("Cladoceran", "Copepod", "Rotifer", "Total biomass")
names(facet_labels) <- c("cladoceran", "copepod", "rotifer","total")

# plot zoops
plot2 <- ggplot() +
  geom_line(data=all_zoops_baseline, aes(DateTime, value)) + 
  geom_point(data=all_zoops_obs,
            aes(DateTime, value, color=taxon)) +
  theme_bw() + xlab("") + guides(color = "none") +
  facet_wrap(~taxon, scales = "free_y", nrow=2,
             labeller = labeller(taxon = facet_labels)) +
  ylab(expression("Biomass (mg L"^{-1}*")")) +
  scale_color_manual(values = c("#084c61","#db504a","#e3b505","red"),
                     breaks = c("cladoceran","copepod","rotifer", "total"))+
  tag_facets(tag_pool = letters[7:10] ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(1, 0.1, 0, 0), "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))
#ggsave("figures/zoop_mod_vs_obs_copes_last.jpg", width=6, height=4)

ggplot() +
  geom_line(data=subset(all_zoops_baseline, !taxon %in% "total"),
            aes(DateTime, value, color=taxon)) + 
  theme_bw() + xlab("") +
  ylab(expression("Biomass (mg L"^{-1}*")")) +
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"),
                     breaks = c("cladoceran","copepod","rotifer"),
                     labels = c("Cladoceran","Copepod","Rotifer"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.75,0.95),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-10,-10,-10)),
        legend.key = element_rect(fill = "transparent"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_dynamics_copes_last.jpg", width=6, height=4)

# numbers for results text
mean(all_zoops_final$value)
sd(all_zoops_final$value)

mean(all_zoops_final$value[all_zoops_final$taxon=="cladoceran"])
sd(all_zoops_final$value[all_zoops_final$taxon=="cladoceran"])

mean(all_zoops_final$value[all_zoops_final$taxon=="copepod"])
sd(all_zoops_final$value[all_zoops_final$taxon=="copepod"])

mean(all_zoops_final$value[all_zoops_final$taxon=="rotifer"])
sd(all_zoops_final$value[all_zoops_final$taxon=="rotifer"])

#add months to zoop df
all_zoops_final <- all_zoops_final |>
  mutate(month = lubridate::month(DateTime)) 
  
(mean(all_zoops_final$value[all_zoops_final$taxon=="total" & 
                              all_zoops_final$month %in% c(12,1,2)]) -
    mean(all_zoops_final$value[all_zoops_final$taxon=="total" & 
                                 all_zoops_final$month %in% c(6,7,8)])) /
  (mean(all_zoops_final$value[all_zoops_final$taxon=="total" & 
                                all_zoops_final$month %in% c(12,1,2)]) +
     mean(all_zoops_final$value[all_zoops_final$taxon=="total" & 
                                  all_zoops_final$month %in% c(6,7,8)])) /2

#------------------------------------------------------------------------#
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
  
  # Assuming time is in 24-hour periods
  origin_date <- as.Date("2000-07-08")  # Your start date
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
baseline_diags <- bind_rows(clads, copes, rots)|>
  mutate(mort = mort * -1,
         resp = resp * -1) |>
  pivot_longer(cols = c(grz, resp, mort),  
               names_to = "diag",         
               values_to = "value") |>
  select(-file) 

ggplot(baseline_diags, aes(x = time, y = value)) + 
  geom_area(aes(color = diag, fill = diag),
            position = "stack", stat="identity",
            linewidth=1) +
  facet_wrap(~taxon) +
  scale_color_manual(values = NatParksPalettes::
                       natparks.pals("Volcanoes", 5,  direction = -1))+
  scale_fill_manual(values = NatParksPalettes::
                      natparks.pals("Volcanoes", 5, direction = -1))+
  ylab("diagnostics (mmolC/m3/day)")+ xlab("") +
  scale_x_datetime(expand = c(0,0),labels = 
                     date_format("%Y",tz="EST5EDT")) +
  #scale_y_continuous(expand = c(0,0), limits = c(-5,10))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.88,0.83),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
ggsave("figures/BVR_stacked_zoop_diags_baseline.jpg", width=5, height=4) 

zoop_diags <- 
  purrr::reduce(list(zoop_diag_clads_last |> mutate(taxon = "cladoceran"), 
                     zoop_diag_copes_last |> mutate(taxon = "copepod"), 
                     zoop_diag_rots_last |> mutate(taxon = "rotifer")), 
                     dplyr::full_join) |>
  mutate(variable = case_when(
      variable == "grz" ~ "grazing rate",
      variable == "mort" ~ "mortality rate",
      variable == "resp" ~ "respiration rate"),
      year = lubridate::year(DateTime),
      season = case_when(
        lubridate::month(DateTime) %in% c(12,1,2) ~ "winter",
        lubridate::month(DateTime) %in% c(3,4,5) ~ "spring",
        lubridate::month(DateTime) %in% c(6,7,8) ~ "summer",
        lubridate::month(DateTime) %in% c(9,10,11) ~ "fall")) |>
  filter(year %in% c(2016:2021))

ggplot(zoop_diags, 
              aes(x = as.factor(year), y = value, fill = taxon)) + 
  geom_boxplot() + theme_bw() +
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"),
                     breaks = c("cladoceran","copepod","rotifer"))+
  ylab("rate (mmolC/m3/day)")+ xlab("") +
  facet_wrap(~ paste(variable, season, sep = " - "), scales = "free_y", nrow=3) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        legend.margin = margin(c(-0,-10,-15,-10)),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/taxa_diag_yearly_boxplots_seasons.jpg", width=6, height=6)

zoop_diags_summary <- zoop_diags |>
  group_by(year = lubridate::year(DateTime), taxon, variable) |>
  summarize(mean_rate = mean(value, na.rm = TRUE), .groups = "drop")

ggplot(zoop_diags_summary, aes(x = year, y = mean_rate, color = taxon)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~variable, scales = "free_y", nrow = 3) +
  scale_color_manual(values = c("#084c61", "#db504a", "#e3b505"),
                     breaks = c("cladoceran", "copepod", "rotifer")) +
  theme_bw() +
  ylab("Mean Rate (mmolC/m3/day)") +
  xlab("") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        legend.margin = margin(c(-0,-10,-15,-10)),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/annual_zoop_diag.jpg", width=6, height=6)
