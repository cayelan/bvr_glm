# sensitivity analysis for zoop pars that differ among taxa

# load packages
pacman::p_load(dplyr)

# Define current parameters and their calibrated values for each taxon
param_values <- list(
  Rgrz_zoo = c(0.8, 1.3, 1.5),
  Rresp_zoo = c(0.1, 0.3, 0.2),
  Rmort_zoo = c(0.06, 0.08, 0.01),
  ffecal_zoo = c(0.05, 0.04, 0.02),
  ffecal_sed = c(0.3, 0.4, 0.8),
  Topt_zoo = c(25, 28, 28),
  Tmax_zoo = c(30, 35, 35),
  INC_zoo = c(0.05, 0.22, 0.15),
  IPC_zoo = c(0.0018, 0.024, 0.0125)
)

# Define the percentage change for sensitivity analysis
percent_change <- c(-0.1, 0.1)

# Define the taxa columns
taxa_columns <- c("rotifer", "cladoceran", "copepod")

# Create new folders for sensitivity files
glm_files <- list.files("./sims/spinup/baseline", full.names = TRUE)[1:3]
sens_dirs <- c("low", "high")

# Iterate over sensitivity cases (-10% and +10%)
for (i in 1:length(percent_change)) {
  # Create directory for the current sensitivity case
  subdirName <- paste0("./sims/spinup/sensitivity_", sens_dirs[i])
  dir.create(subdirName)
  file.copy(from = glm_files, to = subdirName, recursive = TRUE)
  
  # Load and modify the AED zooplankton parameter file
  zoop_aed_file <- read.csv(paste0(subdirName, "/aed/aed_zoop_pars_3groups_4Sep2024.csv")) |>
    rename(
      zoop.name = X.zoop_name.,
      rotifer = X.rotifer.,
      cladoceran = X.cladoceran.,
      copepod = X.copepod.
    )
  
  # Update parameters for each taxa based on percentage change
  for (param_name in names(param_values)) {
    # Calculate adjusted values
    adjusted_values <- param_values[[param_name]] * (1 + percent_change[i])
    
    # Update the corresponding rows for each taxon
    for (j in 1:length(taxa_columns)) {
      taxon <- taxa_columns[j]
      zoop_aed_file[zoop_aed_file$zoop.name == paste0(" '", param_name, "'"), taxon] <- adjusted_values[j]
    }
  }
  
  # Save the modified file
  write.csv(zoop_aed_file, file = paste0(subdirName, "/aed/aed_zoop_pars_3groups_4Sep2024.csv"), 
            row.names = FALSE, quote = FALSE)
  
  # Create output folder for model results
  dir.create(paste0(subdirName, "/output"))
}

# Run the model and generate output for each sensitivity case
for (i in 1:length(sens_dirs)) {
  sim_folder <- paste0("./sims/spinup/sensitivity_", sens_dirs[i])
  GLM3r::run_glm(sim_folder)
  
  # Set .nc file path
  nc_file <- file.path(paste0(sim_folder, "/output/output.nc"))
  
  # Access and plot temperature
  current_temp <- glmtools::get_var(nc_file, var_name = "temp")
  p <- glmtools::plot_var(nc_file, var_name = "temp", reference = "surface", 
                          plot.title = paste0("sensitivity_", sens_dirs[i]))
  plot_filename <- paste0("./figures/waterTemp_sensitivity__", sens_dirs[i], ".png")
  ggplot2::ggsave(p, filename = plot_filename, device = "png",
                  height = 6, width = 8, units = "in")
}


#------------------------------------------------------------------------#
# code to pull zoop data into a df

for (i in 1:length(sens_dirs)){
  
  nc_file = paste0("sims/spinup/sensitivity_",sens_dirs[i],"/output/output.nc")  
  
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
    dplyr::mutate(DateTime = as.Date(DateTime)) |>
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
    dplyr::mutate(DateTime = as.Date(DateTime)) |>
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
    dplyr::mutate(DateTime = as.Date(DateTime)) |>
    dplyr::filter(DateTime >= "2015-07-08")
  
  
  #combine into one df 
  all_zoops <- purrr::reduce(list(clad, cope, rot), dplyr::full_join)  |> 
    dplyr::group_by(DateTime) |>
    dplyr::mutate(ZOO_total = sum(ZOO_cladoceran, ZOO_copepod, ZOO_rotifer)) 
  
  all_zoops_obs <- purrr::reduce(list(clad_obs, cope_obs, rot_obs), dplyr::full_join) |> 
    tidyr::pivot_longer(cols = -c(DateTime), 
                        names_pattern = "(...)_(...*)$",
                        names_to = c("mod", "taxon")) |> 
    na.omit() |> 
    dplyr::mutate(DateTime = as.Date(DateTime)) |>
    dplyr::mutate(value = value * 12.011 / 1000) |> # convert to mg/L
    dplyr::filter(value < 6) # just to make the plot look better
  
  #convert from wide to long for plotting
  all_zoops_final <- all_zoops |> 
    tidyr::pivot_longer(cols = -c(DateTime), 
                        names_pattern = "(...)_(...*)$",
                        names_to = c("mod", "taxon")) |> 
    dplyr::group_by(DateTime) |>
    dplyr::mutate(daily_sum = sum(value),
                  year = lubridate::year(DateTime),
                  doy = lubridate::yday(DateTime)) |>
    dplyr::ungroup() |>
    na.omit() |>
    dplyr::mutate(value = value * 12.011 / 1000) |> # convert to mg/L 
    dplyr::mutate(scenario = sens_dirs[i])
  
  #now create a dynamic df name
  assign(paste0("all_zoops_sens_", sens_dirs[i]), all_zoops_final)
}
#------------------------------------------------------------------------#
#create a combined zoop df with all scenarios
#zoop_pars_sens <-  mget(c("all_zoops_sens_high","all_zoops_sens_low")) |>
#                   setNames(paste0(sens_dirs)) |>
#                   bind_rows(.id = "pars") |>
#                   relocate(pars, .after = last_col())
#write.csv(zoop_pars_sens, "./analysis/data/zoop_pars_sens.csv", row.names = F)

zoop_pars <-read.csv("analysis/data/zoop_pars_sens.csv") |>
  mutate(DateTime = as.Date(DateTime)) |>
  filter(DateTime >= "2015-07-07")


# zoop figs to summarize changes between high vs. low pars
ggplot() +
  geom_line(data=zoop_pars,
            aes(DateTime, value, color = scenario)) +
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") + ylab("Value") +
  scale_color_manual("", values = c("#f4a261","#2a9d8f"),
                     breaks = c("high","low")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.32,0.98),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_sens_high_low_pars.jpg", width=6, height=6)

ggplot(data = subset(zoop_pars, !taxon %in% "total"),
       aes(x=DateTime, y = value, color=taxon)) +
  geom_area(aes(color = taxon, fill = taxon),
            position = "fill", 
            stat = "identity") +
  facet_wrap(~pars, scales = "free_x")+
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_x_date(expand = c(0,0), date_breaks = "1 year", 
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0))+
  xlab("") + ylab("Relative biomass") +
  guides(color= "none",
         fill = guide_legend(ncol=3)) +
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
#ggsave("figures/BVR_relative_zoop_high_low_pars.jpg", width=7, height=4)

# values for results text
mean(zoop_pars$value[zoop_pars$taxon=="cladoceran" & 
                       zoop_pars$scenario=="high"]) -
mean(zoop_pars$value[zoop_pars$taxon=="cladoceran" & 
                       zoop_pars$scenario=="low"])

mean(zoop_pars$value[zoop_pars$taxon=="copepod" & 
                       zoop_pars$scenario=="high"]) -
mean(zoop_pars$value[zoop_pars$taxon=="copepod" & 
                       zoop_pars$scenario=="low"])

mean(zoop_pars$value[zoop_pars$taxon=="rotifer" & 
                       zoop_pars$scenario=="high"]) -
mean(zoop_pars$value[zoop_pars$taxon=="rotifer" & 
                       zoop_pars$scenario=="low"])

mean_proportions_par_sens <- zoop_pars |>
  group_by(DateTime, scenario) |>
  mutate(proportion = value / value[taxon=="total"]) |>
  group_by(taxon, scenario, DateTime) |>
  summarise(mean_proportion = mean(proportion)) |>
  filter(!taxon %in% "total")

(mean(mean_proportions_par_sens$mean_proportion[
  mean_proportions_par_sens$taxon=="cladoceran" &
    mean_proportions_par_sens$scenario=="high"]) -
mean(mean_proportions_par_sens$mean_proportion[
  mean_proportions_par_sens$taxon=="cladoceran" &
    mean_proportions_par_sens$scenario=="low"])) * 100

(mean(mean_proportions_par_sens$mean_proportion[
  mean_proportions_par_sens$taxon=="copepod" &
    mean_proportions_par_sens$scenario=="high"]) -
mean(mean_proportions_par_sens$mean_proportion[
  mean_proportions_par_sens$taxon=="copepod" &
    mean_proportions_par_sens$scenario=="low"])) *100

(mean(mean_proportions_par_sens$mean_proportion[
  mean_proportions_par_sens$taxon=="rotifer" &
    mean_proportions_par_sens$scenario=="high"]) -
mean(mean_proportions_par_sens$mean_proportion[
  mean_proportions_par_sens$taxon=="rotifer" &
    mean_proportions_par_sens$scenario=="low"])) *100


# smoothed monthly biomass for each scenario
zoop_pars |>
  filter(year %in% c(2016:2021)) |>
  mutate(month = lubridate::month(DateTime),
         taxon = stringr::str_to_title(taxon)) |>
  group_by(taxon, scenario, month) |>
  summarise(monthly_biom = mean(value), .groups = "drop") |>
  ggplot(aes(x = factor(month), y = monthly_biom, color = factor(
    scenario, levels = c("high","low")),
    group = interaction(taxon, scenario))) + 
  geom_smooth(method = "loess") +
  facet_wrap(~taxon, nrow=1, scales = "free_y")+ 
  scale_x_discrete(labels = c("Jan","","Mar","", "May", "", "Jul",
                              "","Sep", "","Nov","")) +
  ylab(expression("Biomass (mg L"^{-1}*")")) + xlab("") +
  scale_color_manual("", values = c("#f4a261","#2a9d8f"),
                     breaks = c("high", "low")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size = 10), 
        axis.text.x = element_text(angle=45, vjust = 0.9, hjust= 0.8),
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold", hjust = ),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 11),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.box.margin = margin(0, -10, -10, -10),
        legend.margin = margin(0, 0, 0, 0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/smoothed_monthly_biom_high_vs_low.jpg", width=7, height=4) 

