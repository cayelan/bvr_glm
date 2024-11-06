# sensitivity analysis for zoop pars that differ among taxa

# load packages
pacman::p_load(dplyr)

# Rgrz_zoo (0.9-1.1), Rresp_zoo (0.2-0.3), Rmort_zoo (0.01-0.07), theta_resp_zoo (1.06-1.09)
# note not doing prey preferences even though those differ among taxa

#define low and high vals
Rgrz <- c(0.9,1.1)
Rresp <- c(0.2,0.3)
Rmort <- c(0.01,0.07)
theta_resp <- c(1.06,1.09)

# List of parameter names and their values
param_list <- list(Rgrz = Rgrz, Rresp = Rresp, Rmort = Rmort, 
                   theta_resp = theta_resp)
taxa_columns <- c("rotifer", "cladoceran", "copepod")  # Columns for each taxa

# create a new folder where all the sensitivity files will be copied into
glm_files = list.files("./sims/baseline", full.names = TRUE)[1:3] 

pars <- c("low","high")

# Iterate over each sensitivity setting (low and high)
for(i in 1:length(pars)){
  # Directory setup
  subdirName <- paste0("./sims/spinup/sensitivity_", pars[i])
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
  
  # Update parameters for each taxa
  for(param_name in names(param_list)){
    # Get the specific low or high value for this iteration
    value <- param_list[[param_name]][i]
    
    # Modify the corresponding rows for each taxa
    for(taxa in taxa_columns){
      zoop_aed_file[zoop_aed_file$zoop.name == paste0(" '", param_name, "_zoo'"), taxa] <- as.numeric(value)
      
    }
  }
  
  # Save the modified file
  write.csv(zoop_aed_file, file = paste0(subdirName, "/aed/aed_zoop_pars_3groups_4Sep2024.csv"), 
            row.names = FALSE, quote = FALSE)
  
  # Create output folder for model results
  dir.create(paste0(subdirName, "/output"))
}  

# now run the model and generate output
for (i in 1:length(pars)){
  
  # run the model
  sim_folder = paste0("./sims/spinup/sensitivity_",pars[i])
  GLM3r::run_glm(sim_folder)
  
  # set nml file
  nc_file <- file.path(paste0("sims/spinup/sensitivity_",pars[i],"/output/output.nc")) 
  
  # access and plot temperature
  current_temp <- glmtools::get_var(nc_file, var_name = "temp")
  p <- glmtools::plot_var(nc_file, var_name = "temp", reference = "surface", 
                          plot.title = paste0("sensitivity_",pars[i]))
  plot_filename <- paste0("./figures/waterTemp_sensitivity__",pars[i],".png")
  ggplot2::ggsave(p, filename = plot_filename, device = "png",
                  height = 6, width = 8, units = "in")
  
}

#------------------------------------------------------------------------#
# code to pull zoop data into a df

for (i in 1:length(pars)){
  
  nc_file = paste0("sims/spinup/sensitivity_",pars[i],"/output/output.nc")  
  
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
    dplyr::mutate(DateTime = as.Date(DateTime)) 
  
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
    dplyr::mutate(DateTime = as.Date(DateTime))
  
  
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
    dplyr::mutate(DateTime = as.Date(DateTime))
  
  
  #combine into one df 
  all_zoops <- purrr::reduce(list(clad, cope, rot), dplyr::full_join) 
  
  all_zoops_obs <- purrr::reduce(list(clad_obs, cope_obs, rot_obs), dplyr::full_join) |> 
    tidyr::pivot_longer(cols = -c(DateTime), 
                        names_pattern = "(...)_(...*)$",
                        names_to = c("mod", "taxon")) |> 
    na.omit() |> 
    dplyr::filter(value < 500) |> 
    dplyr::mutate(DateTime = as.Date(DateTime))
  
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
    dplyr::group_by(year) |>
    dplyr::mutate(annual_sum = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(annual_prop = (daily_sum / annual_sum) * 100) |>
    na.omit()
  
  #now create a dynamic df name
  assign(paste0("all_zoops_sens_", pars[i]), all_zoops_final)
}

#------------------------------------------------------------------------#
# now zoop figs to summarize changes between high vs. low pars
ggplot() +
  geom_line(data=all_zoops_sens_high,
            aes(DateTime, value, color = "high")) +
  geom_line(data=all_zoops_sens_low,
            aes(DateTime, value, color = "low")) +
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") + 
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

#proportional zoop figs
ggplot() +
  geom_line(data=all_zoops_sens_high,
            aes(DateTime, value, color = "high")) +
  geom_line(data=all_zoops_sens_low,
            aes(DateTime, value, color = "low")) +
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") + ylab("Zooplankton annual proportion") +
  scale_color_manual("", values = c("#f4a261","#2a9d8f"),
                     breaks = c("high","low")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.84,.15),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.key = element_rect(fill = "transparent"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/proportional_zoop_sens_high_low_pars.jpg", width=6, height=6)

#create a combined zoop df with all scenarios
#zoop_pars <-  mget(c("all_zoops_sens_high","all_zoops_sens_low")) |>
#                   setNames(paste0(pars)) |>
#                   bind_rows(.id = "pars") |>
#                   relocate(pars, .after = last_col())
#write.csv(zoop_pars, "./analysis/data/zoop_pars.csv", row.names = F)

zoop_pars <-read.csv("analysis/data/zoop_pars.csv") |>
  mutate(DateTime = as.Date(DateTime)) |>
  filter(DateTime >= "2015-07-07")

ggplot(data = zoop_pars,
       aes(x=DateTime, y = value, color=taxon)) +
  geom_area(aes(color = taxon, fill = taxon),
            position = "fill", 
            stat = "identity", 
            alpha=0.7) +
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
        axis.text.x = element_text(angle=90),
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
