# script to run glm and generate output.nc file
# Note - run spin-up.R first!

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

