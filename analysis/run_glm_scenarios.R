#run baseline scenario
GLM3r::run_glm(sim_folder = "./sims/baseline")

# set nml file
nc_file <- file.path('sims/baseline/output/output.nc') 

# install glmtools
library(devtools)
devtools::install_github("rqthomas/glmtools", force = TRUE)

# access and plot temperature
current_temp <- glmtools::get_var(nc_file, var_name = "temp")
p <- glmtools::plot_temp(nc_file, reference = "surface",
                         plot.title = "baseline")
ggplot2::ggsave(p, filename = paste0("./figures/waterTemp_baseline.png"), device = "png",
                height = 6, width = 8, units = "in")

# assign names to scenarios
scenario_folder_names <- c("plus1","plus5","plus10")

# create a folder for each scenarios and populate with sim files
glm_files = list.files("./sims/baseline", full.names = TRUE)[1:3] #1:3
#note that nml and input files are edited in spin-up.R

for (j in 1:length(scenario_folder_names)){
  subdirName <- paste0("./sims/",scenario_folder_names[j])
  folder<-dir.create(subdirName)
  file.copy(from = glm_files, to = subdirName, recursive = TRUE)
  outputdirName <- paste0(subdirName,"/output")
  output_folder<-dir.create(outputdirName)
}

