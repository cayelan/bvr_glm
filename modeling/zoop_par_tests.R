#code for plotting different zoop parameter sets 

pacman::p_load(tidyverse)

file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
file.copy('aed/aed_zoop_pars_3groups_28Apr2024.csv', 
          'aed/aed_zoop_pars.csv', overwrite = TRUE)

#run the model!
system2(paste0(sim_folder,"/glm.app/Contents/MacOS/glm"), 
        stdout = TRUE, stderr = TRUE, 
        env = paste0("DYLD_LIBRARY_PATH=", sim_folder,
                     "/glm.app/Contents/MacOS"))

#define the output.nc file 
nc_file <- file.path(sim_folder, 'output/output.nc') 


#save zoop full wc biomass by taxon

var="ZOO_cladoceran"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

clad_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                           zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                           zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                           zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                           zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                           zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_clad = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_clad)


var="ZOO_copepod"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

cope_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                          zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                          zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                          zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                          zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                          zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_cope = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_cope)
  

var="ZOO_rotifer"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

rot_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                         zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                         zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                         zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                         zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                         zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_rot = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_rot)

#combine all taxa into 1 df (ps = par set)
zoops_ps1 <- bind_cols(clad_full_wc, 
                       cope_full_wc[!names(cope_full_wc) %in% "DateTime"], 
                       rot_full_wc[!names(rot_full_wc) %in% "DateTime"]) |> 
  mutate(ps = 1) |> 
  pivot_longer(cols = ZOO_clad:ZOO_rot,
               names_to = "Taxon")

#------------------------------------------------------------------------------#
#high pars
file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
file.copy('aed/aed_zoop_high_pars_3groups_14May2024.csv', 
          'aed/aed_zoop_pars.csv', overwrite = TRUE)

#run the model!
system2(paste0(sim_folder,"/glm.app/Contents/MacOS/glm"), 
        stdout = TRUE, stderr = TRUE, 
        env = paste0("DYLD_LIBRARY_PATH=", sim_folder,
                     "/glm.app/Contents/MacOS"))

var="ZOO_cladoceran"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

clad_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                          zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                          zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                          zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                          zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                          zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_clad = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_clad)


var="ZOO_copepod"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

cope_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                          zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                          zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                          zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                          zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                          zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_cope = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_cope)


var="ZOO_rotifer"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

rot_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                         zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                         zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                         zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                         zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                         zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_rot = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_rot)

#combine all taxa into 1 df (ps = par set)
zoops_ps2 <- bind_cols(clad_full_wc, 
                       cope_full_wc[!names(cope_full_wc) %in% "DateTime"], 
                       rot_full_wc[!names(rot_full_wc) %in% "DateTime"]) |> 
  mutate(ps = 2) |> 
  pivot_longer(cols = ZOO_clad:ZOO_rot,
               names_to = "Taxon")

#------------------------------------------------------------------------------#
#low pars
file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
file.copy('aed/aed_zoop_low_pars_3groups_14May2024.csv', 
          'aed/aed_zoop_pars.csv', overwrite = TRUE)

#run the model!
system2(paste0(sim_folder,"/glm.app/Contents/MacOS/glm"), 
        stdout = TRUE, stderr = TRUE, 
        env = paste0("DYLD_LIBRARY_PATH=", sim_folder,
                     "/glm.app/Contents/MacOS"))

var="ZOO_cladoceran"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

clad_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                          zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                          zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                          zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                          zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                          zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_clad = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_clad)


var="ZOO_copepod"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

cope_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                          zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                          zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                          zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                          zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                          zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_cope = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_cope)


var="ZOO_rotifer"

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> select(-DateTime)

rot_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                         zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                         zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                         zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                         zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                         zoops_10,zoops_10.5,zoops_11) |> 
  mutate(ZOO_rot = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime)=="12") |> 
  select(DateTime, ZOO_rot)

#combine all taxa into 1 df (ps = par set)
zoops_ps3 <- bind_cols(clad_full_wc, 
                       cope_full_wc[!names(cope_full_wc) %in% "DateTime"], 
                       rot_full_wc[!names(rot_full_wc) %in% "DateTime"]) |> 
  mutate(ps = 3) |> 
  pivot_longer(cols = ZOO_clad:ZOO_rot,
               names_to = "Taxon")

#------------------------------------------------------------------------------#
#bind all 3 par sets
all_zoop_pars <- bind_rows(zoops_ps1, zoops_ps2, zoops_ps3) 

#get obs for each taxon
clad_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  dplyr::select(DateTime, ZOO_cladoceran) %>%
  na.omit() |>  filter(DateTime < as.POSIXct("2020-12-31") &
                         DateTime > as.POSIXct("2015-07-08"))

cope_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  dplyr::select(DateTime, ZOO_copepod) %>%
  na.omit() |>  filter(DateTime < as.POSIXct("2020-12-31") &
                         DateTime > as.POSIXct("2015-07-08"))

rot_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  dplyr::select(DateTime, ZOO_rotifer) %>%
  na.omit() |>  filter(DateTime < as.POSIXct("2020-12-31"))

#plot different par sets
ggplot(all_zoop_pars, aes(DateTime, value, color=as.factor(ps))) +
  geom_line() + geom_point() + guides(color=guide_legend("parameters")) +
  facet_wrap(~Taxon, ncol=1, scales = "free_y") + theme_bw() +
  geom_point(data=data.frame(DateTime = clad_obs$DateTime, 
                             value = clad_obs$ZOO_cladoceran, Taxon="ZOO_clad"), 
             color="black") +
  geom_point(data=data.frame(DateTime = cope_obs$DateTime, 
                             value = cope_obs$ZOO_copepod, Taxon="ZOO_cope"), 
             color="black") +
  geom_point(data=data.frame(DateTime = rot_obs$DateTime, 
                             value = rot_obs$ZOO_rotifer, Taxon="ZOO_rot"), 
             color="black") +
  scale_color_manual(values = NatParksPalettes::natparks.pals("Triglav",7)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "right",
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_zoop_biomass_par_tests.jpg", width=5, height=4)

