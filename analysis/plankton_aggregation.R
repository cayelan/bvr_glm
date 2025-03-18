# Plankton full water column aggregation script
# Written by Heather Wander
# 22 August 2024

pacman::p_load(ggplot2, dplyr, scales, NatParksPalettes, 
               glmtools, tagger, cowplot, tidyr)

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
  write.csv(all_zoops,"analysis/data/all_zoops.csv", row.names = F)
  
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
  #write.csv(all_zoops_obs, "./analysis/data/zoop_obs.csv", row.names = F)
  
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

# phytos
for (i in 1:length(scenario)){
  
  nc_file = paste0("sims/spinup/",scenario[i],"/output/output.nc")  
  
  var="PHY_cyano"
  # Function to get phyto data for varying depths
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
  cyano_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  cyano <- cyano_full_wc |> 
    #dplyr::select(DateTime, PHY_cyano_0.5, PHY_cyano_9) |>
    dplyr::mutate(PHY_cyano = rowSums(dplyr::across(where(is.numeric)),na.rm=TRUE)) |>
    dplyr::select(DateTime, PHY_cyano) |> 
    dplyr::mutate(DateTime = as.Date(DateTime)) |>
    dplyr::filter(DateTime >= "2015-07-08")
  
  var="PHY_green"
  green_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  green <- green_full_wc |> 
    #dplyr::select(DateTime, PHY_green_0.5, PHY_green_9) |>
    dplyr::mutate(PHY_green = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
    dplyr::select(DateTime, PHY_green) |> 
    dplyr::mutate(DateTime = as.Date(DateTime)) |>
    dplyr::filter(DateTime >= "2015-07-08")
  
  var="PHY_diatom"
  diatom_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  diatom <- diatom_full_wc |> 
    #dplyr::select(DateTime, PHY_diatom_0.5, PHY_diatom_9) |>
    dplyr::mutate(PHY_diatom = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
    dplyr::select(DateTime, PHY_diatom) |> 
    dplyr::mutate(DateTime = as.Date(DateTime)) |>
    dplyr::filter(DateTime >= "2015-07-08")
  
  #combine into one df 
  all_phytos <- purrr::reduce(list(cyano, green, diatom), dplyr::full_join) 
  
  #convert from wide to long for plotting
  all_phytos_final <- all_phytos |> 
    tidyr::pivot_longer(cols = -c(DateTime), 
                        names_pattern = "(...)_(...*)$",
                        names_to = c("mod", "taxon")) |> 
    #tidyr::pivot_longer(cols = -c(DateTime), 
    #                   names_pattern = "(...)_(...*)_(..*)$",
    #                    names_to = c("mod", "taxon","depth")) |> 
    dplyr::group_by(DateTime) |>
    dplyr::mutate(daily_sum = sum(value),
                  year = lubridate::year(DateTime),
                  doy = lubridate::yday(DateTime)) |>
    dplyr::ungroup() |>
    dplyr::group_by(year) |>
    dplyr::mutate(annual_sum = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(annual_prop = (daily_sum / annual_sum) * 100) |>
    na.omit() |>
    dplyr::mutate(scenario = scenario[i]) |> # in mg C / m3 here
    dplyr::mutate(value = value * 12.011) # convert to ug/L
  
  #now create a dynamic df name
  assign(paste0("all_phytos_", scenario[i]), all_phytos_final)
}

#create a combined phyto df with all scenarios
  phyto_scenarios <-  mget(c("all_phytos_baseline","all_phytos_plus1",
                            "all_phytos_plus5", "all_phytos_plus10")) |>
    setNames(paste0(scenario)) |>
    bind_rows(.id = "scenario") |>
    relocate(scenario, .after = last_col())
 write.csv(phyto_scenarios, "./analysis/data/phyto_scenarios.csv", row.names = F)


