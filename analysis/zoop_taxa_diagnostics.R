# need to automate this and have the output save in separate folders instead of overwriting

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

#---------------------------------------------------------------------#
# add a total zoops to taxa col
#zoop_scenarios <- read_csv("./analysis/data/zoop_scenarios_vw.csv") |>
#  group_by(DateTime, scenario) |>
#  mutate(total = sum(value)) |>
#  pivot_wider(names_from = taxon, values_from = value) |>
#  pivot_longer(cols = c(total,cladoceran,copepod,rotifer),  
#                              names_to = "taxon",         
#                              values_to = "value")
  
#zoop_obs <- read_csv("analysis/data/zoop_obs.csv")

# reorder the 'taxon' factor levels
facet_labels <- c("Cladoceran", "Copepod", "Rotifer", "Total biomass")
names(facet_labels) <- c("cladoceran", "copepod", "rotifer","total")

zoop_obs <- read_csv("analysis/data/zoop_obs.csv")
zoop_scenarios <- read_csv("./analysis/data/zoop_scenarios.csv")

# plot zoops (Figure 1 g-j)
plot2 <- ggplot(data=subset(zoop_scenarios, scenario %in% "baseline")) +
  geom_line(aes(DateTime, value)) + 
  geom_point(data=zoop_obs,
            aes(DateTime, value, color=taxon)) +
  theme_bw() + xlab("") + guides(color = "none") +
  facet_wrap(~taxon, scales = "free_y", nrow=2,
             labeller = labeller(taxon = facet_labels)) +
  ylab(expression("Biomass (mg C L"^{-1}*")")) +
  geom_vline(xintercept = as.Date("2020-12-31"), linetype = "dashed") +
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

#summarize across taxa to describe bl zoop timing in results (Figure 2)
bl_zoop_summary <- zoop_scenarios |>
  filter(scenario %in% c("baseline"),
         year %in% c(2016:2021)) |>
  group_by(year, taxon) |>
  mutate(max_biom_doy = doy[which.max(value)]) |>
  ungroup() |> group_by(taxon) |>
  summarise(max_doy = mean(max_biom_doy))

ggplot() +
  geom_line(data=subset(zoop_scenarios, 
                        scenario %in% "baseline" &
                        !taxon %in% "total" &
                          DateTime >= "2015-07-08"),
            aes(DateTime, value, color=taxon)) + 
  theme_bw() + xlab("") +
  ylab(expression("Biomass (mg C L"^{-1}*")")) +
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
mean(zoop_scenarios$value[zoop_scenarios$scenario=="baseline"])
sd(zoop_scenarios$value[zoop_scenarios$scenario=="baseline"])

mean(zoop_scenarios$value[zoop_scenarios$taxon=="cladoceran" &
                             zoop_scenarios$scenario=="baseline"])
sd(zoop_scenarios$value[zoop_scenarios$taxon=="cladoceran" &
                           zoop_scenarios$scenario=="baseline"])

mean(zoop_scenarios$value[zoop_scenarios$taxon=="copepod" &
                            zoop_scenarios$scenario=="baseline"])
sd(zoop_scenarios$value[zoop_scenarios$taxon=="copepod" &
                          zoop_scenarios$scenario=="baseline"])

mean(zoop_scenarios$value[zoop_scenarios$taxon=="rotifer" &
                             zoop_scenarios$scenario=="baseline"])
sd(zoop_scenarios$value[zoop_scenarios$taxon=="rotifer" &
                           zoop_scenarios$scenario=="baseline"])

#add months to zoop df
zoop_scenarios <- zoop_scenarios |>
  mutate(month = lubridate::month(DateTime)) 
  
(mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                             zoop_scenarios$month %in% c(12,1,2)] &
        zoop_scenarios$scenario=="baseline") -
    mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                                zoop_scenarios$month %in% c(6,7,8)] &
           zoop_scenarios$scenario=="baseline")) /
  (mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                               zoop_scenarios$month %in% c(12,1,2)] &
          zoop_scenarios$scenario=="baseline") +
     mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                                 zoop_scenarios$month %in% c(6,7,8)] &
            zoop_scenarios$scenario=="baseline")) /2

#------------------------------------------------------------------------#
# zoop diags for all taxa and scenarios
#taxa <- c("copes","clads","rots")
#
#for(i in 1:length(scenario)){
#for(j in 1:length(taxa)){
#
## change output location "output/cladoceran/output.nc
#glm_nml_file <- file.path(paste0("./sims/spinup/",scenario[i],"/glm3.nml"))
#glm_nml <- glmtools::read_nml(nml_file = glm_nml_file)
#glm_nml <- glmtools::set_nml(glm_nml,
#                             arg_name = "output::out_dir",
#                             arg_val = paste0("output/",taxa[j]))
#glmtools::write_nml(glm_nml, file = glm_nml_file)
#
#zoop_files <- c("aed/aed_zoop_pars_3groups_4Sep2024.csv",
#                "aed/aed_zoop_pars_3groups_4Sep2024_clads.csv",
#                "aed/aed_zoop_pars_3groups_4Sep2024_rots.csv")
#
## walk through this to manually change which aed file glm uses
#aed_nml_file <- file.path(paste0("./sims/spinup/",scenario[i],"/aed/aed2_4zones.nml"))
#aed_nml <- glmtools::read_nml(nml_file = aed_nml_file)
#aed_nml <- glmtools::set_nml(aed_nml,
#                             arg_name = "aed_zooplankton::dbase",
#                             arg_val = zoop_files[j])
#glmtools::write_nml(aed_nml, file = aed_nml_file)
#
## run the model
#sim_folder = paste0("./sims/spinup/",scenario[i])
#GLM3r::run_glm(sim_folder)
#  
#}
#}
#
#library(ncdf4)
#library(dplyr)
#
## Specify the zip file path
#zip_file <- "./sims/spinup/baseline/output/baseline_taxa.zip"
#zip_file <- "./sims/spinup/plus1/output/plus1_taxa.zip"
#zip_file <- "./sims/spinup/plus5/output/plus5_taxa.zip"
#zip_file <- "./sims/spinup/plus10/output/plus10_taxa.zip"
#
## Create a temporary directory to extract the files
#temp_dir <- tempdir()
#unzip(zip_file, exdir = temp_dir)

# Function to extract data from an .nc file
#read_nc_data <- function(nc_file) {
#  nc <- nc_open(nc_file)
#  
#  # Extract variables, assuming the 500 is depth and 7969 is time
#  time <- ncvar_get(nc, "time")
#  grz <- ncvar_get(nc, "ZOO_grz") 
#  resp <- ncvar_get(nc, "ZOO_resp")
#  mort <- ncvar_get(nc, "ZOO_mort")
#  
#  origin_date <- as.Date("2000-07-08")  
#  time_in_days <- time / 24  # Convert time to days
#  
#  # Calculate the actual dates by adding the number of days to the origin
#  dates <- origin_date + time_in_days
#  
#  # Convert to POSIXct (if you need precise time information, e.g., midnight each day)
#  time <- as.POSIXct(dates, tz = "UTC")
#  
#  # Flatten the variables (use only time dimension)
#  # Assuming that depth is the first dimension (500) and time is the second (7969)
#  grz <- as.vector(grz[1, ])  # Select the first depth slice for grazing
#  resp <- as.vector(resp[1, ])  # Select the first depth slice for respiration
#  mort <- as.vector(mort[1, ])  # Select the first depth slice for mortality
#  
#  # Close the NetCDF file
#  nc_close(nc)
#  
#  # Extract taxon name from file path (assuming the folder name is the taxon)
#  taxon_name <- basename(dirname(nc_file))  # Extract folder name as taxon
#  
#  # Combine into a data frame
#  diags <- data.frame(
#    time = time,
#    grz = grz,
#    resp = resp,
#    mort = mort,
#    taxon = taxon_name, 
#    file = basename(nc_file)  
#  )
#  
#  return(diags) 
#}
#
## List all .nc files in the extracted folders
#nc_files <- list.files(temp_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)

# Initialize individual taxa data frames
#clads <- NULL
#copes <- NULL
#rots <- NULL
#
## Loop through all .nc files and read data for each taxon
#for (file in nc_files) {
#  taxon_name <- basename(dirname(file))  # Get the folder name (taxon)
#  
#  # Read data only for a specific taxon
#  if (taxon_name == "clads") {
#    clads <- read_nc_data(file)
#  } else if (taxon_name == "copes") {
#    copes <- read_nc_data(file)
#  } else if (taxon_name == "rots") {
#    rots <- read_nc_data(file)
#  }
#}
#
## Combine all taxa data frames into one
#plus5_diags <- bind_rows(clads, copes, rots)|>
#  mutate(mort = mort * -1,
#         resp = resp * -1) |>
#  pivot_longer(cols = c(grz, resp, mort),  
#               names_to = "diag",         
#               values_to = "value") |>
#  select(-file) |>
#  mutate(scenario = "baseline")

#----------------------------------------------------------------------#
# write modeled vars to file
#taxa_diags_scenarios <-  mget(c("baseline_diags","plus1_diags",
#                 "plus5_diags", "plus10_diags")) |>
#                   setNames(paste0(scenario)) |>
#                   bind_rows(.id = "scenario") |>
#                   relocate(scenario, .after = last_col()) |>
#             filter(time >= as.POSIXct("2015-07-07")) 
#write.csv(taxa_diags_scenarios, "./analysis/data/taxa_diags_scenarios.csv", row.names = F)

taxa_diags_scenarios <- read_csv("./analysis/data/taxa_diags_scenarios.csv")

zoop_diags_summary <- taxa_diags_scenarios |>
  mutate(DateTime = time,
         diag = case_when(diag == "grz" ~ "Grazing", 
                          diag == "mort" ~ "Mortality",
                          diag == "resp" ~ "Respiration"),
         scenario = factor(str_to_title(scenario), levels = c("Baseline", "Plus1", "Plus5", "Plus10"))) |>
  group_by(year = lubridate::year(DateTime), taxon, diag, scenario) |>
  summarize(mean_rate = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(combined_label = paste0(scenario, " ", diag))  |>
  mutate(DateTime = as.Date(paste0(year, "-01-01"))) |>
  mutate(DateTime = case_when(
    year == 2015 ~ as.Date("2015-07-08"),
    year == 2022 ~ as.Date("2022-05-03"),
    TRUE ~ DateTime))

# values for results text 
(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                    zoop_diags_summary$taxon=="clads" &
                                    zoop_diags_summary$diag=="Grazing"]) - 
mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                               zoop_diags_summary$taxon=="clads" &
                               zoop_diags_summary$diag=="Grazing"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="clads" &
                                      zoop_diags_summary$diag=="Grazing"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="copes" &
                                     zoop_diags_summary$diag=="Grazing"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="copes" &
                                        zoop_diags_summary$diag=="Grazing"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="copes" &
                                      zoop_diags_summary$diag=="Grazing"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="rots" &
                                     zoop_diags_summary$diag=="Grazing"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="rots" &
                                        zoop_diags_summary$diag=="Grazing"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="rots" &
                                      zoop_diags_summary$diag=="Grazing"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="clads" &
                                     zoop_diags_summary$diag=="Mortality"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="clads" &
                                        zoop_diags_summary$diag=="Mortality"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="clads" &
                                      zoop_diags_summary$diag=="Mortality"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="copes" &
                                     zoop_diags_summary$diag=="Mortality"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="copes" &
                                        zoop_diags_summary$diag=="Mortality"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="copes" &
                                      zoop_diags_summary$diag=="Mortality"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="rots" &
                                     zoop_diags_summary$diag=="Mortality"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="rots" &
                                        zoop_diags_summary$diag=="Mortality"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="rots" &
                                      zoop_diags_summary$diag=="Mortality"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="clads" &
                                     zoop_diags_summary$diag=="Respiration"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="clads" &
                                        zoop_diags_summary$diag=="Respiration"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="clads" &
                                      zoop_diags_summary$diag=="Respiration"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="copes" &
                                     zoop_diags_summary$diag=="Respiration"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="copes" &
                                        zoop_diags_summary$diag=="Respiration"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="copes" &
                                      zoop_diags_summary$diag=="Respiration"])

(mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus10" &
                                     zoop_diags_summary$taxon=="rots" &
                                     zoop_diags_summary$diag=="Respiration"]) - 
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="rots" &
                                        zoop_diags_summary$diag=="Respiration"])) /
  mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                      zoop_diags_summary$taxon=="rots" &
                                      zoop_diags_summary$diag=="Respiration"])


# summary diag fig + phytos for ms (Fig. 8)
plot1 <- ggplot(data=subset(zoop_diags_summary, 
                            scenario %in% c("Baseline","Plus10")),
                aes(x = DateTime, y = mean_rate, color = taxon)) +
  geom_line(size = 1) +
  geom_point() +
  facet_grid(rows = vars(diag), cols = vars(scenario), scales = "free_y") + 
  scale_x_date(limits = as.Date(c("2015-07-01", "2022-05-05")),expand = c(0.02,0.02))  +
  scale_color_manual(values = c("#084c61", "#db504a", "#e3b505"),
                     breaks = c("clads", "copes", "rots"),
                     labels = c("Cladoceran","Copepod","Rotifer")) +
  ylab("Mean rate (mmolC/m3/day)") +
  xlab("") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=10), 
        axis.text = element_text(size = 9),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold", hjust = 0),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(), 
        axis.title.y = element_text(size = 10),
        panel.spacing.x = unit(0.2, "in"),
        legend.margin = margin(c(-0,-10,-15,-10)),
        plot.margin = unit(c(0, 1.2, -1.6, 0), "cm"),
        panel.spacing = unit(0.5, "lines"),
        plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"))

total_phytos <- phyto_scenarios |>
  group_by(DateTime, scenario) |>
  summarise(total = sum(value)) |>  
  ungroup() |>
  mutate(taxon = "total") |>
  filter(scenario %in% c("baseline","plus10"))

# Compute max total biomass to rescale
max_total <- max(total_phytos$total, na.rm = TRUE)

#note that this comes from zoop_scenarios.R
plot2 <- ggplot(data = subset(phyto_scenarios, 
                              scenario %in% c("baseline", "plus10")), 
                aes(x = DateTime, y = value, fill=taxon)) +
  geom_area(position = "fill", stat = "identity") +
  geom_line(data = total_phytos, 
            aes(x = DateTime, y = total/max_total, 
            color = "Total"), linewidth = 1) +  
  facet_wrap(~scenario, scales = "free_x") +
  scale_fill_manual(values = c("cyan", "green", "brown4","black"),
                    labels = c("Cyanobacteria", "Greens", "Diatoms","")) +
  scale_color_manual(values = c("Total" = "black"),  
                     labels = c("Total phytoplankton")) +
  scale_x_date(expand = c(0, 0), 
               breaks = as.Date(c("2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01")),
               date_labels = '%Y') + xlab("") +
  scale_y_continuous(expand = c(0, 0), 
                     name = "Relative biomass",  
                     sec.axis = sec_axis(~ . * max_total, 
                                         name = expression("Total biomass (" * mu * " g L"^{-1}*")"))) +
  guides(fill = guide_legend(ncol = 4),
         color = guide_legend(ncol=1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 11),
        plot.margin = unit(c(1.4, 0.2, 0.2, 0), "cm"),
        legend.box.margin = margin(-20, -10, -10, -10),
        legend.margin = margin(0, 0, 0, 0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.spacing = unit(0.01, "cm"))

combined_plot <- plot_grid(
  plot1, plot2, 
  ncol = 1,   # Number of columns
  align = "v" # Align plots vertically
)
#ggsave("figures/ms_fig8.jpg", width=8, height=6)
  
# numbers for results text (phyto groups)
(mean(phyto_scenarios$value[phyto_scenarios$scenario=="plus10" &
                             phyto_scenarios$taxon=="cyano"]) -
mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                        phyto_scenarios$taxon=="cyano"])) /
  mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                               phyto_scenarios$taxon=="cyano"])

(mean(phyto_scenarios$value[phyto_scenarios$scenario=="plus10" &
                              phyto_scenarios$taxon=="diatom"]) -
    mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                 phyto_scenarios$taxon=="diatom"])) /
  mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                               phyto_scenarios$taxon=="diatom"])

(mean(phyto_scenarios$value[phyto_scenarios$scenario=="plus10" &
                              phyto_scenarios$taxon=="green"]) -
    mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                 phyto_scenarios$taxon=="green"])) /
  mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                               phyto_scenarios$taxon=="green"])
