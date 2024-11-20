# Plankton air temp scenarios
# 22 August 2024

pacman::p_load(ggplot2,ggridges,dplyr, emeans, lme4, lmerTest)

scenario <- c("baseline","plus1", "plus5","plus10")

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
assign(paste0("all_zoops_", scenario[i]), all_zoops_final)
}

#------------------------------------------------------------------#
# same for phytos
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
    dplyr::mutate(PHY_cyano = rowSums(dplyr::across(where(is.numeric)),na.rm=TRUE)) |>
    dplyr::select(DateTime, PHY_cyano) |> 
    dplyr::mutate(DateTime = as.Date(DateTime)) 
  
  var="PHY_green"
  green_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  green <- green_full_wc |> 
    dplyr::mutate(PHY_green = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
    dplyr::select(DateTime, PHY_green) |> 
    dplyr::mutate(DateTime = as.Date(DateTime))
  
  var="PHY_diatom"
  diatom_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  diatom <- diatom_full_wc |> 
    dplyr::mutate(PHY_diatom = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
    dplyr::select(DateTime, PHY_diatom) |> 
    dplyr::mutate(DateTime = as.Date(DateTime))
  
  #combine into one df 
  all_phytos <- purrr::reduce(list(cyano, green, diatom), dplyr::full_join) 
  
  #convert from wide to long for plotting
  all_phytos_final <- all_phytos |> 
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
  assign(paste0("all_phytos_", scenario[i]), all_phytos_final)
}

#-------------------------------------------------------------------------#
# And lastly chla 

for (i in 1:length(scenario)){
  
  nc_file = paste0("sims/spinup/",scenario[i],"/output/output.nc")  
  
var="PHY_tchla"
chla_obs <- read.csv('field_data/CleanedObsChla.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |> 
  na.omit()

#read mod chla
chla_mod<- glmtools::get_var(nc_file, var, reference="surface", z_out=depths) |> 
  tidyr::pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) |> 
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::mutate(Depth=as.numeric(Depth)) |> 
  na.omit()

#now create a dynamic df name
assign(paste0("chla_", scenario[i]), chla_mod)
}

#------------------------------------------------------------------#
# plot zoops
ggplot() +
  geom_line(data=all_zoops_baseline,
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=all_zoops_plus1,
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=all_zoops_plus5,
            aes(DateTime, value, color = "+5C")) +
  geom_line(data=all_zoops_plus10,
            aes(DateTime, value, color = "+10C")) +
  geom_point(data=all_zoops_obs,
             aes(DateTime, value, color="observed")) + 
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") + 
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000","red"),
                     breaks = c("+0C","+1C","+5C","+10C","observed")) +
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
#ggsave("figures/zoop_scenario_airtemp_1_5_10.jpg", width=6, height=6)

#proportional zoop figs
ggplot() +
  geom_line(data=all_zoops_baseline,
            aes(doy, annual_prop, color = "+0C")) +
  geom_line(data=all_zoops_plus1,
            aes(doy, annual_prop, color = "+1C")) +
  geom_line(data=all_zoops_plus5,
            aes(doy, annual_prop, color = "+5C")) +
  geom_line(data=all_zoops_plus10,
            aes(doy, annual_prop, color = "+10C")) +
  facet_wrap(~year, scales="free_y", strip.position = "top") + 
  ylab("Zooplankton annual proportion") +
  theme_bw() + xlab("") + guides(color = guide_legend(nrow = 2)) +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("+0C","+1C","+5C","+10C")) +
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
#ggsave("figures/proportional_zoop_scenario_airtemp_1_5_10.jpg", width=6, height=6)

# plot phytos
ggplot() +
  geom_line(data=all_phytos_baseline,
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=all_phytos_plus1,
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=all_phytos_plus5,
            aes(DateTime, value, color = "+5C")) +
  geom_line(data=all_phytos_plus10,
            aes(DateTime, value, color = "+10C")) +
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("+0C","+1C","+5C","+10C")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.28,0.98),
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
#ggsave("figures/phytos_scenario_airtemp_1_5_10.jpg", width=6, height=6)

#proportional phytos fig
ggplot() +
  geom_line(data=all_phytos_baseline,
            aes(doy, annual_prop, color = "+0C")) +
  geom_line(data=all_phytos_plus1,
            aes(doy, annual_prop, color = "+1C")) +
  geom_line(data=all_phytos_plus5,
            aes(doy, annual_prop, color = "+5C")) +
  geom_line(data=all_phytos_plus10,
            aes(doy, annual_prop, color = "+10C")) +
  facet_wrap(~year, scales="free_y", strip.position = "top") + 
  theme_bw() + xlab("") + guides(color = guide_legend(nrow = 2)) +
  ylab("Phytoplankton annual proportion") +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("+0C","+1C","+3C","+5C")) +
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
#ggsave("figures/proportional_phyto_scenario_airtemp_1_5_10.jpg", width=6, height=6)

# plot chla
ggplot() +
  geom_line(data=subset(chla_baseline, Depth %in% 0),
            aes(DateTime, PHY_tchla, color = "+0C")) +
  geom_line(data=subset(chla_plus1, Depth %in% 0),
            aes(DateTime, PHY_tchla, color = "+1C")) +
  geom_line(data=subset(chla_plus5, Depth %in% 0),
            aes(DateTime, PHY_tchla, color = "+5C")) +
  geom_line(data=subset(chla_plus10, Depth %in% 0),
            aes(DateTime, PHY_tchla, color = "+10C")) +
  geom_point(data=subset(chla_obs, Depth %in% 0.1),
             aes(DateTime, PHY_tchla, color = "observed")) +
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000","red"),
                     breaks = c("+0C","+1C","+5C","+10C","observed")) +
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
#ggsave("figures/chla_0.1m_scenario_airtemp.jpg", width=6, height=6)

#------------------------------------------------------------------------#
# proportional plankton figs for each scenario

#create a combined zoop df with all scenarios
#zoop_scenarios <-  mget(c("all_zoops_baseline","all_zoops_plus1",
#                 "all_zoops_plus5", "all_zoops_plus10")) |>
#                   setNames(paste0(scenario)) |>
#                   bind_rows(.id = "scenario") |>
#                   relocate(scenario, .after = last_col())
  #write.csv(zoop_scenarios, "./analysis/data/zoop_scenarios.csv", row.names = F)

zoop_scenarios <-read.csv("analysis/data/zoop_scenarios.csv") |>
  mutate(DateTime = as.Date(DateTime)) |>
  filter(DateTime >= "2015-07-07") |>
  group_by(taxon, scenario) |>
  mutate(mean_biom = mean(value)) |>
  ungroup() |>
  mutate(diff = value - mean_biom)

# relative zoop density for baseline vs. plus 10
  ggplot(data = subset(zoop_scenarios, scenario %in% c("baseline","plus10")),
         aes(x=DateTime, y = value, color=taxon)) +
  geom_area(aes(fill = taxon, color=taxon),
            position = "fill", 
            stat = "identity") +
  facet_wrap(~scenario, scales = "free_x")+
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_x_date(expand = c(0,0), 
               breaks = as.Date(c("2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01")),
               date_labels = '%Y') +
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
#ggsave("figures/BVR_relative_zoop_scenarios.jpg", width=7, height=4) 
  
# proportion boxplots for each scenario
mean_proportions <- zoop_scenarios |>
    group_by(DateTime, scenario) |>
    mutate(proportion = value / sum(value)) |>
    group_by(taxon, scenario) |>
    summarise(mean_proportion = mean(proportion)) 
  
# biomass for each year + taxa
  ggplot(zoop_scenarios,
         aes(x=DateTime, y = value, color=scenario)) +
    geom_line() +
    facet_wrap(~taxon, scales = "free_y", nrow=3)+
    scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                       breaks = c("baseline","plus1","plus5","plus10")) +
    scale_x_date(expand = c(0,0), date_breaks = "1 year", 
                 date_labels = "%Y") +
    ylab(expression("Biomass (" * mu * "g L"^{-1}*")")) + xlab("") +
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
          legend.box.margin = margin(0,-10,-15,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_zoop_biom_scenario_lineplot.jpg", width=7, height=4) 
  
#boxplots for scenarios + year facets
  ggplot(zoop_scenarios, aes(x = factor(
    scenario,levels=c("baseline","plus1","plus5","plus10")),
    y = value, fill = taxon)) +
    geom_boxplot() + facet_wrap(~year) +
    labs(y = expression("Biomass (" * mu * "g L"^{-1}*")"), x = "") +
    theme_bw() + scale_fill_manual(values = c("#084c61","#db504a","#e3b505"))+
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
          legend.box.margin = margin(0,-10,-15,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
#ggsave("figures/zoop_biom_vs_scenario_year_facet_boxplots.jpg", width=7, height=4) 
    
# smoothed monthly biomass for each scenario
  zoop_scenarios |>
    mutate(month = lubridate::month(DateTime)) |>
    group_by(taxon, scenario, month) |>
    summarise(monthly_biom = mean(value), .groups = "drop") |>
    ggplot(aes(x = factor(month), y = monthly_biom, color = factor(
      scenario, levels = c("baseline", "plus1", "plus5", "plus10")),
      group = interaction(taxon, scenario))) +  # Add `group` aesthetic
    geom_line() +
    geom_smooth(method = "loess") +
    facet_wrap(~taxon) +
    scale_x_discrete(labels = c("Jan","","Mar","","May","","Jul","","Sep","","Nov","")) +
    labs(y = expression("Biomass (" * mu * " g L"^{-1}*")"), x = "") +
    scale_color_manual("", values = c("#00603d", "#c6a000", "#c85b00", "#680000"),
                       breaks = c("baseline", "plus1", "plus5", "plus10")) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          text = element_text(size = 10), 
          axis.text.y = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.text.x = element_text(face = "bold", hjust = 0),
          strip.background.x = element_blank(),
          axis.title.y = element_text(size = 11),
          plot.margin = unit(c(0, 1, 0, 0), "cm"),
          legend.box.margin = margin(0, -10, -10, -10),
          legend.margin = margin(0, 0, 0, 0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(0.5, "lines"))
#ggsave("figures/smoothed_monthly_biom.jpg", width=7, height=4) 
  
  
# scenario boxplots across taxa
zoop_mean_biom <-  zoop_scenarios |>
    group_by(taxon, scenario, year) |>
    summarise(mean_biom = mean(value)) 

  ggplot(zoop_mean_biom, aes(x=factor(
    scenario,levels=c("baseline","plus1","plus5","plus10")), 
             y = mean_biom, fill=taxon)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#084c61","#db504a","#e3b505"))+
    ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) + xlab("") +
    geom_text(data = subset(zoop_mean_biom, scenario == "plus10" & taxon == "rotifer"),
              aes(x = 4.15, y = max(mean_biom) - 7, label = "*"), 
              size = 10, color = "black") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = c(0.1,0.9),
          legend.title = element_blank(),
          text = element_text(size=10), 
          axis.text.y = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.text.x = element_text(face = "bold",hjust = 0),
          strip.background.x = element_blank(),
          axis.title.y = element_text(size = 11),
          plot.margin = unit(c(0, 1, 0, 0), "cm"),
          legend.box.margin = margin(0,-10,-15,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_zoop_biom_scenario_boxplot.jpg", width=7, height=4) 
  
# cannot do nonparametric tests that identify interactions between scenario and taxa bc scenarios lack independence (i.e., shared env influences)
# so let's try mixed-effects model
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
  
# Mixed-effects model with fixed effects for scenario and taxon, random intercept for period
model <- lmer(mean_biom ~ scenario * taxon + (1 | year), 
              data = zoop_mean_biom)

summary(model) #plus 10 has a positive effect on rotifer biomass

install.packages("emmeans")
library(emmeans)

# Pairwise comparisons for scenarios within each taxon
emmeans(model, pairwise ~ scenario | taxon)
# rotifer biom for plus 10 is sig greater than in other scenarios
   
#-------------------------------------------------------------------------#
# playing around with some other visualizations
ggplot(zoop_mean_biom, aes(x = year, y = mean_biom, color = scenario)) +
  geom_point(size=2) + geom_line(size=1) +
  facet_wrap(~taxon) +
  labs(y = expression("Biomass (" * mu * " g L"^{-1}*")"), x = "") +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=10), 
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 11),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.box.margin = margin(0,-10,-10,-10),
        legend.margin=margin(-25,0,10,0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(
          fill = "white"))
#ggsave("figures/zoop_annual_biom_scenario_lineplot.jpg", width=7, height=4) 

# line plots for each taxa/scenario
ggplot(zoop_mean_biom, aes(x = year, y = mean_biom,  color = scenario)) +
  geom_line() +
  facet_grid(taxon ~ factor(scenario,levels=c("baseline","plus1",
                                              "plus5","plus10"))) +
  labs(y = expression("Biomass (" * mu * " g L"^{-1}*")"), x = "") +
  theme_bw() +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=10), 
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(face = "bold",hjust = 0),
        strip.background = element_blank(),
        axis.title.y = element_text(size = 11),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.box.margin = margin(0,-10,-10,-10),
        legend.margin=margin(-25,0,10,0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(
          fill = "white"))
#ggsave("figures/zoop_annual_biom_taxa_timing.jpg", width=7, height=4) 
  
# difference (value - mean) for each scenario and taxa 
  ggplot(zoop_scenarios,
         aes(x=DateTime, y = diff, color=scenario)) +
    geom_line() +
    facet_wrap(~taxon, scales = "free_y", nrow=3)+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                       breaks = c("baseline","plus1","plus5","plus10")) +
    scale_x_date(expand = c(0,0), date_breaks = "1 year", 
                 date_labels = "%Y") +
    ylab("Difference from mean") + xlab("") +
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
          legend.box.margin = margin(0,-10,-15,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_zoop_diff_from_mean_timeseries.jpg", width=7, height=4) 
  
#reverse axes and summarize by year
 diff_zoops <- zoop_scenarios |>
   group_by(taxon, year, scenario) |>
   summarise(mean_diff = mean(diff, na.rm = TRUE), 
                   sd = sd(diff, na.rm = TRUE)) 
 
 ggplot(diff_zoops) + 
   geom_point(aes(x=mean_diff, 
                  y=factor(scenario,levels=c("baseline","plus1",
                                             "plus5","plus10")), 
                           color=scenario), size=3) +
   theme_bw() + xlab("Difference from mean") + ylab("") +
   facet_wrap(~taxon, scales="free_x") + 
   scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
   geom_vline(xintercept = 0, linetype = "dashed") +
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         legend.background = element_blank(),
         legend.position = "right",
         text = element_text(size=10), 
         axis.text.x = element_text(angle=90),
         panel.border = element_rect(colour = "black", fill = NA),
         strip.background.x = element_blank(),
         plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
         legend.margin = margin(c(-10,-1,-10,-10)),
         panel.spacing.x = unit(0.1, "in"),
         panel.background = element_rect(
           fill = "white"),
         panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_mean_diff_allyears.jpg", width=7, height=4)
  
#create a combined phyto df with all scenarios
#  phyto_scenarios <-  mget(c("all_phytos_baseline","all_phytos_plus1",
#                            "all_phytos_plus5", "all_phytos_plus10")) |>
#    setNames(paste0(scenario)) |>
#    bind_rows(.id = "scenario") |>
#    relocate(scenario, .after = last_col())
  #write.csv(phyto_scenarios, "./analysis/data/phyto_scenarios.csv", row.names = F)
  
  phyto_scenarios <-read.csv("analysis/data/phyto_scenarios.csv") |>
    mutate(DateTime = as.Date(DateTime)) |>
    filter(DateTime >= "2015-07-07")
  
#order phytos
  phyto_scenarios$taxon <- factor(phyto_scenarios$taxon, 
                                  levels = c("cyano","green","diatom"))
  
  ggplot(data = subset(phyto_scenarios, scenario %in% c("baseline","plus10")),
         aes(x=DateTime, y = value, color=taxon)) +
    geom_area(aes(color = taxon, fill = taxon),
              position = "fill", 
              stat = "identity") +
    facet_wrap(~scenario, scales = "free_x")+
    scale_color_manual(values = c("cyan","green","brown4"))+
    scale_fill_manual(values = c("cyan","green","brown4"))+
    scale_x_date(expand = c(0,0), date_breaks = "1 year", 
                 date_labels = "%Y") +
    scale_y_continuous(expand = c(0,0))+
    xlab("") + ylab("Relative density") +
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
  #ggsave("figures/BVR_relative_phyto_scenarios.jpg", width=7, height=4) 
  
  
  phyto_mean_biom <-  phyto_scenarios |>
    group_by(taxon, scenario, year) |>
    summarise(mean_biom = mean(value)) 
  
  # line plots for each taxa/scenario
  ggplot(phyto_mean_biom, aes(x = year, y = mean_biom,  color = scenario)) +
    geom_line() +
    facet_grid(taxon ~ factor(scenario,levels=c("baseline","plus1",
                                                "plus5","plus10")),
               scales = "free_y") +
    labs(y = expression("Biomass (" * mu * " g L"^{-1}*")"), x = "") +
    theme_bw() +
    scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                       breaks = c("baseline","plus1","plus5","plus10")) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          text = element_text(size=10), 
          axis.text.y = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.text = element_text(face = "bold",hjust = 0),
          strip.background = element_blank(),
          axis.title.y = element_text(size = 11),
          plot.margin = unit(c(0, 1, 0, 0), "cm"),
          legend.box.margin = margin(0,-10,-10,-10),
          legend.margin=margin(-25,0,10,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"))
  #ggsave("figures/phyto_annual_biom_taxa_timing.jpg", width=7, height=4) 
  
#------------------------------------------------------------------------#
# density plots
  
  #read in zoop scenarios df
  zoop_scenarios <- read.csv("analysis/data/zoop_scenarios.csv") |>
    dplyr::group_by(taxon, year, scenario) |>
    dplyr::mutate(max_doy = doy[which.max(value)],
                  max_value = max(value)) |>
    dplyr::filter(DateTime >= "2016-01-01" &
                    DateTime < "2022-01-01") #filtering out the 2 partial years
  
# median doy across all years
  zoop_scenarios |>
    dplyr::group_by(taxon) |>
    dplyr::mutate(bl_median = median(max_doy[scenario == "baseline"])) |>
    dplyr::ungroup() |>
    dplyr::group_by(taxon, year,scenario) |> 
    dplyr::summarise(
      median_doy = median(max_doy),
      bl_median = first(bl_median)) |>
  ggplot(aes(x = median_doy, y = as.factor(scenario), 
             group = as.factor(scenario))) +
    geom_density_ridges(aes(fill = as.factor(scenario))) +
    scale_fill_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    scale_y_discrete(limits = scenario) + xlab("max doy") +
    geom_vline(aes(xintercept=bl_median), 
               linetype='dashed', col = 'black') +
    facet_wrap(~taxon, ncol=3, scales = "free_x") + ylab("") +
    theme_bw() + guides(fill = "none") +
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
#ggsave("figures/zoop_density_plots_max_doy_allyears.jpg", width=7, height=4)

# max biomass values for all years
 zoop_scenarios |>
   dplyr::group_by(taxon) |>
   dplyr::mutate(bl_median = median(max_value[scenario == "baseline"])) |>
   dplyr::ungroup() |>
   dplyr::group_by(taxon, year, scenario) |> 
   dplyr::summarise(
     max_biom = max(max_value),
     bl_median = first(bl_median)) |>
   ggplot(aes(x = max_biom, y = as.factor(scenario), 
              group = as.factor(scenario))) +
   geom_density_ridges(aes(fill = as.factor(scenario))) +
   scale_fill_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10")) +
   scale_y_discrete(limits = scenario) + xlab("max biomass") +
   geom_vline(aes(xintercept=bl_median), 
              linetype='dashed', col = 'black') +
   facet_wrap(~taxon, ncol=3, scales = "free_x") + ylab("") +
   theme_bw() + guides(fill = "none") +
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
 #ggsave("figures/zoop_density_plots_max_biomass_allyears.jpg", width=7, height=4)
  
#----------------------------------------------------------------#
# KW tests for max biomass differences across scenarios for each taxa 
zoop_max_biom <- zoop_scenarios |>
   dplyr::group_by(taxon, year, scenario) |> 
   dplyr::select(taxon, year, scenario, value, max_doy) |>
   dplyr::summarise(max_doy = median(max_doy),
                    max_val = max(value))

kruskal.test(zoop_max_biom$max_val[zoop_max_biom$taxon=="cladoceran"]~
               zoop_max_biom$scenario[zoop_max_biom$taxon=="cladoceran"])

kruskal.test(zoop_max_biom$max_val[zoop_max_biom$taxon=="copepod"]~
               zoop_max_biom$scenario[zoop_max_biom$taxon=="copepod"])

kruskal.test(zoop_max_biom$max_val[zoop_max_biom$taxon=="rotifer"]~
               zoop_max_biom$scenario[zoop_max_biom$taxon=="rotifer"])
 # no significant differences in max biomass across scenarios for any of the taxa
 

# just visualize the median density for all years
  zoop_scenarios |>
    dplyr::group_by(taxon) |>
    dplyr::mutate(bl_median = median(max_doy[scenario == "baseline"])) |>
    dplyr::ungroup() |>
    dplyr::group_by(taxon,scenario) |> 
    dplyr::summarise(
      median_doy = median(max_doy),
      bl_median = first(bl_median)) |>
    dplyr::mutate(scenario = factor(scenario, 
                                    levels = c("baseline", "plus1",
                                               "plus5", "plus10"))) |>
  ggplot(aes(x = median_doy, y = as.factor(scenario), group = as.factor(scenario))) +
    geom_point(aes(color = as.factor(scenario)), size=3) +
    scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    geom_vline(aes(xintercept = bl_median), linetype = 'dashed', col = 'black') +
    facet_wrap(~taxon, ncol=3, scales = "free_x") + ylab("") +
    theme_bw() + guides(fill = "none") +
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
  #ggsave("figures/zoop_density_plots_max_doy_allyear_median.jpg", width=7, height=4)
  
# also plot the doy as a point for each year
  ggplot(zoop_max_biom, aes(x = max_doy, y = max_val, 
             color = as.factor(scenario))) +
    geom_point(aes(fill = as.factor(scenario))) +
    scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    xlab("Day of year") + ylab("Maximum value") +
    facet_wrap(~taxon, ncol=3, scales = "free_y") +
    theme_bw() + guides(fill = "none") +
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
  #ggsave("figures/zoop_max_val_vs_doy.jpg", width=7, height=4)
  
  #read in phyto scenarios df
  phyto_scenarios <- read.csv("analysis/data/phyto_scenarios.csv") |>
    dplyr::group_by(taxon, year, scenario) |>
    dplyr::mutate(max_doy = doy[which.max(value)]) |>
    dplyr::filter(DateTime >= "2016-01-01" &
                    DateTime < "2022-01-01") #filtering out the 2 partial years
  
  # median doy across all years
  phyto_scenarios |>
    dplyr::group_by(taxon) |>
    dplyr::mutate(bl_median = median(max_doy[scenario == "baseline"])) |>
    dplyr::ungroup() |>
    dplyr::group_by(taxon, year, scenario) |> # drop year if you want median across all years
    dplyr::summarise(
      median_doy = median(max_doy),
      bl_median = first(bl_median)) |>
    ggplot(aes(x = median_doy, y = as.factor(scenario), 
               group = as.factor(scenario))) +
    geom_density_ridges(aes(fill = as.factor(scenario))) +
    scale_fill_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    scale_y_discrete(limits = scenario) + xlab("max doy") +
    geom_vline(aes(xintercept=bl_median), 
               linetype='dashed', col = 'black') +
    facet_wrap(~taxon, ncol=3, scales = "free_x") + ylab("") +
    theme_bw() + guides(fill = "none") +
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
#ggsave("figures/phyto_density_plots_max_doy_allyears.jpg", width=7, height=4)
  
  # just viusalize the median density for all years
  phyto_scenarios |>
    dplyr::group_by(taxon) |>
    dplyr::mutate(bl_median = median(max_doy[scenario == "baseline"])) |>
    dplyr::ungroup() |>
    dplyr::group_by(taxon,scenario) |> 
    dplyr::summarise(
      median_doy = median(max_doy),
      bl_median = first(bl_median)) |>
    dplyr::mutate(scenario = factor(scenario, 
                                    levels = c("baseline", "plus1",
                                               "plus5", "plus10"))) |>
    ggplot(aes(x = median_doy, y = as.factor(scenario), group = as.factor(scenario))) +
    geom_point(aes(color = as.factor(scenario)), size=3) +
    scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                       breaks = c("baseline","plus1","plus5","plus10")) +
    geom_vline(aes(xintercept = bl_median), linetype = 'dashed', col = 'black') +
    facet_wrap(~taxon, ncol=3, scales = "free_x") + ylab("") +
    theme_bw() + guides(fill = "none") +
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
  #ggsave("figures/phyto_max_doy_allyear_median.jpg", width=7, height=4)
   
#------------------------------------------------------------------------#
# effect size fig for zoops
  zoop_scenarios_summary <- zoop_scenarios |>
    dplyr::group_by(taxon, year, scenario) |>
    dplyr::summarise(mean = mean(value),
                     sd = sd(value),
                     size = n()) 
  
# function to calculate the pooled sd
  pooled_sd <- function(group_sizes, group_sds) {
    # Ensure that the inputs are of the same length
    if (length(group_sizes) != length(group_sds)) {
      stop("group_sizes and group_sds must be of the same length.")
    }
    
    # Calculate pooled standard deviation
    sqrt(
      sum((group_sizes - 1) * (group_sds^2)) / 
        (sum(group_sizes) - length(group_sizes))
    )
  }
  
zoop_effect_size <- zoop_scenarios_summary |>
  dplyr::group_by(taxon, year) |> 
  dplyr::summarise(plus1 = (mean[scenario=="plus1"] - 
                     mean[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]),
                                 group_sd = c(sd[scenario=="baseline"],
                                              sd[scenario=="plus1"])),
                   plus5 = (mean[scenario=="plus5"] - 
                                 mean[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]), 
                               group_sd = c(sd[scenario=="baseline"],
                                            sd[scenario=="plus5"])),
                   plus10 = (mean[scenario=="plus10"] - 
                                 mean[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]),
                               group_sd = c(sd[scenario=="baseline"],
                                            sd[scenario=="plus10"]))) |>
  tidyr::pivot_longer(cols = -c(taxon,year),
                      names_to = "scenario",
                      values_to = "value") |> dplyr::ungroup() |> 
  dplyr::group_by(taxon,scenario) |>
  dplyr::mutate(sd = sd(value)) 

# plot median effect sizes for each scenario  
zoop_effect_size |> 
  dplyr::group_by(taxon, scenario, sd) |> 
  dplyr::summarise(median = median(value)) |> 
  ggplot() + geom_point(aes(x=median, y=factor(scenario,levels=c("plus1","plus5","plus10")), 
                             color=scenario), size=3) +
  theme_bw() + xlab("Effect Size") + ylab("") +
  facet_wrap(~taxon, scales="free_x") + 
  scale_color_manual("", values = c("#c6a000","#c85b00","#680000"),
                     breaks = c("plus1","plus5","plus10"),
                     labels = c("+1C","+5C","+10C")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(y = factor(scenario, levels = c("plus1", "plus5", "plus10")), 
                     xmin = median - sd, xmax = median + sd), 
                 height = 0.2) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "right",
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-1,-10,-10)),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_scenario_effect_size_sd.jpg", width=7, height=4)

# KW tests for effect size across scenarios for each taxa
annual_effect_size <- zoop_effect_size |> 
  dplyr::group_by(taxon, scenario, sd) |> 
  dplyr::summarise(median = median(value)) 

kruskal.test(annual_effect_size$median[annual_effect_size$taxon=="cladoceran"]~
               annual_effect_size$scenario[annual_effect_size$taxon=="cladoceran"])

kruskal.test(annual_effect_size$median[annual_effect_size$taxon=="copepod"]~
               annual_effect_size$scenario[annual_effect_size$taxon=="copepod"])

kruskal.test(annual_effect_size$median[annual_effect_size$taxon=="rotifer"]~
               annual_effect_size$scenario[annual_effect_size$taxon=="rotifer"])
# no significant differences in effect size across scenarios for any of the taxa


# effect size fig for phytos
phyto_scenarios_summary <- phyto_scenarios |>
  dplyr::group_by(taxon, year, scenario) |>
  dplyr::summarise(mean = mean(value),
                   sd = sd(value),
                   size = n()) 

phyto_effect_size <- phyto_scenarios_summary |>
  dplyr::group_by(taxon, year) |> 
  dplyr::summarise(plus1 = (mean[scenario=="plus1"] - 
                              mean[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]),
                               group_sd = c(sd[scenario=="baseline"],
                                            sd[scenario=="plus1"])),
                   plus5 = (mean[scenario=="plus5"] - 
                              mean[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]), 
                               group_sd = c(sd[scenario=="baseline"],
                                            sd[scenario=="plus5"])),
                   plus10 = (mean[scenario=="plus10"] - 
                               mean[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]),
                               group_sd = c(sd[scenario=="baseline"],
                                            sd[scenario=="plus10"]))) |>
  tidyr::pivot_longer(cols = -c(taxon,year),
                      names_to = "scenario",
                      values_to = "value") |> dplyr::ungroup() |> 
  dplyr::group_by(taxon,scenario) |>
  dplyr::mutate(sd = sd(value)) 

# plot median effect sizes for each scenario  
phyto_effect_size |> 
  dplyr::group_by(taxon, scenario, sd) |> 
  dplyr::summarise(median = median(value)) |> 
  ggplot() + geom_point(aes(x=median, y=factor(scenario,levels=c("plus1","plus5","plus10")), 
                            color=scenario), size=3) +
  theme_bw() + xlab("Effect Size") + ylab("") +
  facet_wrap(~taxon, scales="free_x") + 
  scale_color_manual("", values = c("#c6a000","#c85b00","#680000"),
                     breaks = c("plus1","plus5","plus10"),
                     labels = c("+1C","+5C","+10C")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(y = factor(scenario, levels = c("plus1", "plus5", "plus10")), 
                     xmin = median - sd, xmax = median + sd), 
                 height = 0.2) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "right",
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-1,-10,-10)),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/phyto_scenario_effect_size_sd.jpg", width=7, height=4)