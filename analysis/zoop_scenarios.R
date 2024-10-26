# Plankton air temp scenarios
# 22 August 2024

pacman::p_load(ggplot2,ggridges)

scenario <- c("baseline","plus1", "plus5","plus10")

for (i in 1:length(scenario)){

nc_file = paste0("sims/",scenario[i],"/output/output.nc")  
  
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
  
  nc_file = paste0("sims/",scenario[i],"/output/output.nc")  
  
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
  
  nc_file = paste0("sims/",scenario[i],"/output/output.nc")  
  
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
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51","red"),
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
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51"),
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
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51"),
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
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51"),
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
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51","red"),
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
zoop_scenarios <-  mget(c("all_zoops_baseline","all_zoops_plus1",
                 "all_zoops_plus5", "all_zoops_plus10")) %>% 
                   setNames(paste0(scenario)) %>%
                   bind_rows(.id = "scenario") %>%
                   relocate(scenario, .after = last_col())
  #write.csv(zoop_scenarios, "./analysis/output/zoop_scenarios.csv", row.names = F)
  
  
  ggplot(data = zoop_scenarios,
         aes(x=DateTime, y = value, color=taxon)) +
  geom_area(aes(color = taxon, fill = taxon),
            position = "fill", 
            stat = "identity", 
            alpha=0.7) +
  facet_wrap(~scenario, scales = "free")+
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_x_date(expand = c(0,0), date_breaks = "6 months", 
               date_labels = "%b-%Y") +
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
#ggsave("Figures/BVR_relative_zoop_scenarios.jpg", width=7, height=4) 

  #create a combined df with all scenarios
zoop_scenarios <-  mget(c("all_zoops_baseline","all_zoops_plus1",
                 "all_zoops_plus5", "all_zoops_plus10")) %>% 
                   setNames(paste0(scenario)) %>%
                   bind_rows(.id = "scenario") %>%
                   relocate(scenario, .after = last_col())
  #write.csv(zoop_scenarios, "./analysis/data/zoop_scenarios.csv", row.names = F)
  
  
  ggplot(data = zoop_scenarios,
         aes(x=DateTime, y = value, color=taxon)) +
  geom_area(aes(color = taxon, fill = taxon),
            position = "fill", 
            stat = "identity", 
            alpha=0.7) +
  facet_wrap(~scenario, scales = "free")+
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_x_date(expand = c(0,0), date_breaks = "6 months", 
               date_labels = "%b-%Y") +
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
#ggsave("figures/BVR_relative_zoop_scenarios.jpg", width=7, height=4) 

  
#create a combined phyto df with all scenarios
  phyto_scenarios <-  mget(c("all_phytos_baseline","all_phytos_plus1",
                            "all_phytos_plus5", "all_phytos_plus10")) %>% 
    setNames(paste0(scenario)) %>%
    bind_rows(.id = "scenario") %>%
    relocate(scenario, .after = last_col())
  #write.csv(phyto_scenarios, "./analysis/data/phyto_scenarios.csv", row.names = F)
  
  ggplot(data = phyto_scenarios,
         aes(x=DateTime, y = value, color=taxon)) +
    geom_area(aes(color = taxon, fill = taxon),
              position = "fill", 
              stat = "identity", 
              alpha=0.7) +
    facet_wrap(~scenario, scales = "free")+
    scale_color_manual(values = c("cyan","green","brown4"))+
    scale_fill_manual(values = c("cyan","green","brown4"))+
    scale_x_date(expand = c(0,0), date_breaks = "6 months", 
                 date_labels = "%b-%Y") +
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
  
#------------------------------------------------------------------------#
# density plot of max peak for each - 3 panel fig with 4 humps for scenarios
  
  #read in zoop scnearios df
  zoop_scenarios <- read.csv("analysis/data/zoop_scenarios.csv") |>
    dplyr::group_by(year, taxon, scenario) |>
    dplyr::mutate(max_doy = doy[max(value)])
  
  ggplot(data=subset(zoop_scenarios, year %in% "2015"), 
         aes(x = max_doy, y = as.factor(scenario), 
                             group = as.factor(scenario))) +
    geom_density_ridges(aes(fill = as.factor(scenario))) +
    scale_fill_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    scale_y_discrete(limits = scenario) + xlab("max doy") +
    facet_wrap(~taxon, ncol=3) + ylab("") +
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
#ggsave("figures/taxa_density_plots_max_doy_allyears.jpg", width=7, height=4)
  