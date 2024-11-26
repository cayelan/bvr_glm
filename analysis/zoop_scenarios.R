# Plankton air temp scenarios
# 22 August 2024

devtools::install_github("eliocamp/tagger")
pacman::p_load(ggplot2,ggridges,dplyr, ARTool, FSA, egg, tagger)

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
  geom_line(data=subset(all_zoops_baseline,DateTime >= as.Date("2015-07-07")),
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=subset(all_zoops_plus1,DateTime >= as.Date("2015-07-07")),
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=subset(all_zoops_plus5,DateTime >= as.Date("2015-07-07")),
            aes(DateTime, value, color = "+5C")) +
  geom_line(data=subset(all_zoops_plus10,DateTime >= as.Date("2015-07-07")),
            aes(DateTime, value, color = "+10C")) +
  geom_point(data=subset(all_zoops_obs,DateTime >= as.Date("2015-07-07")),
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
  geom_line(data=subset(all_zoops_baseline,year %in% 2016:2021),
            aes(doy, annual_prop, color = "+0C")) +
  geom_line(data=subset(all_zoops_plus1, year %in% 2016:2021),
            aes(doy, annual_prop, color = "+1C")) +
  geom_line(data=subset(all_zoops_plus5, year %in% 2016:2021),
            aes(doy, annual_prop, color = "+5C")) +
  geom_line(data=subset(all_zoops_plus10, year %in% 2016:2021),
            aes(doy, annual_prop, color = "+10C")) +
  facet_wrap(~year, scales="free_y", strip.position = "top") + 
  ylab("Zooplankton annual proportion") +
  theme_bw() + xlab("") + guides(color = guide_legend(nrow = 1)) +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("+0C","+1C","+5C","+10C")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-10,-15,-10)),
        legend.key = element_rect(fill = "transparent"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/proportional_zoop_scenario_airtemp_1_5_10.jpg", width=6, height=6)

# plot phytos
ggplot() +
  geom_line(data=subset(all_phytos_baseline, DateTime >= as.Date("2015-07-07")),
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=subset(all_phytos_plus1, DateTime >= as.Date("2015-07-07")),
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=subset(all_phytos_plus5, DateTime >= as.Date("2015-07-07")),
            aes(DateTime, value, color = "+5C")) +
  geom_line(data=subset(all_phytos_plus10, DateTime >= as.Date("2015-07-07")),
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
  geom_line(data=subset(all_phytos_baseline, year %in% 2016:2021),
            aes(doy, annual_prop, color = "+0C")) +
  geom_line(data=subset(all_phytos_plus1, year %in% 2016:2021),
            aes(doy, annual_prop, color = "+1C")) +
  geom_line(data=subset(all_phytos_plus5, year %in% 2016:2021),
            aes(doy, annual_prop, color = "+5C")) +
  geom_line(data=subset(all_phytos_plus10, year %in% 2016:2021),
            aes(doy, annual_prop, color = "+10C")) +
  facet_wrap(~year, scales="free_y", strip.position = "top") + 
  theme_bw() + xlab("") + guides(color = guide_legend(nrow = 1)) +
  ylab("Phytoplankton annual proportion") +
  scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                     breaks = c("+0C","+1C","+5C","+10C")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.margin = margin(c(-10,-10,-15,-10)),
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
  geom_line(data=subset(chla_baseline, Depth %in% 0 & 
                          DateTime >= as.Date("2015-07-07")),
            aes(DateTime, PHY_tchla, color = "+0C")) +
  geom_line(data=subset(chla_plus1, Depth %in% 0 & 
                          DateTime >= as.Date("2015-07-07")),
            aes(DateTime, PHY_tchla, color = "+1C")) +
  geom_line(data=subset(chla_plus5, Depth %in% 0 & 
                          DateTime >= as.Date("2015-07-07")),
            aes(DateTime, PHY_tchla, color = "+5C")) +
  geom_line(data=subset(chla_plus10, Depth %in% 0 & 
                          DateTime >= as.Date("2015-07-07")),
            aes(DateTime, PHY_tchla, color = "+10C")) +
  geom_point(data=subset(chla_obs, Depth %in% 0.1 & 
                           DateTime >= as.Date("2015-07-07")),
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
#                   relocate(scenario, .after = last_col()) |>
#  filter(DateTime >= as.Date("2015-07-07"))
  #write.csv(zoop_scenarios, "./analysis/data/zoop_scenarios.csv", row.names = F)

zoop_scenarios <-read.csv("analysis/data/zoop_scenarios.csv") |>
  mutate(DateTime = as.Date(DateTime)) |>
  filter(DateTime >= "2015-07-07") |>
  group_by(taxon, scenario) |>
  mutate(mean_biom = mean(value)) |>
  ungroup() |>
  mutate(diff = value - mean_biom)

# relative zoop density for baseline vs. plus 10
area <-  ggplot(data = subset(zoop_scenarios, scenario %in% c("baseline","plus10")),
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
  tag_facets(tag_pool = c("a","b")) +
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
        text = element_text(size=9), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        strip.background = element_blank(),
        axis.title.y = element_text(size = 10),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.box.margin = margin(0,-10,-10,-10),
        legend.margin=margin(0,0,0,0),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing = unit(0.5, "lines"))
  
# proportion boxplots for each scenario
mean_proportions <- zoop_scenarios |>
    group_by(DateTime, scenario) |>
    mutate(proportion = value / sum(value)) |>
    group_by(taxon, scenario, DateTime) |>
    summarise(mean_proportion = mean(proportion)) 

box <- ggplot(data = subset(mean_proportions, scenario %in% c("baseline","plus10")),
              aes(x=taxon, y = mean_proportion, fill=scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#00603d","#680000"))+
  scale_y_continuous(expand = c(0,0))+
  xlab("") + ylab("Relative biomass") +
  tag_facets(tag_pool = c("c")) +
  guides(color= "none",
         fill = guide_legend(ncol=3)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size=9), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0, 0.1, 0, -0.8), "cm"),
        legend.box.margin = margin(0,-10, 5,-10),
        legend.margin=margin(0,0,0,0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing = unit(0.5, "lines"))

p <- egg::ggarrange(area,box, nrow=1, widths = c(2,1))
#ggsave("figures/BVR_relative_zoop_scenarios.jpg", p, width=5, height=2) 
  
# numbers for results text
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="cladoceran" &
                                        mean_proportions$scenario=="baseline"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="baseline"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="baseline"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="cladoceran" &
                                        mean_proportions$scenario=="plus10"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="plus10"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="plus10"])


# biomass for each year + taxa
  ggplot(zoop_scenarios,
         aes(x=DateTime, y = value, color=scenario)) +
    geom_line() +
    facet_wrap(~taxon, scales = "free_y", nrow=3)+
    scale_color_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                       breaks = c("baseline","plus1","plus5","plus10")) +
    scale_x_date(expand = c(0,0), date_breaks = "1 year", 
                 date_labels = "%Y") +
    ylab(expression("Biomass (mmol m"^{3}*")")) + xlab("") +
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
  ggplot(data = subset(zoop_scenarios, year %in% 2016:2021),
         aes(x = factor(
    scenario,levels=c("baseline","plus1","plus5","plus10")),
    y = value, fill = taxon)) +
    geom_boxplot() + facet_wrap(~year, scales = "free_y") +
    labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
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
      group = interaction(taxon, scenario))) + 
    geom_smooth(method = "loess") +
    facet_wrap(~taxon, nrow=1) +
    scale_x_discrete(labels = c("Jan","","Mar","","May","","Jul","","Sep","","Nov","")) +
    labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
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
  
#same but panels for each year
  zoop_scenarios |>
    mutate(month = lubridate::month(DateTime)) |>
    group_by(taxon, scenario, month, year) |>
    filter(year %in% c(2016:2021)) |>
    summarise(monthly_biom = mean(value), .groups = "drop") |>
    ggplot(aes(x = factor(month), y = monthly_biom, 
      color = factor(scenario, levels = c("baseline", "plus1", "plus5", "plus10")),
      group = interaction(taxon, scenario))) + 
    geom_smooth(method = "loess") +
    facet_grid(taxon ~ year, scales = "free_y") +
    scale_x_discrete(labels = c("Jan","","","","May","","","","Sep","","","")) +
    labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
    scale_color_manual( "", 
      values = c("#00603d", "#c6a000", "#c85b00", "#680000"),
      breaks = c("baseline", "plus1", "plus5", "plus10") ) +
    theme(
      panel.grid.major = element_blank(), 
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
      strip.text.y = element_text(face = "bold", angle = 270, hjust = 0.5), 
      strip.background.x = element_blank(),
      strip.background.y = element_blank(),
      axis.title.y = element_text(size = 11),
      plot.margin = unit(c(0, 1, 0, 0), "cm"),
      legend.box.margin = margin(0, -10, -10, -10),
      legend.margin = margin(0, 0, 0, 0),
      panel.spacing.x = unit(0.2, "in"),
      panel.background = element_rect(fill = "white"),
      panel.spacing = unit(0.5, "lines"))
#ggsave("figures/smoothed_monthly_biom_multipanel.jpg", width=7, height=4) 
  
  
# scenario boxplots across taxa
zoop_mean_biom <-  zoop_scenarios |>
    group_by(taxon, scenario, year) |>
    summarise(mean_biom = mean(value)) 

  ggplot(zoop_mean_biom, aes(x=factor(
    scenario,levels=c("baseline","plus1","plus5","plus10")), 
             y = mean_biom, fill=taxon)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#084c61","#db504a","#e3b505"))+
    labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
    geom_text(data = subset(zoop_mean_biom, scenario == "plus10" & taxon == "rotifer"),
              aes(x = c(1.24,NA,2.24,NA,3.24,NA,4.24,NA), 
                  y = c(30,0,27,0,36,0,58,0), 
                  label = c("ab","","a","","a","","b","")), 
              size = 5, color = "black") +
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
  
# numbers for results text
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                  zoop_mean_biom$taxon=="rotifer"]) - 
    mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                    zoop_mean_biom$taxon=="rotifer"])) /
   ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                    zoop_mean_biom$taxon=="rotifer"]) + 
    mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                    zoop_mean_biom$taxon=="rotifer"])) / 2) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="rotifer"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                      zoop_mean_biom$taxon=="rotifer"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="rotifer"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                        zoop_mean_biom$taxon=="rotifer"])) / 2) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="rotifer"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                      zoop_mean_biom$taxon=="rotifer"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="rotifer"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                        zoop_mean_biom$taxon=="rotifer"])) / 2) *100
  
  #clads
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="cladoceran"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="cladoceran"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="cladoceran"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="cladoceran"])) / 2) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="cladoceran"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                      zoop_mean_biom$taxon=="cladoceran"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="cladoceran"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                        zoop_mean_biom$taxon=="cladoceran"])) / 2) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="cladoceran"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                      zoop_mean_biom$taxon=="cladoceran"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="cladoceran"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                        zoop_mean_biom$taxon=="cladoceran"])) / 2) *100
  
  #copes
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="copepod"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="copepod"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="copepod"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="copepod"])) / 2) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="copepod"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                      zoop_mean_biom$taxon=="copepod"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="copepod"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                        zoop_mean_biom$taxon=="copepod"])) / 2) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="copepod"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                      zoop_mean_biom$taxon=="copepod"])) /
    ((mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                      zoop_mean_biom$taxon=="copepod"]) + 
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                        zoop_mean_biom$taxon=="copepod"])) / 2) *100
  
mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline"])
mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1"])
mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10"])
  
# Aligned Rank Transform (ART)
  
zoop_annual_biom <-  zoop_scenarios |>
  group_by(taxon, scenario, year) |>
  summarise(mean_biom = mean(value)) |>
  mutate(taxon = as.factor(taxon),
         scenario = as.factor(scenario),
         year = as.factor(year))
  
art_model <- art(mean_biom ~ scenario * taxon + (1 | year), 
                 data = zoop_annual_biom)
anova(art_model)
# both main effects and interaction is significant

# post-hoc tests
# Test main effect of scenario - yes difference
kruskal_scenario <- kruskal.test(mean_biom ~ scenario, data = zoop_annual_biom)

# Test main effect of taxon - no difference
kruskal_taxon <- kruskal.test(mean_biom ~ taxon, data = zoop_annual_biom)

# Perform pairwise comparisons for scenario
pairwise_scenario <- pairwise.wilcox.test(zoop_annual_biom$mean_biom, 
                                          zoop_annual_biom$scenario, 
                                          p.adjust.method = "bonferroni")
# plus 1 and baseline are different than plus 10

# get rank-transformed values given that ART identified sig differences among taxa
zoop_annual_biom$ranked_mean_biom <- residuals(art_model)

# does taxon biomass differ among scenarios? - only rotifers!
kruskal_rot_scenario <- kruskal.test(zoop_annual_biom$mean_biom[zoop_annual_biom$taxon=="rotifer"] ~ 
                                       zoop_annual_biom$scenario[zoop_annual_biom$taxon=="rotifer"])

kruskal_clad_scenario <- kruskal.test(zoop_annual_biom$mean_biom[zoop_annual_biom$taxon=="cladoceran"] ~ 
                                        zoop_annual_biom$scenario[zoop_annual_biom$taxon=="cladoceran"])

kruskal_cope_scenario <- kruskal.test(zoop_annual_biom$mean_biom[zoop_annual_biom$taxon=="copepod"] ~ 
                                        zoop_annual_biom$scenario[zoop_annual_biom$taxon=="copepod"])

# Perform pairwise comparisons for scenario
pairwise_scenario_rot <- pairwise.wilcox.test(zoop_annual_biom$mean_biom[zoop_annual_biom$taxon=="rotifer"], 
                                              zoop_annual_biom$scenario[zoop_annual_biom$taxon=="rotifer"], 
                                              p.adjust.method = "bonferroni")
# plus 1 and 5 are different than plus 10 (not baseline)

#-------------------------------------------------------------------------#
# playing around with some other visualizations
ggplot(zoop_mean_biom, aes(x = year, y = mean_biom, color = scenario)) +
  geom_point(size=2) + geom_line(size=1) +
  facet_wrap(~taxon, scales="free_y") +
  labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
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
  labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
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
  
area <- ggplot(data = subset(phyto_scenarios, scenario %in% c("baseline","plus10")),
         aes(x=DateTime, y = value, color=taxon)) +
    geom_area(aes(color = taxon, fill = taxon),
              position = "fill", 
              stat = "identity") +
    facet_wrap(~scenario, scales = "free_x")+
    scale_color_manual(values = c("cyan","green","brown4"))+
    scale_fill_manual(values = c("cyan","green","brown4"))+
    scale_x_date(expand = c(0,0), 
               breaks = as.Date(c("2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01")),
               date_labels = '%Y') +
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
          strip.background.x = element_blank(),
          axis.title.y = element_text(size = 11),
          plot.margin = unit(c(0, 1, 0, 0), "cm"),
          legend.box.margin = margin(0,-10,-10,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
  
  # proportion boxplots for each scenario
  mean_phyto_proportions <- phyto_scenarios |>
    group_by(DateTime, scenario) |>
    mutate(proportion = value / sum(value)) |>
    group_by(taxon, scenario, DateTime) |>
    summarise(mean_proportion = mean(proportion)) 
  
  box <- ggplot(data = subset(mean_phyto_proportions, scenario %in% c("baseline","plus10")),
                aes(x=taxon, y = mean_proportion, fill=scenario)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#00603d","#680000"))+
    scale_y_continuous(expand = c(0,0))+
    xlab("") + ylab("Relative biomass") +
    tag_facets(tag_pool = c("c")) +
    guides(color= "none",
           fill = guide_legend(ncol=3)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          text = element_text(size=9), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.text.x = element_text(face = "bold",hjust = 0),
          strip.background = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0, 0.1, 0, -0.8), "cm"),
          legend.box.margin = margin(0,-10, 5,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_rect(
            fill = "white"),
          panel.spacing = unit(0.5, "lines"))
  
  p <- egg::ggarrange(area,box, nrow=1, widths = c(2,1))
  #ggsave("figures/BVR_relative_phyto_scenarios.jpg", p, width=5, height=2) 
  
  phyto_mean_biom <-  phyto_scenarios |>
    group_by(taxon, scenario, year) |>
    summarise(mean_biom = mean(value)) 
  
  # line plots for each taxa/scenario
  ggplot(phyto_mean_biom, aes(x = year, y = mean_biom,  color = scenario)) +
    geom_line() +
    facet_grid(taxon ~ factor(scenario,levels=c("baseline","plus1",
                                                "plus5","plus10")),
               scales = "free_y") +
    labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
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
  
  ggplot(phyto_mean_biom, aes(x = year, y = mean_biom, color = scenario)) +
    geom_point(size=2) + geom_line(size=1) +
    facet_wrap(~taxon, scales="free_y") +
    labs(y = expression("Biomass (mmol m"^{3}*")"), x = "") +
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
  #ggsave("figures/phyto_annual_biom_scenario_lineplot.jpg", width=7, height=4) 
  
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
zoop_mag <-  zoop_scenarios |>
    dplyr::group_by(taxon) |>
    dplyr::mutate(bl_mean = mean(value[scenario == "baseline"]),
                  p1_mean = mean(value[scenario == "plus1"]),
                  p5_mean = mean(value[scenario == "plus5"]),
                  p10_mean = mean(value[scenario == "plus10"])) |>
    dplyr::select(DateTime, taxon, value, year, scenario, 
                  bl_mean, p1_mean, p5_mean, p10_mean)

  ggplot(zoop_mag, aes(x = value, y = as.factor(scenario), 
             group = as.factor(scenario))) +
    geom_density_ridges(aes(fill = as.factor(scenario))) +
    scale_fill_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
    scale_y_discrete(limits = scenario) +
    labs(x = expression("Biomass (mmol m"^{3}*")"), y = "") +
    geom_vline(aes(xintercept=bl_mean), 
               linetype='dashed', col = '#00603d') +
    geom_vline(aes(xintercept=p1_mean), 
               linetype='dashed', col = '#c6a000') +
    geom_vline(aes(xintercept=p5_mean), 
               linetype='dashed', col = '#c85b00') +
    geom_vline(aes(xintercept=p10_mean), 
               linetype='dashed', col = '#680000') +
    facet_wrap(~taxon, ncol=3, scales = "free_x") +
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
#ggsave("figures/zoop_density_plots_biomass_mag.jpg", width=7, height=4)

# numbers for results text
  mean(zoop_timing$mean_doy[zoop_timing$scenario=="baseline" &
                              zoop_timing$taxon=="cladoceran"]) - 
  mean(zoop_timing$mean_doy[zoop_timing$scenario=="plus10" &
                            zoop_timing$taxon=="cladoceran"])
  
  mean(zoop_timing$mean_doy[zoop_timing$scenario=="baseline" &
                              zoop_timing$taxon=="copepod"]) - 
    mean(zoop_timing$mean_doy[zoop_timing$scenario=="plus10" &
                                zoop_timing$taxon=="copepod"])
  
  mean(zoop_timing$mean_doy[zoop_timing$scenario=="baseline" &
                              zoop_timing$taxon=="rotifer"]) - 
    mean(zoop_timing$mean_doy[zoop_timing$scenario=="plus10" &
                                zoop_timing$taxon=="rotifer"])
  
# numbers for results text
   mean(zoop_mag$max_biom[zoop_mag$scenario=="baseline" &
                            zoop_mag$taxon=="cladoceran"]) - 
     mean(zoop_mag$max_biom[zoop_mag$scenario=="plus10" &
                              zoop_mag$taxon=="cladoceran"])
   
   mean(zoop_mag$max_biom[zoop_mag$scenario=="baseline" &
                               zoop_mag$taxon=="copepod"]) - 
     mean(zoop_mag$max_biom[zoop_mag$scenario=="plus10" &
                                 zoop_mag$taxon=="copepod"])
   
   mean(zoop_mag$max_biom[zoop_mag$scenario=="baseline" &
                            zoop_mag$taxon=="rotifer"]) - 
     mean(zoop_mag$max_biom[zoop_mag$scenario=="plus10" &
                              zoop_mag$taxon=="rotifer"])
  
#----------------------------------------------------------------#
# KW tests for max biomass differences across scenarios for each taxa 
kruskal.test(zoop_mag$max_biom[zoop_mag$taxon=="cladoceran"]~
               zoop_mag$scenario[zoop_mag$taxon=="cladoceran"])

kruskal.test(zoop_mag$max_biom[zoop_mag$taxon=="copepod"]~
               zoop_mag$scenario[zoop_mag$taxon=="copepod"])

kruskal.test(zoop_mag$max_biom[zoop_mag$taxon=="rotifer"]~
               zoop_mag$scenario[zoop_mag$taxon=="rotifer"])
 # no significant differences in max biomass across scenarios for any of the taxa
 
# KW tests for timing of max biomass  across scenarios for each taxa 
kruskal.test(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran"]~
               zoop_timing$scenario[zoop_timing$taxon=="cladoceran"])

kruskal.test(zoop_timing$mean_doy[zoop_timing$taxon=="copepod"]~
               zoop_timing$scenario[zoop_timing$taxon=="copepod"])

kruskal.test(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer"]~
               zoop_timing$scenario[zoop_timing$taxon=="rotifer"])
# rotifer timing is sig different

#perform Dunn's Test with Bonferroni correction for p-values
dunnTest(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer"]~
           zoop_timing$scenario[zoop_timing$taxon=="rotifer"],
         method="bonferroni")
# rotifer plus 10 biomass peaks happen earlier than plus 1 or baseline


# just visualize the mean doy for peak biomass across all years
  zoop_scenarios |>
    dplyr::mutate(scenario = factor(scenario, 
                                    levels = c("baseline", "plus1",
                                               "plus5", "plus10"))) |>
  ggplot(aes(x = max_doy, y = as.factor(scenario), 
             fill = as.factor(scenario))) +
    geom_boxplot(aes(fill = as.factor(scenario))) +
    scale_fill_manual("", values = c("#00603d","#c6a000","#c85b00","#680000"),
                      breaks = c("baseline","plus1","plus5","plus10")) +
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
  #ggsave("figures/zoop_boxplots_max_doy.jpg", width=7, height=4)
  
zoop_timing <- zoop_scenarios |>
  group_by(year, taxon, scenario) |>
  summarise(mean_doy = mean(max_doy))
  
# mean doy is not sig different
  kruskal.test(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer"]~
                 zoop_timing$scenario[zoop_timing$taxon=="rotifer"])
  
  kruskal.test(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran"]~
                 zoop_timing$scenario[zoop_timing$taxon=="cladoceran"])
  
  kruskal.test(zoop_timing$mean_doy[zoop_timing$taxon=="copepod"]~
                 zoop_timing$scenario[zoop_timing$taxon=="copepod"])
  
# numbers for results text HERE
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                              zoop_timing$scenario=="baseline"])
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                              zoop_timing$scenario=="plus10"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                              zoop_timing$scenario=="baseline"])
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                              zoop_timing$scenario=="plus10"])
  
    mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                              zoop_timing$scenario=="baseline"])
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                              zoop_timing$scenario=="plus10"])
  
# also plot the doy as a point for each year
  ggplot(zoop_scenarios, aes(x = max_doy, y = max_value, 
             color = as.factor(scenario))) +
    geom_point(aes(fill = as.factor(scenario))) +
    geom_line() +
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
  
#------------------------------------------------------------------------#
# effect size fig for zoops
  zoop_scenarios_summary <- zoop_scenarios |>
    dplyr::group_by(taxon, year, scenario) |>
    dplyr::summarise(mean = mean(value),
                     sd = sd(value),
                     doy = doy[which.max(value)],
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

# effect size for change in magnitude of biomass 
zoop_effect_size_magnitude <- zoop_scenarios_summary |>
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
zoop_effect_size_magnitude |> 
  dplyr::group_by(taxon, scenario, sd) |> 
  dplyr::summarise(mean = mean(value)) |> 
  ggplot() + geom_point(aes(x=mean, y=factor(scenario,levels=c("plus1","plus5","plus10")), 
                             color=scenario), size=3) +
  theme_bw() + xlab("Effect Size") + ylab("") +
  facet_wrap(~taxon, scales="free_x") + 
  scale_color_manual("", values = c("#c6a000","#c85b00","#680000"),
                     breaks = c("plus1","plus5","plus10"),
                     labels = c("+1C","+5C","+10C")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(y = factor(scenario, levels = c("plus1", "plus5", "plus10")), 
                     xmin = mean - sd, xmax = mean + sd), 
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
#ggsave("figures/zoop_scenario_effect_size_sd_magnitude.jpg", width=7, height=4)

# kw tests for significant differences in ES across across scenarios
kruskal.test(zoop_effect_size_magnitude$value[zoop_effect_size_magnitude$taxon=="cladoceran"]~
               zoop_effect_size_magnitude$scenario[zoop_effect_size_magnitude$taxon=="cladoceran"])

kruskal.test(zoop_effect_size_magnitude$value[zoop_effect_size_magnitude$taxon=="copepod"]~
               zoop_effect_size_magnitude$scenario[zoop_effect_size_magnitude$taxon=="copepod"])

kruskal.test(zoop_effect_size_magnitude$value[zoop_effect_size_magnitude$taxon=="rotifer"]~
               zoop_effect_size_magnitude$scenario[zoop_effect_size_magnitude$taxon=="rotifer"])
# all taxa have sig different effec sizes

#perform Dunn's Test with Bonferroni correction for p-values
dunnTest(zoop_effect_size_magnitude$value[zoop_effect_size_magnitude$taxon=="cladoceran"]~
           zoop_effect_size_magnitude$scenario[zoop_effect_size_magnitude$taxon=="cladoceran"],
         method="bonferroni")
# clad ES in plus10 is greater than in plus1

dunnTest(zoop_effect_size_magnitude$value[zoop_effect_size_magnitude$taxon=="copepod"]~
           zoop_effect_size_magnitude$scenario[zoop_effect_size_magnitude$taxon=="copepod"],
         method="bonferroni")
# cope ES in plus10 is greater than in plus1

dunnTest(zoop_effect_size_magnitude$value[zoop_effect_size_magnitude$taxon=="rotifer"]~
           zoop_effect_size_magnitude$scenario[zoop_effect_size_magnitude$taxon=="rotifer"],
         method="bonferroni")
# rot ES in plus10 is greater than in plus1

# effect size for timing of zooplankton biomass peaks
zoop_effect_size_timing <- zoop_scenarios_summary |>
  dplyr::group_by(taxon, year) |> 
  dplyr::summarise(plus1 = (doy[scenario=="plus1"] - 
                              doy[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]),
                               group_sd = c(sd[scenario=="baseline"],
                                            sd[scenario=="plus1"])),
                   plus5 = (doy[scenario=="plus5"] - 
                              doy[scenario=="baseline"]) / 
                     pooled_sd(group_sizes = c(size[scenario=="baseline"],
                                               size[scenario=="plus1"]), 
                               group_sd = c(sd[scenario=="baseline"],
                                            sd[scenario=="plus5"])),
                   plus10 = (doy[scenario=="plus10"] - 
                               doy[scenario=="baseline"]) / 
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
zoop_effect_size_timing |> 
  #filter(value >= -11.5) |>
  dplyr::group_by(taxon, scenario, sd) |> 
  dplyr::summarise(mean = mean(value)) |> 
  ggplot() + geom_point(aes(x=mean, y=factor(scenario,levels=c("plus1","plus5","plus10")), 
                            color=scenario), size=3) +
  theme_bw() + xlab("Effect Size") + ylab("") +
  facet_wrap(~taxon, scales="free_x") + 
  scale_color_manual("", values = c("#c6a000","#c85b00","#680000"),
                     breaks = c("plus1","plus5","plus10"),
                     labels = c("+1C","+5C","+10C")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(y = factor(scenario, levels = c("plus1", "plus5", "plus10")), 
                     xmin = mean - sd, xmax = mean + sd), 
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
#ggsave("figures/zoop_scenario_effect_size_sd_timing_points_no_outlier.jpg", width=7, height=4)


# KW tests for effect size across scenarios for each taxa
kruskal.test(zoop_effect_size_timing$value[zoop_effect_size_timing$taxon=="cladoceran"]~
               zoop_effect_size_timing$scenario[zoop_effect_size_timing$taxon=="cladoceran"])

kruskal.test(zoop_effect_size_timing$value[zoop_effect_size_timing$taxon=="copepod"]~
               zoop_effect_size_timing$scenario[zoop_effect_size_timing$taxon=="copepod"])

kruskal.test(zoop_effect_size_timing$value[zoop_effect_size_timing$taxon=="rotifer"]~
               zoop_effect_size_timing$scenario[zoop_effect_size_timing$taxon=="rotifer"])
# effect size not different for any taxa
