# Plankton air temp scenarios
# 22 August 2024

devtools::install_github("eliocamp/tagger")
pacman::p_load(ggplot2,ggridges,dplyr, ARTool, 
               FSA, egg, tagger, stringr)

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
    #                    names_pattern = "(...)_(...*)_(..*)$",
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

# quick fig for surface and deep phyto groups
ggplot() +
  geom_line(data=all_phytos_final,
            aes(DateTime, value, color = taxon)) +
  #facet_wrap(~depth, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") +
  ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) +
  scale_color_manual("", values = c("cyan","green","#680000"),
                     breaks = c("cyano","green","diatom")) +
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
#ggsave("figures/phytos_fullwc_baseline.jpg", width=6, height=6)
#ggsave("figures/phytos_0.5_9m_baseline.jpg", width=6, height=6)

#------------------------------------------------------------------------#
# proportional plankton figs for each scenario

zoop_scenarios <-read.csv("analysis/data/zoop_scenarios.csv") |>
  mutate(DateTime = as.Date(DateTime)) |>
  filter(DateTime >= "2015-07-07") |>
  group_by(year, taxon, scenario) |>
  mutate(mean_biom = mean(value)) |>
  ungroup() |>
  mutate(diff = value - mean_biom) 

# relative zoop density for baseline vs. plus 10
area <-  ggplot(data = subset(zoop_scenarios, 
                              scenario %in% c("baseline","plus10") &
                                !taxon %in% c("total")),
         aes(x=DateTime, y = value, color=taxon)) +
  geom_area(aes(fill = taxon, color=taxon),
            position = "fill", 
            stat = "identity") +
  facet_wrap(~scenario, scales = "free_x",
             labeller = labeller(scenario = function(x) str_to_title(x)))+
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"))+
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"),
                    labels = c("Cladoceran","Copepod","Rotifer"))+
  scale_x_date(expand = c(0,0), 
               breaks = as.Date(c("2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01")),
               date_labels = '%Y') +
  scale_y_continuous(expand = c(0,0))+
  tag_facets(tag_pool = c("a","b")) +
  xlab("") + ylab("Relative biomass") +
  guides(color= "none",
         fill = guide_legend(ncol=3)) +
  theme(tagger.panel.tag.text = element_text(color ="white", size=8),
        panel.grid.major = element_blank(), 
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
    mutate(proportion = value / value[taxon=="total"]) |>
    group_by(taxon, scenario, DateTime) |>
    summarise(mean_proportion = mean(proportion)) |>
    filter(!taxon %in% "total")

box <- ggplot(data = subset(mean_proportions, 
                            scenario %in% c("baseline","plus10")),
              aes(x=taxon, y = mean_proportion, fill=scenario)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#147582","#680000"),
                    labels = c("Baseline","Plus10"))+
  scale_y_continuous(expand = c(0,0), limits=c(-0.05,1.05))+
  xlab("") + ylab("Relative biomass") +
  tag_facets(tag_pool = c("c")) +
  guides(color= "none",
         fill = guide_legend(ncol=3)) +
  theme(tagger.panel.tag.text = element_text(size=8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size=9), 
        axis.text.x = element_text(angle=25, vjust=0.6),
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
mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                            zoop_scenarios$scenario=="baseline"])
sd(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                               zoop_scenarios$scenario=="baseline"])
mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                            zoop_scenarios$scenario=="plus1"])
sd(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                          zoop_scenarios$scenario=="plus1"])
mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                            zoop_scenarios$scenario=="plus5"])
sd(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                            zoop_scenarios$scenario=="plus5"])
mean(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                            zoop_scenarios$scenario=="plus10"])
sd(zoop_scenarios$value[zoop_scenarios$taxon=="total" & 
                          zoop_scenarios$scenario=="plus10"])

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

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="cladoceran" &
                                        mean_proportions$scenario=="plus10"]) -
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="cladoceran" &
                                        mean_proportions$scenario=="baseline"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="plus10"]) -
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="baseline"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="plus5"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="plus10"]) -
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="baseline"])

#list of proportions for high and low taxon biomass
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="cladoceran" &
                                        mean_proportions$scenario=="baseline"])
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="cladoceran" &
                                        mean_proportions$scenario=="plus10"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="baseline"])
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="plus5"])
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="plus10"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="baseline"])
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="plus5"])
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="plus10"])


# biomass for each year + taxa
  ggplot(zoop_scenarios,
         aes(x=DateTime, y = value, color=scenario)) +
    geom_line() +
    facet_wrap(~taxon, scales = "free_y", nrow=3)+
    scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                       breaks = c("baseline","plus1","plus5","plus10")) +
    scale_x_date(expand = c(0,0), date_breaks = "1 year", 
                 date_labels = "%Y") +
    ylab(expression("Biomass (mg L"^{-1}*")")) + xlab("") +
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
  ggplot(data = subset(zoop_scenarios, 
                       year %in% 2016:2021 &
                         !taxon %in% "total"),
         aes(x = factor(
    scenario,levels=c("baseline","plus1","plus5","plus10")),
    y = value, fill = taxon)) +
    geom_boxplot() + facet_wrap(~year, scales = "free_y") +
    labs(y = expression("Biomass (mg C L"^{-1}*")"), x = "") +
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
    filter(year %in% c(2016:2021)) |>
    mutate(month = lubridate::month(DateTime)) |>
    group_by(taxon, scenario, month) |>
    summarise(monthly_biom = mean(value), .groups = "drop") |>
    ggplot(aes(x = factor(month), y = monthly_biom, color = factor(
      scenario, levels = c("baseline", "plus1", "plus5", "plus10")),
      group = interaction(taxon, scenario))) + 
    geom_smooth(method = "loess") +
    facet_wrap(~taxon, nrow=1)+ 
    scale_x_discrete(labels = c("Jan","","Mar","", "May", "", "Jul",
                                "","Sep", "","Nov","")) +
    ylab(expression("Biomass (mg C L"^{-1}*")")) + xlab("") +
    scale_color_manual("", values = c("#147582", "#c6a000", "#c85b00", "#680000"),
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
    facet_grid(str_to_title(taxon) ~ year, scales = "free_y") +
    scale_x_discrete(labels = c("Jan","","","","May","","","","Sep","","","")) +
    ylab(expression("Biomass (mg C L"^{-1}*")")) + xlab("") +
    scale_color_manual( "", 
      values = c("#147582", "#c6a000", "#c85b00", "#680000"),
      breaks = c("baseline", "plus1", "plus5", "plus10"),
      labels = c("Baseline","Plus1","Plus5","Plus10")) +
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
    filter(!taxon %in% "total",
           year %in% c(2016:2021)) |>
    group_by(year, taxon, scenario) |>
    summarise(mean_biom = mean(mean_biom))  

  ggplot(data=zoop_mean_biom, aes(x=factor(
    scenario,levels=c("baseline","plus1","plus5","plus10")), 
             y = mean_biom, fill=taxon)) +
    geom_boxplot() + ylim(0,1.27) +
    scale_fill_manual(values = c("#084c61","#db504a","#e3b505"),
                      labels = c("Cladoceran","Copepod","Rotifer"))+
    ylab(expression("Biomass (mg C L"^{-1}*")")) + xlab("") +
    geom_text(data = subset(zoop_mean_biom, scenario == "plus10" & taxon == "rotifer"),
              aes(x = c(1.24,2.24,3.24,4.24,NA,NA), 
                  y = c(0.7,0.8,1,1,0,0), 
                  label = c("a","a","b","b","","")), 
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
  
  #KW tests - does mean biomass differ across scenarios for each taxa?
  kruskal.test(mean_biom ~ scenario, data = 
                 zoop_mean_biom[zoop_mean_biom$taxon=="cladoceran",])
  # no p > 0.05
  kruskal.test(mean_biom ~ scenario, data = 
                 zoop_mean_biom[zoop_mean_biom$taxon=="copepod",])
  # no p > 0.05
  kruskal.test(mean_biom ~ scenario, data = 
                 zoop_mean_biom[zoop_mean_biom$taxon=="rotifer",])
  # yes p < 0.05
  
  #does mean biomass differ across scenarios?
  kruskal.test(mean_biom ~ scenario, data = 
                 zoop_mean_biom)
  # no p > 0.05
  
  # pairwise wilcoxin rank sum tests for rot
  # Get unique scenario names
  scenarios <- unique(zoop_mean_biom$scenario[zoop_mean_biom$taxon == "rotifer"])
  
  # Store results of raw p-values and test statistics
  raw_p_values <- c()
  test_statistics <- c()
  comparisons <- c()  
  
  # Loop through all pairwise combinations of scenarios
  for (i in 1:(length(scenarios) - 1)) {
    for (j in (i + 1):length(scenarios)) {
      # Subset data for the two scenarios
      group1 <- zoop_mean_biom$mean_biom[zoop_mean_biom$scenario == scenarios[i] & zoop_mean_biom$taxon == "rotifer"]
      group2 <- zoop_mean_biom$mean_biom[zoop_mean_biom$scenario == scenarios[j] & zoop_mean_biom$taxon == "rotifer"]
      
      # Run Wilcoxon rank-sum test
      test_result <- wilcox.test(group1, group2, paired = FALSE) 
      
      # Store raw p-values, test statistics (W values), and comparisons
      raw_p_values <- c(raw_p_values, test_result$p.value)
      test_statistics <- c(test_statistics, test_result$statistic)
      comparisons <- c(comparisons, paste(scenarios[i], "vs", scenarios[j]))  # Correct comparison name
    }
  }
  
  # Adjust p-values using Holm's method
  adjusted_p_values <- p.adjust(raw_p_values, method = "holm")
  
  # Combine results into a dataframe
  results_df <- data.frame(
    Comparison = comparisons,  # Use the comparisons vector here
    W_Statistic = test_statistics,
    Raw_P_Value = raw_p_values,
    Adjusted_P_Value = adjusted_p_values
  )
  
  print(results_df)
  
  
  # numbers for results text
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                  zoop_mean_biom$taxon=="rotifer"]) - 
    mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                    zoop_mean_biom$taxon=="rotifer"])) /
    mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                    zoop_mean_biom$taxon=="rotifer"])  *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                   zoop_mean_biom$taxon=="rotifer"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="rotifer"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="rotifer"]) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                   zoop_mean_biom$taxon=="rotifer"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="rotifer"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="rotifer"]) *100
  
  #clads
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="cladoceran"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="cladoceran"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="cladoceran"]) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                   zoop_mean_biom$taxon=="cladoceran"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="cladoceran"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="cladoceran"]) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                   zoop_mean_biom$taxon=="cladoceran"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="cladoceran"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="cladoceran"]) *100 
  #copes
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus10" &
                                   zoop_mean_biom$taxon=="copepod"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="copepod"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="copepod"]) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus1" &
                                   zoop_mean_biom$taxon=="copepod"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="copepod"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="copepod"]) *100
  
  (mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="plus5" &
                                   zoop_mean_biom$taxon=="copepod"]) - 
      mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                      zoop_mean_biom$taxon=="copepod"])) /
        mean(zoop_mean_biom$mean_biom[zoop_mean_biom$scenario=="baseline" &
                                        zoop_mean_biom$taxon=="copepod"]) *100 
  
#-------------------------------------------------------------------------#
# playing around with some other visualizations
ggplot(zoop_mean_biom, aes(x = year, y = mean_biom, color = scenario)) +
  geom_point(size=2) + geom_line(size=1) +
  facet_wrap(~taxon, scales="free_y") +
  ylab(expression("Biomass (mg L"^{-1}*")")) + xlab("") +
  scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
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
  ylab(expression("Biomass (mg L"^{-1}*")")) + xlab("") +
  theme_bw() +
  scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
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
#ggsave("figures/zoop_annual_biom_taxa.jpg", width=7, height=4) 
  
#reverse axes and summarize by year
 diff_zoops <- zoop_scenarios |>
   group_by(taxon, year, scenario) |>
   summarise(mean_diff = mean(diff, na.rm = TRUE), 
                   sd = sd(diff, na.rm = TRUE)) 
 
 ggplot(data=subset(diff_zoops, year %in% 2016:2021)) +  
   geom_point(aes(x=mean_diff, 
                  y=factor(scenario,levels=c("baseline","plus1",
                                             "plus5","plus10")), 
                           color=scenario), size=3) +
   theme_bw() + xlab("Difference from mean") + ylab("") +
   facet_wrap(~taxon, scales="free_x") + 
   scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
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
# write.csv(phyto_scenarios, "./analysis/data/phyto_scenarios.csv", row.names = F)
  
  phyto_scenarios <-read.csv("analysis/data/phyto_scenarios.csv") |>
    mutate(DateTime = as.Date(DateTime)) |>
    filter(DateTime >= "2015-07-07")
  
#order phytos
phyto_scenarios$taxon <- factor(phyto_scenarios$taxon, 
                                  levels = c("cyano","green","diatom"))
  
ggplot(data = phyto_scenarios,
         aes(x=DateTime, y = value, color=taxon)) +
    geom_area(aes(color = taxon, fill = taxon),
              position = "fill", 
              stat = "identity") +
    facet_wrap(~factor(str_to_title(scenario), 
                       levels = c("Baseline","Plus1","Plus5","Plus10")), 
                       scales = "free_x")+
    scale_color_manual(values = c("cyan","green","brown4"))+
    scale_fill_manual(values = c("cyan","green","brown4"),
                      labels = c("Cyanobacteria","Greens","Diatoms"))+
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
#ggsave("figures/relative_phyto_scenarios.jpg", width=7, height=4)
  
  
  phyto_mean_biom <-  phyto_scenarios |>
    group_by(taxon, scenario, year) |>
    summarise(mean_biom = mean(value)) |>
    mutate(taxon = recode(taxon,
                          "cyano" = "Cyanobacteria",
                          "diatom" = "Diatoms",
                          "green" = "Greens"))
  
  ggplot(data = subset(phyto_mean_biom, year %in% 2016:2021), 
         aes(x = year, y = mean_biom, color = scenario)) +
    geom_point(size=2) + geom_line(size=1) +
    facet_wrap(~taxon, scales="free_y") +
    ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) +
    scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                       breaks = c("baseline","plus1","plus5","plus10"),
                       labels = c("Baseline","Plus1","Plus5","Plus10")) +
    theme_bw() + xlab("") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = "top",
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
          legend.margin=margin(0,0,-1,0),
          panel.spacing.x = unit(0.1, "in"),
          panel.background = element_rect(
            fill = "white"))
  #ggsave("figures/phyto_annual_biom_scenario_lineplot.jpg", width=7, height=4) 
  
  # smoothed monthly biomass for each scenario
  phyto_scenarios |>
    filter(year %in% c(2016:2021)) |>
    mutate(month = lubridate::month(DateTime)) |>
    group_by(taxon, scenario, month) |>
    summarise(monthly_biom = mean(value), .groups = "drop") |>
    ggplot(aes(x = factor(month), y = monthly_biom, color = factor(
      scenario, levels = c("baseline", "plus1", "plus5", "plus10")),
      group = interaction(taxon, scenario))) + 
    geom_smooth(method = "loess") +
    facet_wrap(~taxon, nrow=1)+ 
    scale_x_discrete(labels = c("Jan","","Mar","", "May", "", "Jul",
                                "","Sep", "","Nov","")) +
    ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) + xlab("") +
    scale_color_manual("", values = c("#147582", "#c6a000", "#c85b00", "#680000"),
                       breaks = c("baseline", "plus1", "plus5", "plus10"),
                       labels = c("Baseline","Plus1","Plus5","Plus10")) +
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
  #ggsave("figures/smoothed_monthly_biom_phytos.jpg", width=7, height=4) 
  
#------------------------------------------------------------------------#
# zoop plots
  
  #read in zoop scenarios df
  zoop_scenarios <- read.csv("analysis/data/zoop_scenarios.csv") |>
    dplyr::filter(!taxon %in% "total") |>
    dplyr::group_by(taxon, year, scenario) |>
    dplyr::mutate(max_doy = doy[which.max(value)],
                  max_value = max(value)) |>
    dplyr::filter(DateTime >= "2016-01-01" &
                    DateTime < "2022-01-01") #filtering out the 2 partial years

# visualize the mean doy for peak biomass across all years
  zoop_scenarios |>
    dplyr::mutate(scenario = factor(scenario, 
                                    levels = c("baseline", "plus1",
                                               "plus5", "plus10")),
                  taxon = stringr::str_to_title(taxon)) |>
  ggplot(aes(x = max_doy, y = as.factor(scenario), 
             fill = as.factor(scenario))) + xlab("Max DOY") +
    geom_violin(aes(fill = as.factor(scenario)), scale = "width", trim = FALSE) +
    scale_fill_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
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
  #ggsave("figures/zoop_violin_max_doy.jpg", width=7, height=4)
  
  
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
  
# numbers for results text
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                              zoop_timing$scenario=="baseline"]) -
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                              zoop_timing$scenario=="plus10"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                              zoop_timing$scenario=="baseline"]) -
    mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                                zoop_timing$scenario=="plus5"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                              zoop_timing$scenario=="baseline"]) -
    mean(zoop_timing$mean_doy[zoop_timing$taxon=="cladoceran" &
                                zoop_timing$scenario=="plus1"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                              zoop_timing$scenario=="plus10"]) -
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                              zoop_timing$scenario=="baseline"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                              zoop_timing$scenario=="plus5"]) -
    mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                                zoop_timing$scenario=="baseline"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                              zoop_timing$scenario=="plus1"]) -
    mean(zoop_timing$mean_doy[zoop_timing$taxon=="copepod" &
                                zoop_timing$scenario=="baseline"])

  mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                              zoop_timing$scenario=="baseline"]) -
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                              zoop_timing$scenario=="plus10"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                              zoop_timing$scenario=="baseline"]) -
    mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                                zoop_timing$scenario=="plus5"])
  
  mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                              zoop_timing$scenario=="baseline"]) -
    mean(zoop_timing$mean_doy[zoop_timing$taxon=="rotifer" &
                                zoop_timing$scenario=="plus1"])
  
# also plot the doy as a point for each year
  ggplot(zoop_scenarios, aes(x = max_doy, y = max_value, 
             color = as.factor(scenario))) +
    geom_point(aes(fill = as.factor(scenario)), cex=2) +
    scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
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
  