# Plankton air temp scenarios
# 22 August 2024

devtools::install_github("eliocamp/tagger")
pacman::p_load(ggplot2,ggridges,dplyr, ARTool, scales, NatParksPalettes,
               FSA, egg, tagger, stringr, cowplot)

scenario <- c("baseline","plus1","plus5","plus10")

# proportional plankton figs for each scenario
zoop_scenarios <-read.csv("analysis/data/zoop_scenarios.csv") |>
  mutate(DateTime = as.Date(DateTime)) |>
  filter(DateTime >= "2015-07-07") |>
  group_by(year, taxon, scenario) |>
  mutate(mean_biom = mean(value)) |>
  ungroup() |>
  mutate(diff = value - mean_biom) 

#summarize across taxa to describe bl zoop timing in results (Figure 3)
bl_zoop_summary <- zoop_scenarios |>
  filter(scenario %in% c("baseline"),
         year %in% c(2016:2021)) |>
  group_by(year, taxon) |>
  mutate(max_biom_doy = doy[which.max(value)]) |>
  ungroup() |> group_by(taxon) |>
  summarise(max_doy = mean(max_biom_doy))

# reorder the 'taxon' factor levels
facet_labels <- c("Cladoceran", "Copepod", "Rotifer", "Total biomass")
names(facet_labels) <- c("cladoceran", "copepod", "rotifer","total")

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

# relative zoop density for baseline vs. plus 5 (Figure 6)
area <-  ggplot(data = subset(zoop_scenarios, 
                              scenario %in% c("baseline","plus5") &
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
                            scenario %in% c("baseline","plus5")),
              aes(x=taxon, y = mean_proportion, fill=scenario)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#147582","#680000"),
                    labels = c("Baseline","Plus5"))+
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
                                        mean_proportions$scenario=="plus5"]) -
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="cladoceran" &
                                        mean_proportions$scenario=="baseline"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="plus5"]) -
mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="baseline"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="copepod" &
                                        mean_proportions$scenario=="plus5"])

mean(mean_proportions$mean_proportion[mean_proportions$taxon=="rotifer" &
                                        mean_proportions$scenario=="plus5"]) -
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

# stacked annual biomass bar chart (Figure S6)
zoop_annual <- zoop_scenarios |>
  mutate(year = year(DateTime)) |>
  group_by(year, taxon, scenario) |>
  summarize(annual_biomass = sum(value, na.rm = TRUE), .groups = "drop")

ggplot(data=subset(zoop_annual, !taxon %in% "total" &
                     scenario %in% "baseline" & 
                     year %in% c(2016:2021)),
       aes(x = factor(year), y = annual_biomass, fill = taxon)) +
  geom_col(position = "stack") + 
  facet_wrap(~ str_to_title(scenario)) +       
  labs(x = "", y = expression("Biomass (mg C L"^{-1}*")"), fill = "") +
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"),
                    labels = c("Cladoceran","Copepod","Rotifer"))+
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
#ggsave("figures/BVR_stacked_zoop_biom_barchart.jpg", width=7, height=4) 
  
# smoothed monthly biomass for each scenario (Figure 7)
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
  
  #calculate cv
  zoop_cv <- zoop_scenarios |> 
    filter(year %in% c(2016:2021)) |> 
    mutate(month = lubridate::month(DateTime)) |> 
    group_by(taxon, scenario) |> 
    summarise(
      mean_biom = mean(value, na.rm = TRUE), 
      sd_biom = sd(value, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(cv_biom = sd_biom / mean_biom)
  
#same but panels for each year (Figure S14)
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
  
# scenario boxplots across taxa (Figure 5)
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
  
  # numbers for results text (percent change for each taxa between baseline vs. warming scenarios)
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
  
# mean annual zoop biomass across scenarios (Figure S8)
ggplot(zoop_mean_biom, aes(x = year, y = mean_biom, color = scenario)) +
  geom_point(size=2) + geom_line(size=1) +
  facet_wrap(~str_to_title(taxon), scales="free_y") +
  ylab(expression("Biomass (mg L"^{-1}*")")) + xlab("") +
  scale_color_manual("", values = c("#147582","#c6a000","#c85b00","#680000"),
                     breaks = c("baseline","plus1","plus5","plus10"),
                     labels = c("Baseline","Plus1","Plus5","Plus10")) +
  theme_bw() +
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
        legend.box.margin = margin(-20,-10,-10,-10),
        legend.margin=margin(20,0,-5,0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(
          fill = "white"))
#ggsave("figures/zoop_annual_biom_scenario_lineplot.jpg", width=7, height=4) 

#-----------------------------------------------------------------------#
  phyto_scenarios <-read.csv("analysis/data/phyto_scenarios.csv") |>
    mutate(DateTime = as.Date(DateTime)) |>
    filter(DateTime >= "2015-07-07")
  
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
  
  # values for results text (percent change for zoop diag rates for each taxa - bl vs. warming scenario)
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="clads" &
                                       zoop_diags_summary$diag=="Grazing"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="clads" &
                                          zoop_diags_summary$diag=="Grazing"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="clads" &
                                        zoop_diags_summary$diag=="Grazing"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="copes" &
                                       zoop_diags_summary$diag=="Grazing"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="copes" &
                                          zoop_diags_summary$diag=="Grazing"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="copes" &
                                        zoop_diags_summary$diag=="Grazing"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="rots" &
                                       zoop_diags_summary$diag=="Grazing"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="rots" &
                                          zoop_diags_summary$diag=="Grazing"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="rots" &
                                        zoop_diags_summary$diag=="Grazing"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="clads" &
                                       zoop_diags_summary$diag=="Mortality"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="clads" &
                                          zoop_diags_summary$diag=="Mortality"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="clads" &
                                        zoop_diags_summary$diag=="Mortality"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="copes" &
                                       zoop_diags_summary$diag=="Mortality"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="copes" &
                                          zoop_diags_summary$diag=="Mortality"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="copes" &
                                        zoop_diags_summary$diag=="Mortality"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="rots" &
                                       zoop_diags_summary$diag=="Mortality"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="rots" &
                                          zoop_diags_summary$diag=="Mortality"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="rots" &
                                        zoop_diags_summary$diag=="Mortality"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="clads" &
                                       zoop_diags_summary$diag=="Respiration"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="clads" &
                                          zoop_diags_summary$diag=="Respiration"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="clads" &
                                        zoop_diags_summary$diag=="Respiration"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="copes" &
                                       zoop_diags_summary$diag=="Respiration"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="copes" &
                                          zoop_diags_summary$diag=="Respiration"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="copes" &
                                        zoop_diags_summary$diag=="Respiration"])
  
  (mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Plus5" &
                                       zoop_diags_summary$taxon=="rots" &
                                       zoop_diags_summary$diag=="Respiration"]) - 
      mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                          zoop_diags_summary$taxon=="rots" &
                                          zoop_diags_summary$diag=="Respiration"])) /
    mean(zoop_diags_summary$mean_rate[zoop_diags_summary$scenario=="Baseline" &
                                        zoop_diags_summary$taxon=="rots" &
                                        zoop_diags_summary$diag=="Respiration"])
  
  # summary diag fig + phytos for ms (Fig. 9)
  plot1 <- ggplot(data=subset(zoop_diags_summary, 
                              scenario %in% c("Baseline","Plus5")),
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
    filter(scenario %in% c("baseline","plus5"))
  
  # Compute max total biomass to rescale
  max_total <- max(total_phytos$total, na.rm = TRUE)
  
  # make sure phytos are in the right order
  phyto_scenarios$taxon <- factor(phyto_scenarios$taxon, 
                                  levels = c("cyano","green","diatom"))
  
  #note that this comes from zoop_scenarios.R
  plot2 <- ggplot(data = subset(phyto_scenarios, 
                                scenario %in% c("baseline", "plus5")), 
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
  #ggsave("figures/ms_fig9.jpg", width=8, height=6)
  
  # numbers for results text (percent change between phyto group biomass for baseline vs. warming scenarios)
  (mean(phyto_scenarios$value[phyto_scenarios$scenario=="plus5" &
                                phyto_scenarios$taxon=="cyano"]) -
      mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                   phyto_scenarios$taxon=="cyano"])) /
    mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                 phyto_scenarios$taxon=="cyano"])
  
  (mean(phyto_scenarios$value[phyto_scenarios$scenario=="plus5" &
                                phyto_scenarios$taxon=="diatom"]) -
      mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                   phyto_scenarios$taxon=="diatom"])) /
    mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                 phyto_scenarios$taxon=="diatom"])
  
  (mean(phyto_scenarios$value[phyto_scenarios$scenario=="plus5" &
                                phyto_scenarios$taxon=="green"]) -
      mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                   phyto_scenarios$taxon=="green"])) /
    mean(phyto_scenarios$value[phyto_scenarios$scenario=="baseline" &
                                 phyto_scenarios$taxon=="green"])
  
  phyto_mean_biom <-  phyto_scenarios |>
    group_by(taxon, scenario, year) |>
    summarise(mean_biom = mean(value)) |>
    mutate(taxon = recode(taxon,
                          "cyano" = "Cyanobacteria",
                          "diatom" = "Diatoms",
                          "green" = "Greens"))
  
  # Figure S12
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

  # Figure S9
  ggplot(data = subset(phyto_scenarios),
         aes(x=DateTime, y = value, color=taxon)) +
    geom_line() +
    facet_wrap(~factor(str_to_title(scenario), 
                       levels = c("Baseline","Plus1","Plus5","Plus10")), 
               scales = "free_x")+
    scale_color_manual(values = c("cyan","green","brown4"),
                       labels = c("Cyanobacteria","Greens","Diatoms"))+
    scale_fill_manual(values = c("cyan","green","brown4"))+
    scale_x_date(expand = c(0.02,0.02)) +
    xlab("") + ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) +
    guides(color= guide_legend(ncol=3),
           fill = "none") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          text = element_text(size=10), 
          axis.text = element_text(size = 9),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.background.x = element_blank(),
          axis.title.y = element_text(size = 10),
          plot.margin = unit(c(0, 0.3, 0, 0), "cm"),
          legend.box.margin = margin(0,-10,-10,-10),
          legend.margin=margin(0,0,0,0),
          panel.spacing.x = unit(0.2, "in"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.spacing = unit(0.5, "lines"))
  #ggsave("figures/phyto_raw_biom_scenario_lineplot.jpg", width=7, height=4) 
  
#------------------------------------------------------------------------#
# zoop timing plots
  #read in zoop scenarios df
  zoop_scenarios <- read.csv("analysis/data/zoop_scenarios.csv") |>
    dplyr::filter(!taxon %in% "total") |>
    dplyr::group_by(taxon, year, scenario) |>
    dplyr::mutate(max_doy = doy[which.max(value)],
                  max_value = max(value)) |>
    dplyr::filter(DateTime >= "2016-01-01" &
                    DateTime < "2022-01-01") #filtering out the 2 partial years

# visualize the mean doy for peak biomass across all years (Figure 8)
  zoop_scenarios |>
    dplyr::mutate(scenario = factor(scenario, 
                                    levels = c("baseline", "plus1",
                                               "plus5", "plus10")),
                  taxon = stringr::str_to_title(taxon)) |>
  ggplot(aes(x = max_doy, y = as.factor(scenario), 
             fill = as.factor(scenario))) + xlab("Max DOY") +
    geom_violin(aes(fill = as.factor(scenario)), scale = "width", trim = TRUE) +
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
  

# numbers for results text (change in days for max biomass for each taxa between baseline and warming scenarios)
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
  
# Create a phyto and zoop PEG model fig (Figure S15)
  total_phyto_scenarios <- phyto_scenarios |>
    mutate(DateTime = as.Date(DateTime)) |>
    group_by(DateTime, scenario) |>
    summarise(total_phytos = sum(value)) |>
    filter(DateTime >= "2016-01-01" & DateTime <= "2021-12-31") |>
    arrange(scenario, DateTime)
  
  total_zoop_scenarios <- zoop_scenarios |>
    mutate(DateTime = as.Date(DateTime)) |>
    group_by(DateTime, scenario) |>
    summarise(total_zoops = sum(value)) |>
    arrange(scenario, DateTime)
  
  plankton <- full_join(total_phyto_scenarios, total_zoop_scenarios, 
                        by = c("DateTime", "scenario")) |>
    pivot_longer(cols = c(total_phytos, total_zoops), 
                 names_to = "plankton", 
                 values_to = "value") 
  
  # standardize plankton biomass
  plankton_standardized <- plankton |>
    group_by(plankton) |>
    mutate(standardized_biom = if_else(plankton == "total_phytos", 
                                  (value - mean(value, na.rm = TRUE)) / 
                                    sd(value, na.rm = TRUE),
                                  if_else(plankton == "total_zoops", 
                                          (value - mean(value, na.rm = TRUE)) / 
                                            sd(value, na.rm = TRUE),
                                          NA_real_))) |>
    ungroup()
  
  # smoothed monthly biomass for each scenario
  plankton_standardized |>
    mutate(DateTime = as.Date(DateTime),
           month = lubridate::month(DateTime),
           scenario = factor(str_to_title(scenario), 
                             levels = c("Baseline","Plus1","Plus5","Plus10"))) |>
    group_by(plankton, scenario, month) |>
    summarise(monthly_biom = mean(standardized_biom, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = factor(month), y = monthly_biom, color = plankton)) + 
    geom_point(alpha = 0.5) +  # Add points for visibility
    geom_smooth(aes(group = plankton), method = "loess") + 
    facet_wrap(~scenario, nrow = 1) + 
    scale_color_manual("", values = c("#4A1634", "#4B91BB"),
                       labels = c("Total phytoplankton", "Total zooplankton")) +
  scale_x_discrete(breaks = 1:12, labels = c("Jan","","Mar","", "May", "", "Jul",
                              "","Sep", "","Nov","")) +
  ylab(expression("Biomass (" * mu * " g L"^{-1}*")")) + xlab("") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size = 10), 
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.8),
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold", hjust = 0.5),  # Added hjust
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 11),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.box.margin = margin(0, -10, -10, -10),
        legend.margin = margin(0, 0, 0, 0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0.5, "lines"))
  #ggsave("figures/phyto_zoop_timing.jpg", width=7, height=5)
