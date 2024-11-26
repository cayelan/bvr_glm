# exploring patterns in individual years and trying to link with drivers

#read in zoop df and add ratios
zoop_ratios <-read.csv("analysis/data/zoop_scenarios.csv") |>
  group_by(DateTime, scenario) |>
  mutate(DateTime = as.Date(DateTime),
         crust_rot = sum(c(value[taxon=="cladoceran"],
                           value[taxon=="copepod"])) /
                           value[taxon=="rotifer"],
         clad_rot = value[taxon=="cladoceran"] /
                    value[taxon=="rotifer"],
         cope_rot = value[taxon=="copepod"] /
                    value[taxon=="rotifer"],
         clad_cope = value[taxon=="cladoceran"] /
                     value[taxon=="copepod"]) |>
  filter(DateTime >= "2016-01-01" &
           DateTime <= "2021-12-31") |>
  ungroup() |>
  select(-c(DateTime,mod,daily_sum,doy,annual_sum,annual_prop,value,taxon)) |>
  group_by(year, scenario) |>
  summarise_all(list(mean)) |>
  tidyr::pivot_longer(cols = -c(scenario,year),
                      names_to = "ratio",
                      values_to = "value")

# plot ratios as facets - value vs. year
ggplot(zoop_ratios, aes(x = year, y = value, color = scenario)) +
  geom_point(size=2) + geom_line(size=1) +
  facet_wrap(~ratio) +
  xlab("")+
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
#ggsave("figures/zoop_annual_ratios_scenario_lineplot.jpg", width=7, height=4) 

# boxplots
ggplot(zoop_ratios, aes(x=factor(
  scenario,levels=c("baseline","plus1","plus5","plus10")), 
  y = value, fill=ratio)) +
  geom_boxplot() +
  ylab("value") + xlab("") +
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
        legend.margin=margin(0,0,0,0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_zoop_ratio_scenario_boxplot.jpg", width=7, height=4) 

# read in zoop diagnostics
zoop_diag <- read.csv("./analysis/data/zoop_diagnostics.csv") |>
  mutate(year = lubridate::year(DateTime)) |>
  group_by(year, scenario, variable) |>
  summarise(yearly_sum = sum(value)) |>
  filter(!year %in% c("2015","2022"))

# plot rates as facets - value vs. year
ggplot(zoop_diag, aes(x = year, y = yearly_sum, color = scenario)) +
  geom_point(size=2) + geom_line(size=1) +
  facet_wrap(~variable, scales="free_y") +
  xlab("")+
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
#ggsave("figures/zoop_annual_rates_scenario_lineplot.jpg", width=7, height=4) 

# boxplots
ggplot(zoop_diag, aes(x=factor(
  scenario,levels=c("baseline","plus1","plus5","plus10")), 
  y = yearly_sum, fill=variable)) +
  scale_fill_manual(values = NatParksPalettes::
                      natparks.pals("Volcanoes", 5, direction = -1))+
  geom_boxplot() + guides(fill = "none") +
  ylab("value") + xlab("") +
  facet_wrap(~variable, scales = "free_y") +
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
        axis.text.x = element_text(angle=90),
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
#ggsave("figures/BVR_zoop_rates_scenario_boxplot.jpg", width=7, height=4) 


# now time to look at some phytos

phyto_mean_biom <-read.csv("analysis/data/phyto_scenarios.csv") |>
  group_by(taxon, year, scenario) |>
  summarise(mean_biom = mean(value)) |>
  filter(year %in% c(2016:2021))

ggplot(phyto_mean_biom, aes(x = year, y = mean_biom,  color = scenario)) +
  geom_line(size=1) + geom_point(size=2) +
  facet_wrap(~taxon, scales = "free") +
  xlab("") + theme_bw() +
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
#ggsave("figures/phyto_annual_biom_timing.jpg", width=7, height=4) 

# boxplots
ggplot(phyto_mean_biom, aes(x=factor(
  scenario,levels=c("baseline","plus1","plus5","plus10")), 
  y = mean_biom, fill=taxon)) +
  geom_boxplot() +
  ylab("value") + xlab("") +
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
        legend.margin=margin(0,0,0,0),
        panel.spacing.x = unit(0.2, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/phyto_scenario_boxplot.jpg", width=7, height=4) 

zoop_mean_biom <-read.csv("analysis/data/zoop_scenarios.csv") |>
  group_by(taxon, year, scenario) |>
  summarise(mean_biom = mean(value)) |>
  filter(year %in% c(2016:2021))

ggplot(zoop_mean_biom, aes(x = year, y = mean_biom,  color = scenario)) +
  geom_line(size=1) + geom_point(size=2) +
  facet_wrap(~taxon, scales = "free") +
  xlab("") + theme_bw() +
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
#ggsave("figures/zoop_annual_biom_timing.jpg", width=7, height=4) 

