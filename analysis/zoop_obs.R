# zoop observations to inform zoop parameterization and justify 3 groups

pacman::p_load(tidyverse)

# read in zoops
zoops <- read.csv("field_data/field_zoops.csv")

# read in obs temp
temp <- read.csv("field_data/CleanedObsTemp.csv") |>
  filter(Depth %in% c(0.1),
         DateTime %in% zoops$DateTime)

# combine zoops and temp
zoops_temp <- zoops |>
  rename("cladoceran" = "ZOO_cladoceran",
         "copepod" = "ZOO_copepod",
         "rotifer" = "ZOO_rotifer") |>
  filter(DateTime %in% temp$DateTime) |>
  mutate(epi_temp = temp$temp) |>
  tidyr::pivot_longer(cols= -c(DateTime, epi_temp),
                      names_to = c("Taxon"),
                      values_to = "Biomass_ugL") |>
  mutate(Biomass_ugL = Biomass_ugL * 12.011, #convert to ug/L from mmol/m3
         biomass_log = log(Biomass_ugL))


# zoop size histogram for the three focal taxa (supplemental figure)
ggplot(zoops_temp, aes(biomass_log, fill=Taxon)) +
  geom_density(alpha = 0.7) + theme_bw() +
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"),
                    breaks = c("cladoceran","copepod","rotifer"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-5,-10,-10,-10)),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_biom_density_plot_log.jpg", width=4, height=3)

# Fit separate models for each group and add predictions
zoops_temp_fit <- zoops_temp |>
  na.omit() |>
 # filter(Biomass_ugL <= 500) |>
  group_by(Taxon) |>
  mutate(fit = predict(lm(Biomass_ugL ~ poly(epi_temp, 2, raw = TRUE))),
         fit_log = predict(lm(biomass_log ~ poly(epi_temp, 2, raw = TRUE))))

# now plot biomass vs. epi temp
ggplot(zoops_temp_fit, aes(epi_temp, Biomass_ugL, color=Taxon)) +
  geom_point() + theme_bw() +
  geom_line(aes(y = fit), color = "black", size = 0.8) +
  scale_color_manual(values = c("#084c61","#db504a","#e3b505"),
                    breaks = c("cladoceran","copepod","rotifer"))+
  facet_wrap(~Taxon, scales = "free_y", ncol=3) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-5,-10,-10,-10)),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_biom_vs_epi_temp_500_lim.jpg", width=4, height=3)


# Fit a quadratic model
model <- lm(zoops_temp$Biomass_ugL[zoops_temp$Taxon=="rotifer"] ~ 
              poly(zoops_temp$epi_temp[zoops_temp$Taxon=="rotifer"], 
                   2, raw = TRUE))

# Extract coefficients
coefs <- coef(model)

# Create a formatted equation
equation <- paste0(
  "y = ",
  round(coefs[1], 2), " + ",
  round(coefs[2], 2), "*x + ",
  round(coefs[3], 2), "*x^2"
)

# Print the equation
print(equation)
