# Miscellaneous code for checking on plankton dynamics and creating zoop_diagnostics.csv
# Written by Heather Wander
# 22 August 2024

pacman::p_load(scales, ggplot2, glmtools, dplyr, tidyr, NatParksPalettes)

scenario <- c("baseline","plus1", "plus5","plus10")

for (i in 1:length(scenario)){

#define the output.nc file 
nc_file <- file.path(paste0('./sims/spinup/',scenario[i],'/output/output.nc')) 

#--------------------------------------------------------------------------#
#phyto and zoop stacked line plot

cyano <- get_var(file=nc_file,var_name = "PHY_cyano",
                 z_out=0.1,reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

green <- get_var(file=nc_file,var_name = "PHY_green",
                 z_out=0.1,reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

diatom <- get_var(file=nc_file,var_name = "PHY_diatom",
                  z_out=0.1,reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

#combine taxon dfs
phytos_long <- bind_cols(cyano, green[!colnames(green) %in% "DateTime"], 
                         diatom[!colnames(diatom) %in% "DateTime"]) |> 
  rename(cyano = PHY_cyano_0.1,
         green = PHY_green_0.1,
         diatom = PHY_diatom_0.1) |> 
  pivot_longer(cols = cyano:diatom, 
               names_to = "variable")

#reorder taxa
phytos_long$variable <- factor(phytos_long$variable, 
                               levels=c("diatom","cyano","green"))

#phyto plot
p <- ggplot(phytos_long, aes(x = DateTime, y = value)) + 
        geom_area(aes(color = variable, fill = variable),
                  position = "stack", stat="identity",
                  linewidth=1) +
        scale_color_manual(values = NatParksPalettes::
                             natparks.pals("RockyMtn", 7,  direction = -1))+
        scale_fill_manual(values = NatParksPalettes::
                            natparks.pals("RockyMtn", 7, direction = -1))+
        ylab("Biomass (mmol/m3 C)")+
        scale_x_datetime(expand = c(0,0),labels = 
                           date_format("%Y",tz="EST5EDT")) +
        scale_y_continuous(expand = c(0,0))+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.key = element_blank(),
              legend.background = element_blank(),
              legend.position = c(0.88,0.83),
              text = element_text(size=8), 
              axis.text.y = element_text(size = 8),
              panel.border = element_rect(colour = "black", fill = NA),
              strip.text.x = element_text(face = "bold",hjust = 0),
              axis.text.x = element_text(angle=0),
              strip.background.x = element_blank(),
              axis.title.y = element_text(size = 9),
              plot.margin = unit(c(0, 1, 0, 0), "cm"),
              panel.spacing = unit(0.5, "lines"))
ggsave(p, filename = paste0("figures/BVR_stacked_phyto_composition_0.1m_",scenario[i],".jpg"), width=5, height=4) 

clad <- get_var(file=nc_file,var_name = "ZOO_cladoceran",
                z_out=0.1,reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

cope <- get_var(file=nc_file,var_name = "ZOO_copepod",
                z_out=0.1,reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

rot <- get_var(file=nc_file,var_name = "ZOO_rotifer",
               z_out=0.1,reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

#combine taxon dfs
zoops_long <- bind_cols(clad, cope[!colnames(cope) %in% "DateTime"], 
                        rot[!colnames(rot) %in% "DateTime"]) |> 
  rename(cladoceran = ZOO_cladoceran_0.1,
         copepod = ZOO_copepod_0.1,
         rotifer = ZOO_rotifer_0.1) |> 
  pivot_longer(cols = cladoceran:rotifer, 
               names_to = "variable")

#zoop plot
p <- ggplot(zoops_long, aes(x = DateTime, y = value)) + 
        geom_area(aes(color = variable, fill = variable),
                  position = "stack", stat="identity",
                  linewidth=1) +
        scale_color_manual(values = NatParksPalettes::
                             natparks.pals("KingsCanyon", 3,  direction = -1))+
        scale_fill_manual(values = NatParksPalettes::
                            natparks.pals("KingsCanyon", 3, direction = -1))+
        ylab("Biomass (mmol/m3 C)")+
        scale_x_datetime(expand = c(0,0),labels = 
                           date_format("%Y",tz="EST5EDT")) +
        scale_y_continuous(expand = c(0,0))+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.key = element_blank(),
              legend.background = element_blank(),
              legend.position = c(0.88,0.83),
              text = element_text(size=8), 
              axis.text.y = element_text(size = 8),
              panel.border = element_rect(colour = "black", fill = NA),
              strip.text.x = element_text(face = "bold",hjust = 0),
              axis.text.x = element_text(angle=0),
              strip.background.x = element_blank(),
              axis.title.y = element_text(size = 9),
              plot.margin = unit(c(0, 1, 0, 0), "cm"),
              panel.spacing = unit(0.5, "lines"))
ggsave(p, filename = paste0("figures/BVR_stacked_zoop_composition_0.1m_",scenario[i],".jpg"), width=5, height=4) 

#--------------------------------------------------------------------------#
#stacked plot for zoop diagnostics

grz <- get_var(file=nc_file,var_name = 'ZOO_grz',z_out=1,
               reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07"))

resp <- get_var(file=nc_file,var_name = 'ZOO_resp',z_out=1,
                reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07")) |>
  mutate(ZOO_resp_1 = ZOO_resp_1 * -1)

mort <- get_var(file=nc_file,var_name = 'ZOO_mort',z_out=1,
                reference = 'surface') |> 
  filter(DateTime >= as.POSIXct("2015-07-07")) |> 
  mutate(ZOO_mort_1 = ZOO_mort_1 * -1)

#combine diagnostics into 1 df
diag_long <- bind_cols(grz, resp[!colnames(resp) %in% "DateTime"],
                       mort[!colnames(mort) %in% "DateTime"]) |> 
  rename(grz = ZOO_grz_1,
         resp = ZOO_resp_1,
         mort = ZOO_mort_1) |> 
  pivot_longer(cols = grz:mort,
               names_to = "variable")

p <- ggplot(diag_long, aes(x = DateTime, y = value)) + 
      geom_area(aes(color = variable, fill = variable),
                position = "stack", stat="identity",
                linewidth=1) +
      scale_color_manual(values = NatParksPalettes::
                           natparks.pals("Volcanoes", 5,  direction = -1))+
      scale_fill_manual(values = NatParksPalettes::
                          natparks.pals("Volcanoes", 5, direction = -1))+
      ylab("diagnostics (mmolC/m3/day)")+ xlab("") +
      scale_x_datetime(expand = c(0,0),labels = 
                         date_format("%Y",tz="EST5EDT")) +
      scale_y_continuous(expand = c(0,0), limits = c(-5,10))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.key = element_blank(),
            legend.background = element_blank(),
            legend.position = c(0.88,0.83),
            text = element_text(size=8), 
            axis.text.y = element_text(size = 8),
            panel.border = element_rect(colour = "black", fill = NA),
            strip.text.x = element_text(face = "bold",hjust = 0),
            axis.text.x = element_text(angle=0),
            strip.background.x = element_blank(),
            axis.title.y = element_text(size = 9),
            plot.margin = unit(c(0, 1, 0, 0), "cm"),
            panel.spacing = unit(0.5, "lines"))
ggsave(p, filename = paste0("figures/BVR_stacked_zoop_diag_",scenario[i],".jpg"), width=5, height=4) 

assign(paste0("zoop_diag_", scenario[i]), diag_long)

}

#combine diagnostics into one df
zoop_diag <-  mget(c("zoop_diag_baseline","zoop_diag_plus1",
                          "zoop_diag_plus5", "zoop_diag_plus10")) |>
                   setNames(paste0(scenario)) |>
                   bind_rows(.id = "scenario") |>
                   relocate(scenario, .after = last_col())
write.csv(zoop_diag, "./analysis/data/zoop_diagnostics.csv", row.names = F)

