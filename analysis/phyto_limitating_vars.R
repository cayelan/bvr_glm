# Script to identify limiting vars for phyto groups
# 17 Sep 2024 - adapted from CCC

#### phyto succession ####

pacman::p_load(tidyverse)

scenario <- c("baseline","plus1","plus5","plus10")

for(j in 1:length(scenario)) {
  
  nc_file = paste0("sims/spinup/",scenario[j],"/output/output.nc")  
  
phytos <- c('cyano','green','diatom')
limits <- c("fNit","fI","fPho","fSil","fT","fSal")
vars <- expand.grid(phytos,limits) |> 
  rename(phyto = Var1, limit = Var2)
mod <- NULL
for(i in 1:nrow(vars)){
  var <- paste("PHY",vars[i,1],vars[i,2],sep="_")
  tmp<- glmtools::get_var(nc_file, var, reference="surface", z_out=depths) |> 
    pivot_longer(cols=starts_with(paste0(var,"_")), 
                 names_to="Depth", names_prefix=paste0(var,"_"), 
                 values_to = "value") |> 
    mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
    mutate(Depth=as.numeric(Depth)) |> 
    na.omit() |> 
    mutate(phyto = vars[i,1],
           limit = vars[i,2])
  mod <- rbind(mod, tmp)
}

assign(paste0("mod_", scenario[j]), mod)

mod2 <- mod |>  pivot_wider(names_from = limit, values_from = value) |>  
  mutate(R_growth = ifelse(phyto == "cyano", 1.0, 1.4),
         R_growth = ifelse(phyto == "green", 3.0, R_growth)) |> 
  rowwise() |> 
  mutate(nutrients = min(c(fI,fPho,fSil,fNit)),
         combined = fT * nutrients,
         prod = combined * R_growth) |> 
  select(-fSal) |> 
  pivot_longer(-c("DateTime","Depth", "phyto"), 
               names_to = "limit", values_to = "value")

assign(paste0("mod2_", scenario[j]), mod2)
}

#plot limiting phyto var for each scenario
mod2_baseline |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0 & 
                  DateTime >= as.Date("2015-07-07")) |> 
  ggplot() + xlab("Baseline") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()

mod2_plus1 |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0 & 
                  DateTime >= as.Date("2015-07-07")) |> 
  ggplot() + xlab("Plus 1C") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()

mod2_plus5 |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0 & 
                  DateTime >= as.Date("2015-07-07")) |> 
  ggplot() + xlab("Plus 5C") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()

mod2_plus10 |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0 & 
                  DateTime >= as.Date("2015-07-07")) |>  
  ggplot() + xlab("Plus 10C") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()
