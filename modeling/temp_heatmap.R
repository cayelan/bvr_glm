#code for temp observation contour plots (heatmaps)

pacman::p_load(colorRamps,scales, rLakeAnalyzer, akima)

obs <- read_field_obs('field_data/CleanedObsTemp.csv', var) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  arrange(DateTime, Depth) |> 
  mutate(doy = as.numeric(DateTime))

#temperature
interp_temp <- akima::interp(x=obs$doy, y = obs$Depth, z = obs$temp,
                      xo = seq(min(obs$doy),max(obs$doy), by=1), 
                      yo = seq(0.1, 11, by = 0.1),
                      extrap = F, linear = T, duplicate = "strip")
interp_temp <- interp2xyz(interp_temp, data.frame=T)

#add date column
interp_temp$date <- as.Date(interp_temp$x)

ggplot(interp_temp, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand=c(0,0))+
  scale_x_date(date_breaks = "1 year", 
               labels=scales::date_format("%Y"), expand=c(0,0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "", y = "Depth (m)", title = "BVR Temperature Heatmap",fill=expression(''*~degree*C*''))+
  theme_bw()

#------------------------------------------------------------------------------#
#rLakeanalyzer for expected thermocline depths

#create wide df
temp_wide <- obs %>%
  select(DateTime,Depth,temp)%>%
  spread(Depth,temp)

# renaming the column names to include wtr_ 
# Otherwise, rLakeAnaylzer will not run!
colnames(temp_wide)[-1] = paste0('wtr_',colnames(temp_wide)[-1])

# rename the first column to "datetime"
names(temp_wide)[1] <- "datetime"

# Calculate thermocline depth
BVR_thermo <- ts.thermo.depth(temp_wide) 

#remove NA rows
BVR_thermo<- BVR_thermo |> 
  filter(!is.na(thermo.depth) &
           !is.nan(thermo.depth))

ggplot(BVR_thermo, aes(datetime, thermo.depth)) +
  geom_point(col="red") + geom_line(stat="identity") + theme_bw()

#calculate meta depths
meta_depths <- obs |> group_by(DateTime) |> 
  summarise(meta_top = meta.depths(temp,Depth, seasonal=F)[1],
            meta_bot = meta.depths(temp,Depth, seasonal=F)[2]) |> 
  filter(!is.nan(meta_top)) |> 
  mutate(doy = as.numeric(DateTime))

#convert to long
meta_depths_long <- meta_depths |> 
  pivot_longer(cols=meta_top:meta_bot, names_to="layer")

bounds <- meta_depths_long |> 
  pivot_wider(names_from = layer, values_from = value) |> 
  mutate(
    ymax = pmax(meta_top, meta_bot),
    ymin = pmin(meta_top, meta_bot),
    fill = meta_top > meta_bot
  )

ggplot(meta_depths_long) +
  geom_line(aes(x=DateTime, y=value, linetype=layer)) +
  theme_bw() + scale_y_reverse(expand=c(0,0)) +
  geom_ribbon(data = bounds, aes(DateTime, ymin = ymin, 
                                 ymax = ymax, fill = fill), alpha = 0.4) +
  guides(fill='none') + ylab("Depth (m)")

#create table with summary stats
meta_stats <- data.frame("layer" = c("meta_top","meta_bot"),
                            "mean" = c(mean(meta_depths_long$value[
                              meta_depths_long$layer=="meta_top"]),
                                       mean(meta_depths_long$value[
                                         meta_depths_long$layer=="meta_bot"])),
                            "median" = c(median(meta_depths_long$value[
                              meta_depths_long$layer=="meta_top"]),
                              median(meta_depths_long$value[
                                meta_depths_long$layer=="meta_bot"])),
                            "range1" =c(range(meta_depths_long$value[
                              meta_depths_long$layer=="meta_top"])[1],
                              range(meta_depths_long$value[
                                meta_depths_long$layer=="meta_bot"])[1]),
                            "range2" =c(range(meta_depths_long$value[
                              meta_depths_long$layer=="meta_top"])[2],
                              range(meta_depths_long$value[
                                meta_depths_long$layer=="meta_bot"])[2]))

#------------------------------------------------------------------------------#
#calculate mean, median, range for epi, met, and hypo

temp_epi <- obs |> filter(Depth <= 3 & year(DateTime) == 2015)

temp_meta1 <- obs |> filter(Depth>3 & Depth < 6 & year(DateTime) == 2015)

temp_meta2 <- obs |> filter(Depth>=6 & Depth < 9 & year(DateTime) == 2015)

temp_hypo <- obs |> filter(Depth >= 9 & year(DateTime) == 2015)

#create table with summary stats
temp_stats <- data.frame("layer" = c("epi","meta1","meta2","hypo"),
                         "mean" = c(mean(temp_epi$temp),
                                    mean(temp_meta1$temp), 
                                    mean(temp_meta2$temp), 
                                    mean(temp_hypo$temp)),
                         "median" = c(median(temp_epi$temp),
                                      median(temp_meta1$temp), 
                                      median(temp_meta2$temp), 
                                      median(temp_hypo$temp)),
                         "range1" = c(range(temp_epi$temp)[1],
                                     range(temp_meta1$temp)[1],
                                     range(temp_meta2$temp)[1],
                                     range(temp_hypo$temp)[1]),
                         "range2" = c(range(temp_epi$temp)[2],
                                      range(temp_meta1$temp)[2], 
                                      range(temp_meta2$temp)[2], 
                                      range(temp_hypo$temp)[2]))

