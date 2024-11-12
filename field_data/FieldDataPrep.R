#Use this script to create field data files for the pertinent BVR water quality variables at
#Station 50
#written by CCC originally on 16 July 2018
#updated and cleaned up on 2 June 2020
# Adapted to BVR: A Hounshell, 25 June 2020
# simulation period extended 25May2023 HLW

library(tidyverse)
library(lubridate)
library(zoo)

#focal depths we are trying to compare modeled data vs observations; for CTD/YSI casts
#assumed the deepest point of BVR = 11 m
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) 

###########################################################
######TEMPERATURE, DO, AND CHLA FROM CTD

#need to import CTD observations from EDI
#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf" 
#infile1 <- paste0(getwd(),"/field_data/CTD_final_2013_2022.csv")
#try(download.file(inUrl1,infile1,method="curl"))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


#read in CTD temp file from EDI to create field file, but first need to subset CTD data per each day to depths
ctd<-read.csv(file.path(getwd(),'field_data/CTD_final_2013_2022.csv')) %>% #read in observed CTD data, which has multiple casts on the same day (problematic for comparison)
  filter(Reservoir=="BVR") |>
  filter(Site==50) |>
  rename(time=DateTime, depth=Depth_m, temp=Temp_C, DO=DO_mgL, chla = Chla_ugL) |>
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) |>
  select(time, depth, temp, DO, chla) |>
  filter(time <= as.Date("2022-05-04")) |>
  na.omit() 

#Initialize an empty matrix with the correct number of rows and columns 
temp<-matrix(data=NA, ncol=ncol(ctd), nrow=length(depths)) #of cols in CTD data, and then nrows = # of layers produced
super_final<- NULL #matrix(data=NA, ncol=1, nrow=0)
dates<-unique(ctd$time)

#create a function to chose the matching depth closest to our focal depths
closest<-function(xv, sv){
  xv[which.min(abs(xv-sv))]}

library(plyr) #only use plyr for this for loop, then detach!

#For loop to retrieve CTD depth with the closest function and fill in matrix
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(ctd, ctd$time == j)
  
  layer1<- q[q[, "depth"] == closest(q$depth,0.1),][1,]
  layer2<- q[q[, "depth"] == closest(q$depth,1),][1,]
  layer3<- q[q[, "depth"] == closest(q$depth,2),][1,]
  layer4<- q[q[, "depth"] == closest(q$depth,3),][1,]
  layer5<- q[q[, "depth"] == closest(q$depth,4),][1,]
  layer6<- q[q[, "depth"] == closest(q$depth,5),][1,]
  layer7<- q[q[, "depth"] == closest(q$depth,6),][1,]
  layer8<- q[q[, "depth"] == closest(q$depth,7),][1,]
  layer9<- q[q[, "depth"] == closest(q$depth,8),][1,]
  layer10<-q[q[, "depth"] == closest(q$depth,9),][1,]
  layer11<- q[q[, "depth"] == closest(q$depth,10),][1,]
  layer12<- q[q[, "depth"] == closest(q$depth,11),][1,]
  
  temp<-rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12)
  temp[,((ncol(ctd))+1)] <- depths
  colnames(temp)[((ncol(ctd))+1)]<-"new_depth"
  final <- temp
  final <- data.frame(final)
  super_final <- rbind(super_final, final)
  #super_final <- rbind.fill.matrix(super_final,final)
}

detach(package:plyr)#to prevent issues with dplyr vs plyr not playing well together!


#now need to clean up the data frame and make all factors numeric
super_final1 <- super_final |>
  select(time, new_depth, temp, DO, chla) |>
  rename(depth = new_depth) |>
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) |>
  filter(time > as.Date("2015-07-07")) #need to make sure that the CTD data only start after first day of sim

#now pull in ysi data

# Import YSI observations from EDI
#inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/198/11/6e5a0344231de7fcebbe6dc2bed0a1c3" 
#infile2 <- paste0(getwd(),"/field_data/YSI_PAR_profiles_2013-2022.csv")
#try(download.file(inUrl2,infile2,method="curl"))
#if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

ysi <- read_csv('field_data/YSI_PAR_profiles_2013-2022.csv') %>% 
  filter(Reservoir=="BVR") %>% 
  filter(Site==50) %>% 
  rename(time=DateTime,depth=Depth_m,temp=Temp_C,DO=DO_mgL) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d",tz='EST'))) %>% 
  select(time,depth,temp,DO) %>% 
  filter(time <= as.Date("2022-05-04")) %>%
  na.omit()

#Initialize an empty matrix with the correct number of rows and columns 
temp<-matrix(data=NA, ncol=ncol(ysi), nrow=length(depths)) #of cols in CTD data, and then nrows = # of layers produced
super_final_ysi<- NULL
dates<-unique(ysi$time)

library(plyr) #only use plyr for this for loop, then detach!

#For loop to retrieve ysi depth with the closest function and fill in matrix
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(ysi, ysi$time == j)
  
  layer1<- q[q[, "depth"] == closest(q$depth,0.1),][1,]
  layer2<- q[q[, "depth"] == closest(q$depth,1),][1,]
  layer3<- q[q[, "depth"] == closest(q$depth,2),][1,]
  layer4<- q[q[, "depth"] == closest(q$depth,3),][1,]
  layer5<- q[q[, "depth"] == closest(q$depth,4),][1,]
  layer6<- q[q[, "depth"] == closest(q$depth,5),][1,]
  layer7<- q[q[, "depth"] == closest(q$depth,6),][1,]
  layer8<- q[q[, "depth"] == closest(q$depth,7),][1,]
  layer9<- q[q[, "depth"] == closest(q$depth,8),][1,]
  layer10<-q[q[, "depth"] == closest(q$depth,9),][1,]
  layer11<- q[q[, "depth"] == closest(q$depth,10),][1,]
  layer12<- q[q[, "depth"] == closest(q$depth,11),][1,]
  
  temp<-rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12)
  temp[,((ncol(ysi))+1)] <- depths
  colnames(temp)[((ncol(ysi))+1)]<-"new_depth"
  final <- temp
  final <- data.frame(final)
  super_final_ysi <- rbind(super_final_ysi,final)
}

detach(package:plyr)#to prevent issues with dplyr vs plyr not playing well together!

#now need to clean up the data frame and make all factors numeric
super_final_ysi1 <- super_final_ysi %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  dplyr::filter(time > "2015-07-07") %>% #need to make sure that the ysi data only start after first day of sim
  mutate(diff = new_depth - depth) %>% 
  dplyr::filter(abs(diff)<0.4)%>%
  select(time, new_depth, temp, DO) %>%
  rename(depth = new_depth) 

more2 <- merge(super_final1, super_final_ysi1, all.x=T, all.y=T, by=c("time", "depth"))

#visualize the data
for(i in 1:length(unique(more2$depth))){
  tempdf<-subset(more2, more2$depth==depths[i])
  plot(tempdf$time,tempdf$DO.y, type='l', col='red',
       ylab='Oxygen mmol/m3', xlab='time',
       main = paste0("YSI=Red,CTD=Black,Depth=",depths[i]),ylim=c(0,15))
  points(tempdf$time, tempdf$DO.x, type="l",col='black')
}

#remove outliers
more <- more2 %>% 
  dplyr::filter((depth == 0.1 & DO.x > 5) |
                  (depth == 0.1 & DO.y > 5) |
                  (depth == 1 & DO.x > 5.5) |
                  (depth == 1 & DO.y > 5.5) |
                  (depth == 2 & DO.x > 5) |
                  (depth == 2 & DO.y > 5) |
                  (depth == 3 & DO.x > 2) |
                  (depth == 3 & DO.y > 2) |
                  (depth == 4 & DO.x < 13) |
                  (depth == 4 & DO.y < 13) |
                  (depth == 5 & DO.x < 13) |
                  (depth == 5 & DO.y < 13) |
                  (depth == 6 & DO.x < 13) |
                  (depth == 6 & DO.y < 13) |
                  (depth == 7 & DO.x < 12.5) |
                  (depth == 7 & DO.y < 12.5) |
                  (depth == 8 & DO.x < 12.5) |
                  (depth == 8 & DO.y < 12.5) |
                  (depth == 9 & DO.x < 12.5) |
                  (depth == 9 & DO.y < 12.5) |
                  (depth == 10 & DO.x < 12.5) |
                  (depth == 10 & DO.y < 12.5) |
                  (depth == 11 & DO.x < 12.5) |
                  (depth == 11 & DO.y < 12.5)) 

#visualize the DO data
for(i in 1:length(unique(more$depth))){
  tempdf<-subset(more, more$depth==depths[i])
  plot(tempdf$time,tempdf$DO.y, type='l', col='red',
       ylab='Oxygen mmol/m3', xlab='time',
       main = paste0("YSI=Red,CTD=Black,Depth=",depths[i]),ylim=c(0,15))
  points(tempdf$time, tempdf$DO.x, type="l",col='black')
}

#combine DO & temp data from both YSI & CTD, defaults to CTD if both are present 
for(i in 1:length(more$time)){
  if(is.na(more$temp.x[i])==F){
    more$temp_new[i]=more$temp.x[i]
  }
  if(is.na(more$temp.x[i])==T & is.na(more$temp.y[i])==F){
    more$temp_new[i]=more$temp.y[i]
  }
  if(is.na(more$temp.x[i])==T & is.na(more$temp.y[i])==T){
    more$temp_new[i]=NA
  }
  if(is.na(more$DO.x[i])==F){
    more$DO_new[i]=more$DO.x[i]
  }
  if(is.na(more$DO.x[i])==T & is.na(more$DO.y[i])==F){
    more$DO_new[i]=more$DO.y[i]
  }
  if(is.na(more$DO.x[i])==T & is.na(more$DO.y[i])==T){
    more$DO_new[i]=NA
  }
}     

more1 <- more %>% 
  select(time, depth,temp_new,DO_new, chla) %>% 
  rename(temp = temp_new, DO = DO_new) 

#visualize the DO data
for(i in 1:length(unique(more1$depth))){
  tempdf<-subset(more1, more1$depth==depths[i])
  plot(tempdf$time,tempdf$DO, type='p', col='red',
       ylab='Oxygen mg/L', xlab='time',
       main = paste0("Combined CTD & YSI data,Depth=",depths[i]),ylim=c(0,15))
}

#visualize the temp data
for(i in 1:length(unique(more1$depth))){
  tempdf<-subset(more1, more1$depth==depths[i])
  plot(tempdf$time,tempdf$temp, type='p', col='red',
       ylab='Temp oC', xlab='time',
       main = paste0("Combined CTD & YSI data,Depth=",depths[i]),ylim=c(0,30))
}

#visualize the chla data
for(i in 1:length(unique(more1$depth))){
  tempdf<-subset(more1, more1$depth==depths[i])
  plot(tempdf$time,tempdf$chla, type='p', col='red',
       ylab='Chla ug/L', xlab='time',
       main = paste0("Combined CTD & YSI data,Depth=",depths[i]),ylim=c(0,30))
}


#export CTD data!
temp <- more1 %>%
  select(time, depth, temp) %>%
 #dplyr::filter(!(time==as_date("2014-05-28") & depth == 9.2)) %>% 
 #dplyr::filter(!(time==as_date("2017-03-27") & depth == 8)) %>% 
 #dplyr::filter(!(time==as_date("2013-08-12") & depth == 7)) %>% 
 #dplyr::filter(!(time==as_date("2016-06-28") & depth == 7)) %>% 
 #dplyr::filter(!(time==as_date("2013-08-12") & depth == 6)) %>% 
 #dplyr::filter(!(time==as_date("2014-05-28") & depth == 6)) %>% 
 #dplyr::filter(!(time==as_date("2013-08-12") & depth == 5)) %>% 
 #dplyr::filter(!(time==as_date("2013-08-12") & depth == 4)) %>% 
 #dplyr::filter(!(time==as_date("2013-08-12") & depth == 3)) %>% 
 #dplyr::filter(!(time==as_date("2014-05-28") & depth == 3)) %>% 
 #dplyr::filter(!(time==as_date("2013-08-12") & depth == 2)) %>% 
 #dplyr::filter(!(time==as_date("2014-05-28") & depth == 2)) %>% 
 #dplyr::filter(!(time==as_date("2014-05-28") & depth == 1)) %>% 
  rename(DateTime = time, Depth = depth) %>% 
  drop_na() %>% 
  write.csv("field_data/CleanedObsTemp.csv", row.names = F)

oxygen <- more1 %>%
  select(time, depth, DO) %>%
  rename(DateTime = time, Depth = depth, OXY_oxy=DO) %>%
  mutate(OXY_oxy = OXY_oxy*1000/32) %>% #to convert mg/L to molar units
  drop_na() %>% 
  write.csv("field_data/CleanedObsOxy.csv", row.names = F)

chla <- more1 %>%
  select(time, depth, chla) %>%
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 9.2)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 9)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 8)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 7)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 6)) %>% 
 # dplyr::filter(!(time==as_date("2016-07-26") & depth == 6)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 5)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 2)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 1)) %>% 
 # dplyr::filter(!(time==as_date("2015-04-16") & depth == 0.1)) %>% 
  rename(DateTime = time, Depth = depth, PHY_tchla=chla) %>%
  drop_na() %>% 
  write.csv("field_data/CleanedObsChla.csv", row.names = F)

# Find depth profiles to use for initial model conditions (2015-07-09)
init_data <- more1[more1$time %in% as.POSIXct("2015-07-09"),]

init_interp <- data.frame("depth" = seq(0,12,by=0.5)) 

init_interp$depth[1] <- 0.1

init_interp <- full_join(init_data, init_interp, by="depth") |> 
  arrange(depth) |> select(depth,temp,DO)

#add last value to bottom depth for interpolation
init_interp[init_interp$depth==12,] <- c(12,7.62,0.07)

#interpolate
init_interp$temp <- zoo::na.approx(init_interp$temp)
init_interp$DO <- zoo::na.approx(init_interp$DO)

#now get DO in correct units
init_interp$DO <- init_interp$DO*1000/32

more1$depth <- as.numeric(more$depth)
more1$temp <- as.numeric(more1$temp)
more1$DO <- as.numeric(more1$DO)

ggplot(more1,mapping=aes(x=time,y=temp,color=as.factor(depth)))+
  geom_line()

ggplot(more1,mapping=aes(x=time,y=DO,color=as.factor(depth)))+
  geom_line()

ggplot(more1,mapping=aes(x=time,y=chla,color=as.factor(depth)))+
  geom_line()

ggplot(more1,mapping=aes(x=temp,y=-depth,color=as.factor(time)))+
  geom_line()

sup <- more1 %>% mutate(doy = yday(time)) %>% filter(doy %in% c(186,187,188,189,190,191))

ggplot(sup,mapping=aes(x=DO,y=-depth,color=as.factor(time)))+
  geom_line()

sup <- sup %>% group_by(depth) %>% summarize_all(funs(mean)) # Average of 07-09-2015-2020

# Convert oxygen to correct units
sup <- sup %>% mutate(DO = DO*1000/32) #to convert mg/L to molar units

# Extrapolate to initial depths needed
depth <- c(0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 
           7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11)
#initdepths <- rep(-99,length(depth))
#
#initdepths <- cbind.data.frame(depth,initdepths)
#initdepths <- merge(initdepths, sup, by="depth", all.x=TRUE, all.y=TRUE)
#
#initdepths <- initdepths %>% mutate(temp = na.fill(na.approx(temp,na.rm=FALSE),"extend")) %>% 
#  mutate(DO = na.fill(na.approx(DO,na.rm=FALSE),"extend")) %>% 
#  select(depth,temp,DO,chla) #%>% 
# # write.csv("Init_CTD.csv", row.names = F)

###########################################################
###### WATER CHEM DATA FROM EDI

#now let's build a chemistry field_data file
#first pull in FCR chem data from 2013-2022 from EDI
#inUrl1  <-  "https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76" 
#infile1 <- paste0(getwd(),"/field_data/chem_2013_2022.csv")
#download.file(inUrl1,infile1,method="curl")

BVRchem <- read.csv("field_data/chem_2013_2022.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="BVR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  filter(DateTime > as.Date("2015-07-06") &
           DateTime < as.Date("2022-05-05")) %>%
  select(DateTime, Depth_m, NH4_ugL:DIC_mgL) %>%
  rename(Depth=Depth_m) %>%
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
  mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in recalcitrant DOC pool
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
  #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  select(DateTime, Depth, NIT_amm:CAR_dic) %>%
  drop_na(NIT_amm) %>%
  filter(OGM_docr<500) %>% #remove high DOC outliers
  distinct(DateTime, Depth, .keep_all=TRUE)
 
ggplot(BVRchem, aes(DateTime, OGM_docr, colour=Depth)) + 
  geom_point()

write.csv(BVRchem, "field_data/field_chem_2DOCpools.csv", row.names = F)

#get initial conditions for 'NIT_amm','NIT_nit','PHS_frp','OGM_doc'
init_chem <- data.frame("Depth" = seq(0,12, by=0.5))

chem_init <- BVRchem |> filter(DateTime %in% c("2015-07-16")) |> 
  select(Depth, NIT_amm, NIT_nit, PHS_frp, OGM_doc)
#16Jul is closest day with full nutrients at all the depths

chem_init_final <- merge(init_chem, chem_init, by="Depth", all.x=TRUE, all.y=TRUE)

chem_init_final <- chem_init_final %>% 
  mutate(NIT_amm = na.fill(na.approx(NIT_amm,na.rm=FALSE),"extend"),
         NIT_nit = na.fill(na.approx(NIT_nit,na.rm=FALSE),"extend"),
         PHS_frp = na.fill(na.approx(PHS_frp,na.rm=FALSE),"extend"),
         OGM_doc = na.fill(na.approx(OGM_doc,na.rm=FALSE),"extend")) |> 
  filter(!Depth %in% c(0))

### Plot some exploratory graphs for Chem
ggplot(BVRchem,aes(DateTime,NIT_nit,colour=as.factor(Depth)))+
  geom_point()+
  theme_classic(base_size = 15)

ggplot(BVRchem,aes(DateTime,NIT_amm,colour=as.factor(Depth)))+
  geom_point()+
  theme_classic(base_size = 15)

ggplot(BVRchem,aes(DateTime,PHS_frp,colour=as.factor(Depth)))+
  geom_point()+
  theme_classic(base_size = 15)

ggplot(BVRchem,aes(DateTime,OGM_doc,colour=as.factor(Depth)))+
  geom_point()+
  theme_classic(base_size = 15)

###########################################################
###### ANCILLARY LAB CHEMISTRY DATASETS NEEDED FOR CALIBRATION 

#read in lab dataset of dissolved silica, measured by Jon in summer 2014 only
silica <- read.csv(file.path(getwd(),"inputs/BVR2014_Chemistry.csv"), header=T) %>%
  select(Date, Depth, DRSI_mgL) %>%
  mutate(Date = as.POSIXct(strptime(Date, "%m/%d/%Y", tz="EST"))) %>%
  rename(DateTime = Date, SIL_rsi=DRSI_mgL) %>%
  mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) #convert to molar units

silica <- na.omit(silica)

write.csv(silica, "field_data/field_silica.csv", row.names = F)

#read in lab dataset of dissolved methane concentrations, measured in FCR
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/551/8/454c11035c491710243cae0423efbe7b" 
infile1 <- paste0(getwd(),"/field_data/ghg_2015_2023.csv")
download.file(inUrl1,infile1,method="curl")

ch4 <- read.csv(file.path(getwd(),"field_data/ghg_2015_2023.csv"), header=T) %>%
  dplyr::filter(Reservoir=="BVR") %>%
  dplyr::filter(Site == 50) %>% #just deep site
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  dplyr::filter(DateTime > as.Date("2015-07-07")) %>%
  rename(Depth = Depth_m, CAR_ch4 = CH4_umolL, CAR_pCO2 = CO2_umolL) %>%
  select(DateTime, Depth, CAR_ch4, CAR_pCO2) %>%
  mutate(CAR_pCO2 = CAR_pCO2*(0.0018/1000000)/0.0005667516) %>% #to convert umol/L to pCO2
  group_by(DateTime, Depth) %>%
  summarise(CAR_pCO2=mean(CAR_pCO2,na.rm=TRUE), CAR_ch4=mean(CAR_ch4,na.rm=TRUE))
write.csv(ch4, "field_data/field_gases.csv", row.names=F)

### Exploratory graphs
ggplot(ch4,aes(DateTime,CAR_pCO2,colour=as.factor(Depth)))+
  geom_point()+
  theme_classic(base_size = 15)

ggplot(ch4,aes(DateTime,CAR_ch4,colour=as.factor(Depth)))+
  geom_point()+
  theme_classic(base_size = 15)

###########################################################
###### SECCHI DATA FROM EDI

#first pull in Secchi data from 2013-2022 from EDI
#inUrl1  <-  "https://pasta.lternet.edu/package/data/eml/edi/198/11/81f396b3e910d3359907b7264e689052" 
#infile1 <- paste0(getwd(),"/field_data/Secchi_depth_2013-2022.csv")
#download.file(inUrl1,infile1,method="curl")

secchi <- read.csv("field_data/Secchi_depth_2013-2022.csv", header=T) %>%
  dplyr::filter(Reservoir=="BVR") %>%
  dplyr::filter(Site==50) %>%
  dplyr::filter(Flag_Secchi_m==0) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S", tz="EST"))) %>%
  mutate(Depth = rep(1, length(DateTime))) %>%
  mutate(extc_coef = Secchi_m/1.7) %>%
  select(DateTime, Depth, Secchi_m, extc_coef) %>%
  filter(DateTime > as.Date("2015-07-07")) |> 
  distinct()
write.csv(secchi, "field_data/field_secchi.csv", row.names=F)
  
  
###########################################################
###### pH DATA FROM EDI

#now pull in YSI data to get pH observations
pH <- read_csv('field_data/YSI_PAR_profiles_2013-2022.csv') %>% 
  dplyr::filter(Reservoir == "BVR") %>% 
  dplyr::filter(Site == 50) %>% 
  select(DateTime:Depth_m, pH) %>% 
  rename(time = DateTime, depth = Depth_m) %>% 
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>% 
  drop_na() 

write.csv(pH, "field_data/field_pH.csv", row.names=F)

ggplot(pH, aes(time, pH, colour= depth)) + 
  geom_point() 

###########################################################
###### ZOOPS FROM EDI 

#first pull in zoop data from 2014-2022 from EDI
#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/197/3/9eb6db370194bd3b2824726d89a008a6" 
#infile1 <- paste0(getwd(),"/field_data/zoop_summary_2014-2022.csv")
#try(download.file(inUrl1,infile1,method="curl"))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

#read in bvr tow data
zoops <- read.csv("field_data/zoop_summary_2014-2022.csv", header=T) |>
  filter(CollectionMethod=="Tow" & Reservoir %in% c("BVR") &
           StartDepth_m > 7.1) |> 
  select(-c(Site,EndDepth_m,CollectionMethod, Reservoir))

#split data into pre 2019 and post
zoops_2015_2016 <- read.csv("field_data/EDI_zoop_taxa_2015.csv")
#cyclopoids are a bit underestimated bc JPD only counted them in rep 1, but I think it's okay for biomass bc they make up a small proportion of total copepod biomass

#zoops_2014_2016 <- zoops[as.Date(zoops$DateTime)<"2019-01-01",]
zoops_2019_2021 <- zoops[as.Date(zoops$DateTime)>="2019-01-01",]

#calculate cladoceran, copepod, and rotifer biomass for 2014-2018 data
zoops_final_pre <- zoops_2015_2016 |> 
  mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")) |> 
  filter(hour(DateTime) %in% c(9,10,11,12,13,14)) |> #drop nighttime samples
  filter(Taxon %in% c("Cladocera","Copepoda","Rotifera")) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  group_by(StartDepth_m, DateTime, Taxon) |> 
  summarise(Biomass_ugL = mean(Biomass_ugL, na.rm=T),
            Size_mm = mean(MeanLength_mm, na.rm=T))

#calculate cladoceran, copepod, and rotifer biomass for 2014-2018 data
#zoops_3groups_pre <- zoops_2014_2016 |> 
#  group_by(DateTime, StartDepth_m) |> 
#  summarise(Cladocera_Biomass_ugL = sum(Biomass_ugL[
#    Taxon %in% c("Bosmina","D. catawba", "Chydorus","D. ambigua",
#                 "Diaphanosoma","Ceriodaphnia")], na.rm=T),
#    Copepoda_Biomass_ugL = sum(Biomass_ugL[
#      Taxon %in% c("Diaptomus","Nauplii", "Cyclopoids")], na.rm=T),
#    Rotifera_Biomass_ugL = sum(Biomass_ugL[
#      Taxon %in% c("Total Rotifers")], na.rm=T))

#zoops_final_pre <- zoops_3groups_pre |> 
#  group_by(DateTime, StartDepth_m) |> 
#  pivot_longer(cols=Cladocera_Biomass_ugL:Rotifera_Biomass_ugL,
#               names_to = c("Taxon"),
#               values_to = "Biomass_ugL") |> 
#  filter(hour(DateTime) %in% c(9,10,11,12,13,14)) |> #drop nighttime samples
#  mutate(DateTime = as.Date(DateTime)) |> 
#  mutate(Biomass_ugL = Biomass_ugL * (1/0.031)) |>  #correct for net inefficiency
#  mutate(Taxon = str_extract(Taxon, "[^_]+")) #extract taxon before the first _

#average reps when appropriate
zoops_final_post <- zoops_2019_2021 |> 
  mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")) |> 
  filter(hour(DateTime) %in% c(9,10,11,12,13,14)) |> #drop nighttime samples
  filter(Taxon %in% c("Cladocera","Copepoda","Rotifera")) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  group_by(StartDepth_m, DateTime, Taxon) |> 
  summarise(Biomass_ugL = mean(Biomass_ugL, na.rm=T),
            Size_mm = mean(MeanLength_mm, na.rm=T))

#combine all zoop data
all_zoops <- bind_rows(zoops_final_pre, zoops_final_post) |> 
  mutate_all(~replace(., is.nan(.), NA)) |>  #replace NAN with NA
  ungroup() |> select(-StartDepth_m) #dropping, but note that depths range from 8-11.5m....

#reformat for glm aed
all_zoops_final <- all_zoops |> group_by(Taxon) |> 
  select(-Size_mm) |>
  pivot_wider(names_from = Taxon, values_from = Biomass_ugL,
               names_glue = "{Taxon}_Biomass_ugL") |> 
  rename(ZOO_cladoceran = Cladocera_Biomass_ugL,
         ZOO_copepod = Copepoda_Biomass_ugL,
         ZOO_rotifer = Rotifera_Biomass_ugL) |> 
  mutate(ZOO_rotifer = ifelse(DateTime < "2019-01-01", NA, ZOO_rotifer)) |>  #bc JPD did not calculate rotifer biomass
  mutate(ZOO_cladoceran = ZOO_cladoceran * 1000 * (1 / 12.011) * (1 / 1000),
         ZOO_copepod = ZOO_copepod * 1000 * (1 / 12.011) * (1 / 1000),
         ZOO_rotifer = ZOO_rotifer * 1000 * (1 / 12.011) * (1 / 1000)) #convert from ug/L to mmol/m3 C 

#write.csv(all_zoops_final, "field_data/field_zoops.csv", row.names=F)


ggplot(all_zoops_final, aes(DateTime,ZOO_cladoceran)) + 
  geom_point() + theme_bw() 
ggplot(all_zoops_final, aes(DateTime,ZOO_copepod)) + 
  geom_point() + theme_bw() 
ggplot(all_zoops_final, aes(DateTime,ZOO_rotifer)) + 
  geom_point() + theme_bw() 

#add doy and year column
all_zoops_final$doy <- yday(all_zoops_final$DateTime)
all_zoops_final$year <- year(all_zoops_final$DateTime)

#look at doy on x and year by color
ggplot(all_zoops_final, aes(doy, ZOO_cladoceran, color=as.factor(year))) + 
  geom_point() + theme_bw() + geom_line()

# zoop size histogram for the three focal taxa (supplemental figure)
ggplot(all_zoops, aes(Size_mm, fill=Taxon)) +
  geom_density(alpha = 0.6) + theme_bw() +
  scale_fill_manual(values = c("#084c61","#db504a","#e3b505"),
                    breaks = c("Cladocera","Copepoda","Rotifera"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.6, 0.9),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.margin = margin(c(-10,-10,-10,-10)),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/taxa_size_density_plot.jpg", width=4, height=3)
