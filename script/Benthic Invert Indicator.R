# notes ----
#Summarize benthic invert mean CPUE across years in tanner core area 
  #as a proxy for prey quantity. This is a very course metric though, as 
  #major prey items like polychetes and bivalves are not sampled well 

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)

#Tanner haul data 
tanner_haul <- read.csv("./Data/crabhaul_bairdi.csv")

#Tanner strata data 
tanner_strata <- read_csv("./Data/crabstrata_bairdi.csv")

#Load groundfish data queried directly from Racebase (see "get gf data.R" script)
benthic <- read.csv("./data/gf_cpue_timeseries_2024.csv")

#Load tanner core area (see "get tanner core area.R" script)
sta <- read_csv("./output/tanner_area_50perc.csv")

##################################################
#Benthic invert CPUE

#Use core area dataset to spatially subset invert data 
sta %>% 
  pull(GIS_STATION) -> core

#Num of stations with catch data each yr within core habitat
benthic %>%
  filter(STATION %in% core) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(STATION))) %>%
  print(n=50)
#A few missing stations in some years

#Calculate mean CPUE for each prey guild across years 
benthic %>%
  filter(STATION %in% core, 
         YEAR >= 1988,
         !(SPECIES_CODE %in% c(68560, 68580, 69322, 69323))) %>% #remove commercial crab species 
  group_by(YEAR, STATION) %>%
  summarise(Gersemia_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(41201:41221)], na.rm = T),
            Pennatulacea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(42000:42999)], na.rm = T),
            Actinaria_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(43000:43999)], na.rm = T),
            Polychaeta_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(50000:59099)], na.rm = T),
            Barnacles_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(65100:65211)], na.rm = T),
            Shrimps_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(66000:66912)], na.rm = T),
            Crabs_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(68000:69599)], na.rm = T),
            Gastropods_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(71000:73999)], na.rm = T),
            Bivalves_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(74000:75799)], na.rm = T),
            Asteroidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(80000:82499)], na.rm = T),
            Echinoidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(82500:82729)], na.rm = T),
            Ophiuroidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(83000:84999)], na.rm = T),
            Holothuroidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(85000:85999)], na.rm = T),
            Porifera_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(91000:91999)], na.rm = T),
            Bryozoans_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(95000:95499)], na.rm = T),
            Ascidians_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(98000:99909)], na.rm = T),
            Total_Benthic_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(41201:99909)], na.rm = T)) %>%
  group_by(YEAR) %>%
  summarise(Gersemia = mean(Gersemia_cpue),
            Pennatulacea = mean(Pennatulacea_cpue),
            Actinaria = mean(Actinaria_cpue),
            Polychaeta = mean(Polychaeta_cpue),
            Barnacles = mean(Barnacles_cpue),
            Shrimps = mean(Shrimps_cpue),
            Crabs = mean(Crabs_cpue),
            Gastropods = mean(Gastropods_cpue),
            Bivalves = mean( Bivalves_cpue),
            Asteroidea = mean(Asteroidea_cpue),
            Echinoidea = mean(Echinoidea_cpue),
            Ophiuroidea = mean(Ophiuroidea_cpue),
            Holothuroidea = mean(Holothuroidea_cpue),
            Porifera = mean(Porifera_cpue),
            Bryozoans = mean(Bryozoans_cpue),
            Ascidians = mean(Ascidians_cpue),
            Total_Benthic = mean(Total_Benthic_cpue))-> benthic_invert

#Plots 
benthic_invert %>%
  pivot_longer(c(2:17), names_to = "benthic_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(benthic_guild)))+
  geom_point(aes(colour = benthic_guild)) +
  geom_line(aes(colour = benthic_guild)) +
  # geom_hline(aes(yintercept = mean(kg/km2)), linetype = 2)+
  labs(y = "Benthic Invert CPUE (1000t/km2)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

benthic_invert %>%
  ggplot(aes(x = YEAR, y = Total_Benthic)) +
  geom_point() +
  geom_line() +
  labs(y = "Total Benthic Invert CPUE (kg/km2)", x = "") +
  geom_hline(aes(yintercept = mean(Total_Benthic, na.rm=TRUE)), linetype = 5)+
  theme_bw()+
  theme(panel.grid = element_blank()) 
ggsave(path="./figs", "benthic_invert.png")

write_csv(benthic_invert, file = "./output/benthic_invert.csv")

