# notes ----
#Summarize benthic predator and pcod mean CPUE across years in core tanner habitat

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)

#Tanner haul data 
tanner_haul <- read.csv("./Data/crabhaul_bairdi.csv")

#Tanner strata data 
tanner_strata <- read_csv("./Data/crabstrata_bairdi.csv")

#Load groundfish data queried directly from Racebase (see "get gf data.R" script)
pred <- read.csv("./data/gf_cpue_timeseries.csv")

#Load tanner core area (see "get tanner core area.R" script)
sta <- read_csv("./output/tanner_area_50perc.csv")

##################################################
#Use core area dataset to spatially subset gf data 
sta %>% 
  pull(GIS_STATION) -> core

#Calculate mean CPUE for each predator guild across years 
  #Specifying each species here because stomach contents/diets were validated for most
  #and included if assumed to be benthic predator on crab juv/adult stages 
pred %>%
  filter(STATION %in% core, 
         YEAR >= 1988) %>%
  group_by(YEAR, STATION) %>%
  summarise(Sab_Hal_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(20510, 10120)], na.rm = T),
            Pcod_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(21720, 21722)], na.rm = T),
            Skates_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(420,435,440,455,471,472,480,460,485)], na.rm = T),
            Flatfish_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(10220,10115,10130,10140,10120,10261,10210,10260)], na.rm = T),
            Sculpin_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(21347,21348,21368,21370,21388,21420,21311,21315,21390,21438,21371)], na.rm = T),
            Eelpout_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(24184, 24191, 24185)], na.rm = T),  
            Wolfish_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(20320, 20322)], na.rm = T), 
            Octopus_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(78010, 78012, 78403)], na.rm = T)) %>% 
  group_by(YEAR) %>%
  summarise(Sab_Hal = mean(Sab_Hal_cpue),
            Pcod = mean(Pcod_cpue),
            Skates = mean(Skates_cpue),
            Flatfish = mean(Flatfish_cpue),
            Sculpin = mean(Sculpin_cpue),
            Eelpout = mean(Eelpout_cpue),
            Wolfish = mean(Wolfish_cpue),
            Octopus = mean(Octopus_cpue)) -> ben_pred

#Plots 
ben_pred %>%
  pivot_longer(c(2:9), names_to = "pred_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild)) +
  geom_line(aes(colour = pred_guild)) +
  labs(y = "Benthic Predator CPUE (kg/km2)", x = "") +
  theme_bw() +
  theme(legend.title=element_blank())
#YFS really dominates biomass here....

ben_pred %>%
  pivot_longer(c(2:9), names_to = "pred_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2))+
  geom_point() +
  geom_line() +
  labs(y = "CPUE (kg/km2)", x = "") +
  theme_bw() +
  theme(legend.title=element_blank()) +
  facet_wrap(~pred_guild, scales = "free_y")
ggsave(path="./figs", "benthic_predator.png")

#Just Pcod Plot
ben_pred %>%
  ggplot(aes(x = YEAR, y = Pcod)) +
  geom_point() +
  geom_line()+
  labs(y = "Pacific Cod CPUE (kg/km2)", x = "") +
  theme_bw()
ggsave(path="./figs", "pcod_density.png")

write.csv(ben_pred, file = "./output/benthic_predator.csv")
