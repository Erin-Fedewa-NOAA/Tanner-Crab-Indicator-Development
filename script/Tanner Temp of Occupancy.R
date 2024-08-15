# notes ----
#Calculate tanner crab temperatures of occupancy (CPUE weighted) 

#Author: Erin Fedewa

# load ----
library(tidyverse)

#Tanner haul data 
tanner_haul <- read.csv("./Data/crabhaul_bairdi.csv")

#########################################
## compute cpue by size-sex group for each station

tanner_haul %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < 103 | SEX == 2 & CLUTCH_SIZE == 0, "Immature",
                           ifelse(SEX == 2 & CLUTCH_SIZE >= 1 | SEX == 1 & WIDTH_1MM >= 103,  "Mature", NA))) %>%
  group_by(YEAR, GIS_STATION, MID_LATITUDE, MID_LONGITUDE, AREA_SWEPT, GEAR_TEMPERATURE, size_sex) %>%
  summarise(num_crab = round(sum(SAMPLING_FACTOR))) %>%
  filter(!is.na(AREA_SWEPT)) %>%
  pivot_wider(names_from = size_sex, values_from = num_crab) %>%
  mutate(pop = sum(Immature, Mature, na.rm = T)) %>%
  pivot_longer(c(7:9), names_to = "size_sex", values_to = "num_crab") %>%
  filter(size_sex != "NA") %>%
  mutate(num_crab = replace_na(num_crab, 0),
         cpue = num_crab / AREA_SWEPT) %>%
  ungroup() -> cpue_long

#Temperature of Occupancy Calculations ----
  #Note that this is ignoring NA's in bottom temp data!!! These missing values should be 
  #imputed in future iterations 
cpue_long %>%
  group_by(YEAR) %>%
  mutate(AVG_BT = mean(GEAR_TEMPERATURE, na.rm=T)) %>%
  ungroup() %>%
  group_by(YEAR, size_sex, AVG_BT) %>%
  summarise(TEMP_OCC = weighted.mean(GEAR_TEMPERATURE, w = cpue, na.rm = T)) -> temp_occ

#plot
temp_occ %>%
  filter(size_sex != "pop") %>%
  ggplot(aes(x = YEAR, y = TEMP_OCC, group= size_sex, color = size_sex))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() +
  labs(x="", y="Temperature Occupied (C)") +
  theme(legend.title=element_blank()) 
ggsave(path = "./figs", "Tanner_Temp_Occupied.png")

#Write output for Temp Occupancy indicator     
temp_occ %>%
  select(-AVG_BT) %>%
  pivot_wider(names_from = "size_sex", values_from = "TEMP_OCC") %>%
  write.csv(file="./Output/Temp_Occupied.csv")


