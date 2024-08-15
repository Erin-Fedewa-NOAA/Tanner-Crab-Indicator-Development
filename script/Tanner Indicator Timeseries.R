# notes ----
#Create master csv of Tanner crab ecosystem indicators 

#Author: Erin Fedewa

# load ----
library(tidyverse)
library(corrplot)
library(cowplot)
library(mgcv)

#Ecosystem data to combine
invert <- read_csv("./output/benthic_invert.csv")
env <- read_csv("./output/temp_coldpool.csv")
d95 <- read_csv("./output/area_occupied.csv")
bcs <- read_csv("./output/bcd_prev.csv")
cod <- read_csv("./output/centroid_abun.csv")
occ <- read_csv("./output/Temp_Occupied.csv")
sam <- read_csv("./data/tanner_SAM.csv")

# combine indices and save output
invert %>%
  select(YEAR, Total_Benthic) %>%
  rename(Summer_Benthic_Invertebrate_Density_SEBS_Tanner_Survey = Total_Benthic) %>%
  full_join(env %>%
              select(YEAR, cp_extent, summer_bt) %>%
              rename(Summer_Cold_Pool_SEBS_Tanner_Survey=cp_extent, Summer_Temperature_Bottom_Tanner_Survey=summer_bt)) %>%
  full_join(d95 %>%
              select(YEAR, mature_male) %>%
              rename(Summer_Tanner_Male_Area_Occupied_SEBS_Survey=mature_male)) %>%
  full_join(bcs %>%
              select(YEAR, Immature) %>%
              rename(Summer_Tanner_Juvenile_Disease_Prevalence=Immature)) %>%
  full_join(cod %>%
              select(YEAR, mature_male) %>%
              rename(Summer_Tanner_Male_Center_Distribution_SEBS_Survey = mature_male)) %>%
  full_join(occ %>%
              select(YEAR, Immature) %>%
              rename(Summer_Tanner_Juvenile_Temperature_Occupancy = Immature)) %>%
  full_join(sam %>%
              select(YEAR, Immature) %>%
              rename(Annual_Tanner_Male_Size_Maturity_Model = Immature)) %>%
  rename(year = YEAR) %>%
  filter(year >= 1982) %>%
  arrange(year) -> eco_ind #need to add in 2020 with NAs

write_csv(eco_ind, "./data/tanner_eco_indicators.csv")

#Assess collinearity b/w indicators 
eco_ind %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="number")