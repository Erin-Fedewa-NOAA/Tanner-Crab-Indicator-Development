# notes ----
#Create master csv of Tanner crab ecosystem indicators for Markdown

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
sam <- read_csv("./output/size_at_mat.csv") #Contributor: Jon Richar
chla <- read_csv("./output/chla.csv") #Contributor: Matt Callahan
pcod <- read_csv("./output/benthic_predator.csv")

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
              select(Year, SAM_pop) %>%
              rename(Annual_Tanner_Crab_Male_Size_Maturity_Model = SAM_pop,
                  YEAR = Year)) %>%
  full_join(chla) %>%
  full_join(pcod %>%
              select(YEAR, Pcod) %>%
              rename(Summer_Pacific_Cod_Density_Tanner_Survey = Pcod)) %>%
  rename(year = YEAR) %>%
  filter(year >= 1982) %>%
  arrange(year) -> eco_ind 

write_csv(eco_ind, "./data/tanner_eco_indicators.csv")

#########################
#Plots for contributor indicators 

sam %>%
  select(Year, SAM_pop, SAM_E166, SAM_W166) %>%
  pivot_longer(c(2:4), names_to = "stock", values_to = "SAM") %>%
  ggplot(aes(x = Year, y = SAM, group = factor(stock)))+
  geom_point(aes(colour = stock)) +
  geom_line(aes(colour = stock)) +
  theme_bw() +
  labs(x="", y = "Size at 50% maturity (mm)")
ggsave(path = "./figs", "Size_at_mat.png")

chla %>%
  ggplot(aes(x = YEAR, y = AMJ_Chlorophylla_Biomass_Tanner_Satellite)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_hline(aes(yintercept = mean(AMJ_Chlorophylla_Biomass_Tanner_Satellite, 
                                   na.rm=TRUE)), linetype = 5)+
  labs(x="", y = "Chl-a Concentration")
ggsave(path = "./figs", "chla_conc.png")
