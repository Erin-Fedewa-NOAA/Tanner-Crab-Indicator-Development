# notes ----
# Generate avg bottom temp, and cold pool extent indices from EBS BT timeseries 

#Author: Erin Fedewa

# load ----
library(tidyverse)

#Corner station look up table for cold pool extent calculations 
corner <- c("QP2625","ON2625","HG2019","JI2120","IH1918",
            "GF2221","HG1918","GF2019","ON2524","PO2726",
            "IH2221","GF1918","JI2221","JI2019","JI1918",
            "HG2221","QP2726","PO2423","IH2019","PO2625",
            "QP2423","IH2120","PO2524","HG2120","GF2120",
            "QP2524")

# BT survey data ----
temp <- read_csv("./data/crabhaul_bairdi.csv") 

#Number of stations with data each yr 
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(!(GIS_STATION %in% corner)) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(GIS_STATION))) %>%
  print(n=50)
#Let's use 1988+ but in future iterations, missing temperature data 
  #should be imputed 

# compute mean summer bottom temperature
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(YEAR >= 1988,
         HAUL_TYPE == 3) %>%
  distinct(YEAR, GIS_STATION, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T)) -> avg_bt

#Plot
avg_bt %>%
  ggplot(aes(x = as.numeric(YEAR), y = summer_bt)) +
  geom_point() +
  geom_line()+
  labs(y = "Bottom Temperature (C)", x = "") +
  geom_hline(aes(yintercept = mean(summer_bt, na.rm=TRUE)), linetype = 5)+
  xlim(1988, 2024) +
  theme_bw()
ggsave(path = "./figs", "Bottom_Temp.png")

#compute cold pool areal extent
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(YEAR >= 1988,
         HAUL_TYPE == 3,
         !(GIS_STATION %in% corner)) %>%
  distinct(YEAR, GIS_STATION, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(cpa = sum(GEAR_TEMPERATURE < 2, na.rm = T) * 401) -> cp_extent

#Plot
cp_extent %>%
  ggplot(aes(x = as.numeric(YEAR), y = cpa)) +
  geom_point() +
  geom_line()+
  labs(y = "Cold Pool Extent (nmi2)", x = "") +
  geom_hline(aes(yintercept = mean(cpa, na.rm=TRUE)), linetype = 5)+
  xlim(1988, 2024) +
  theme_bw()
ggsave(path = "./figs", "ColdPool_Extent.png")

# combine indices and save output
avg_bt %>%
  full_join(cpa) %>%
  rename("cp_extent" = "cpa") -> env
write_csv(env, "./output/temp_coldpool.csv")








