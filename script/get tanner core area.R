#Define tanner crab core area as stations with 50th percentile of cpue 

# Erin Fedewa

# load ----
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#Tanner haul data 
tanner_haul <- read.csv("./Data/crabhaul_bairdi.csv")

#Tanner strata data 
tanner_strata <- read_csv("./Data/crabstrata_bairdi.csv")

###############################

#Calculate CPUE by station for all tanner crab 
tanner_haul %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT,MID_LATITUDE, MID_LONGITUDE, GEAR_TEMPERATURE) %>%
  summarise(N_CRAB = sum(SAMPLING_FACTOR, na.rm = T),
            CPUE = N_CRAB / mean(AREA_SWEPT)) %>%
  #join to zero catch stations
  right_join(tanner_strata %>%
               filter(SURVEY_YEAR > 1987) %>%
               distinct(SURVEY_YEAR, STATION_ID, STRATUM, TOTAL_AREA) %>%
               rename_all(~c("YEAR", "GIS_STATION", 
                             "STRATUM", "TOTAL_AREA"))) %>%
  replace_na(list(CPUE = 0)) -> cpue

#stations in 50-100 CPUE percentile range
cpue %>%
  group_by(GIS_STATION) %>%
  summarise(AVG_CPUE = mean(CPUE)) %>%
  filter(AVG_CPUE > quantile(AVG_CPUE, 0.50)) -> perc50 #187 stations
#Lets go with the 50th percentile for defining core area 

#Join lat/long back in to perc50 dataset 
tanner_strata %>%
  filter(SURVEY_YEAR == 2021) %>% #Just selecting a yr when all stations were sampled
  select(STATION_ID, LATITUDE, LONGITUDE) %>%
  dplyr::rename(GIS_STATION = STATION_ID) %>%
  right_join(perc50) -> perc50_core

#Quick plot
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_point(data = perc50_core, aes(x = LONGITUDE, y = LATITUDE), size = 2, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-178, -159), ylim = c(53, 61), expand = FALSE) +
  theme_bw()

#Write csv for stations in 50th percentile of avg CPUE  
write.csv(perc50_core, file="./Output/tanner_area_50perc.csv")