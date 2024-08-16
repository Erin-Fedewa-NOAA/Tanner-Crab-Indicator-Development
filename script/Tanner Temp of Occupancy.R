# notes ----
#Calculate tanner crab temperatures of occupancy (CPUE weighted) 

#Author: Erin Fedewa

# load ----
library(tidyverse)

#Tanner haul data 
tanner_haul <- read.csv("./Data/crabhaul_bairdi.csv")

#size at 50% maturity lookup
#we'll use this to assign male maturity by year, but b/c were missing 
#years, we'll assign with static 103mm cutline, which is nearly eq. to 104mm timeseries mean
read_csv("./output/size_at_mat.csv") %>%
  select(Year, SAM_pop) %>%
  add_row(Year = c(1975:1989, 2013, 2015), SAM_pop = 103) %>%
  mutate(across(SAM_pop, round, 2)) %>%
  rename(YEAR = Year, male_cutline = SAM_pop) -> mat

#########################################
## compute cpue by size-sex group for each station

tanner_haul %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  left_join(mat) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < male_cutline | SEX == 2 & CLUTCH_SIZE == 0, "Immature",
                           ifelse(SEX == 2 & CLUTCH_SIZE >= 1 | SEX == 1 & WIDTH_1MM >= male_cutline,  "Mature", NA))) %>%
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
  geom_hline(aes(yintercept = mean(TEMP_OCC, na.rm=TRUE)), linetype = 5)+
  labs(x="", y="Temperature Occupied (C)") +
  theme(legend.title=element_blank()) 
ggsave(path = "./figs", "Tanner_Temp_Occupied.png")

#Write output for Temp Occupancy indicator     
temp_occ %>%
  select(-AVG_BT) %>%
  pivot_wider(names_from = "size_sex", values_from = "TEMP_OCC") %>%
  write.csv(file="./Output/Temp_Occupied.csv")


