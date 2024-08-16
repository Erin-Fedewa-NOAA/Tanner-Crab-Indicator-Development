#notes ----
#Calculate:
#a) Tanner Crab center of abundance in EBS by size/sex category
#b) Area Occupied (D95)- area of stations that make up 95% of the cumulative Tanner cpue

#Author: Erin Fedewa


#load----
library(tidyverse)
library(rsample)

#######################################

#Exclude corner stations
corner <- list("QP2625","ON2625","HG2019","JI2120","IH1918",
               "GF2221","HG1918","GF2019","ON2524","PO2726",
               "IH2221","GF1918","JI2221","JI2019","JI1918",
               "HG2221","QP2726","PO2423","IH2019","PO2625",
               "QP2423","IH2120","PO2524","HG2120","GF2120",
               "QP2524")

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

#####################################
#Quick data exploration: stations sampled in each year
tanner_haul %>%
  group_by(CRUISE) %>%
  filter(!(GIS_STATION %in% corner)) %>%
  summarise(num_stations = length(unique(GIS_STATION))) %>%
  print(n=60) #magic number is 349 here for standard stations - 26 corner stations
#Lets determine D95 from standardized timeseries (1998+), though
#noting that 1992 is problematic given missing stations  

## compute cpue by size-sex group for each station
tanner_haul %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  left_join(mat) %>%
  filter(HAUL_TYPE == 3 , 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < male_cutline, "immature_male",
                           ifelse(SEX == 1 & WIDTH_1MM >= male_cutline, "mature_male",
                                  ifelse(SEX == 2 & CLUTCH_SIZE >= 1, "mature_female",
                                         ifelse(SEX == 2 & CLUTCH_SIZE == 0, "immature_female", NA))))) %>%
  group_by(YEAR, GIS_STATION, MID_LATITUDE, MID_LONGITUDE, AREA_SWEPT, size_sex) %>%
  summarise(num_crab = round(sum(SAMPLING_FACTOR))) %>%
  filter(!is.na(AREA_SWEPT)) %>%
  pivot_wider(names_from = size_sex, values_from = num_crab) %>%
  mutate(pop = sum(immature_male, mature_male, immature_female, mature_female, na.rm = T)) %>%
  pivot_longer(c(6:10), names_to = "size_sex", values_to = "num_crab") %>%
  filter(size_sex != "NA") %>%
  mutate(num_crab = replace_na(num_crab, 0),
         cpue = num_crab / AREA_SWEPT) %>%
  ungroup() -> cpue_long

###############################
#Compute Tanner crab center of abundance by size/sex
cpue_long %>%
  filter(!(GIS_STATION %in% corner)) %>% #exclude corner stations
  group_by(YEAR, size_sex) %>%
  summarise(Lat_COD = weighted.mean(MID_LATITUDE, w = cpue)) -> COD 

#plot
COD %>%
  select(YEAR, size_sex, Lat_COD) %>%
  filter(size_sex != "pop") %>%
  ggplot(aes(x = YEAR, y = Lat_COD, group= size_sex, color = size_sex))+
  geom_point(size=3)+
  theme_bw() +
  labs(x="", y="Center of Abundance") +
  theme(legend.title=element_blank()) +
  geom_line() 
ggsave(path = "./figs", "Tanner_Centroid.png")

#Write output for COD indicator     
COD %>%
  pivot_wider(names_from = "size_sex", values_from = "Lat_COD") %>%
  write.csv(file="./Output/centroid_abun.csv")

################################
# compute D95 by each size and sex category ----
#i.e. the number of stations contributing to 95% of cumulative cpue

# function to compute D95
f_d95_est <- function(x){
  x %>%
    arrange(-cpue) %>% #sort by cpue (large:small)
    mutate(prop_cpue = cpue/sum(cpue),  #calculate the proportion of total cpue for each station
           cum_cpue = cumsum(prop_cpue)) %>%  
    filter(cum_cpue <= 0.95) %>% #T if in d95, F if not
    count() %>%
    mutate(d95 = (n + 1) * 401) %>% #add 1 station to n to push over 95%, multiply by 401 nm
    pull(d95)
}

# do the estimation
cpue_long %>%
  filter(!(GIS_STATION %in% corner)) %>% #exclude corner stations
  nest(-YEAR, -size_sex) %>%
  mutate(d95 = purrr::map_dbl(data, f_d95_est)) %>% #apply d95 function to each element 
  unnest() %>%
  group_by(YEAR, size_sex) %>%
  summarise(cpue = sum(num_crab) / sum(AREA_SWEPT), # add a column for total cpue of each group in each year
            d95 = mean(d95)) -> d95 # take 'mean' just to get one value (they are all the same)

#plot
d95 %>%
  select(YEAR, size_sex, d95) %>%
  filter(size_sex != "pop") %>%
  ggplot(aes(x = YEAR, y = d95, group= size_sex, color = size_sex))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() +
  labs(x="", y="Area Occupied (nmi)") +
  theme(legend.title=element_blank()) 
ggsave(path = "./figs", "Tanner_Area_Occupied.png")

#would certainly be worthwhile to plot this against abundance, as I'd expect 
#the two to track each other

#Write output for D95 indicator     
d95 %>%
  select(-cpue) %>%
  pivot_wider(names_from = "size_sex", values_from = "d95") %>%
  write.csv(file="./Output/area_occupied.csv")






