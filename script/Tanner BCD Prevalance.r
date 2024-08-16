# notes ----]
# Calculate visual prevalence of BCD in Tanner crab
  #determined via area swept expansion 

#Author: Erin Fedewa

# load ----
library(tidyverse)
library(ggridges)

##############################################

#Tanner haul data 
tanner_haul <- read.csv("./data/crabhaul_bairdi.csv")

#Tanner strata data 
tanner_strata <- read_csv("./data/crabstrata_bairdi.csv")

#size at 50% maturity lookup
  #we'll use this to assign male maturity by year, but b/c were missing 
  #years, we'll assign with static 103mm cutline, which is nearly eq. to 104mm timeseries mean
read_csv("./output/size_at_mat.csv") %>%
  select(Year, SAM_pop) %>%
  add_row(Year = c(1975:1989, 2013, 2015), SAM_pop = 103) %>%
  mutate(across(SAM_pop, round, 2)) %>%
  rename(YEAR = Year, male_cutline = SAM_pop) -> mat

########################################
#compute cpue by disease code 2 for mature crab only
tanner_haul %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  left_join(mat) %>%
  filter(HAUL_TYPE == 3,
         ((SEX == 2 & CLUTCH_SIZE >= 1) |
         (SEX == 1 & WIDTH_1MM >= male_cutline))) %>% 
  mutate(bcs = ifelse(DISEASE_CODE != 2 | is.na(DISEASE_CODE), F, T)) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT, bcs) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue = ncrab / AREA_SWEPT) -> cpue 
  #add zero catch stations to bcs=T/F datasets
cpue %>% 
  filter(bcs == FALSE) %>%
  right_join(tanner_haul %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
  replace_na(list(cpue = 0)) %>%
  replace_na(list(ncrab = 0)) %>%
  replace_na(list(bcs = FALSE)) %>%
rbind(cpue %>% 
          filter(bcs == TRUE) %>%
          right_join(tanner_haul %>% 
                       mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
                       filter(HAUL_TYPE ==3) %>%
                       distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
          replace_na(list(cpue = 0)) %>%
          replace_na(list(ncrab = 0)) %>%
          replace_na(list(bcs = TRUE))) -> catch
#Now our BCS categories should each contain zero catch stations so we can group by 
  #bcs category to calculate abundances 

#BCS prevalence, mature snow crab
catch %>%
  right_join(tanner_strata %>%
               rename(GIS_STATION=STATION_ID, YEAR=SURVEY_YEAR)) %>%
  group_by(YEAR, STRATUM, bcs) %>%
  summarise(total_area = mean(TOTAL_AREA),
            mean_cpue = mean(cpue),
            abundance_mil = mean(total_area) * mean_cpue / 1000000) %>%
  group_by(YEAR, bcs) %>%
  #sum across strata
  summarise(Total_abun=sum(abundance_mil)) %>% 
  filter(!is.na(bcs)) %>%
  #calculate prevalence 
  group_by(YEAR) %>%
  summarise(Perc_Prevalance = (Total_abun[bcs==TRUE]/((Total_abun[bcs==FALSE])+(Total_abun[bcs==TRUE])))*100) %>%
  mutate(Maturity = rep("Mature")) -> matprev

###
#compute cpue by disease code 2 for immature crab only 
tanner_haul %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  left_join(mat) %>%
  filter(HAUL_TYPE == 3,
         ((SEX == 2 & CLUTCH_SIZE == 0) |
            (SEX == 1 & WIDTH_1MM < male_cutline))) %>%  
  mutate(bcs = ifelse(DISEASE_CODE != 2 | is.na(DISEASE_CODE), F, T)) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT, bcs) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue = ncrab / AREA_SWEPT) -> cpue_imm
#add zero catch stations to bcs=T/F datasets
cpue_imm %>% 
  filter(bcs == FALSE) %>%
  right_join(tanner_haul %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
  replace_na(list(cpue = 0)) %>%
  replace_na(list(ncrab = 0)) %>%
  replace_na(list(bcs = FALSE)) %>%
  rbind(cpue_imm %>% 
          filter(bcs == TRUE) %>%
          right_join(tanner_haul %>% 
                       mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
                       filter(HAUL_TYPE ==3) %>%
                       distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
          replace_na(list(cpue = 0)) %>%
          replace_na(list(ncrab = 0)) %>%
          replace_na(list(bcs = TRUE))) -> catch_imm

#BCS prevalence, immature snow crab
catch_imm %>%
  right_join(tanner_strata %>%
               rename(GIS_STATION=STATION_ID, YEAR=SURVEY_YEAR)) %>%
  group_by(YEAR, STRATUM, bcs) %>%
  summarise(total_area = mean(TOTAL_AREA),
            mean_cpue = mean(cpue),
            abundance_mil = mean(total_area) * mean_cpue / 1000000) %>%
  group_by(YEAR, bcs) %>%
  #sum across strata
  summarise(Total_abun=sum(abundance_mil)) %>% 
  filter(!is.na(bcs)) %>%
  #calculate prevalence 
  group_by(YEAR) %>%
  summarise(Perc_Prevalance = (Total_abun[bcs==TRUE]/((Total_abun[bcs==FALSE])+(Total_abun[bcs==TRUE])))*100) %>%
  mutate(Maturity = rep("Immature")) -> immprev

###
#compute cpue by disease code 2 for entire population
tanner_haul %>% 
  filter(HAUL_TYPE == 3,
         SEX %in% c(1,2)) %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}")),
         bcs = ifelse(DISEASE_CODE != 2 | is.na(DISEASE_CODE), F, T)) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT, bcs) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue = ncrab / AREA_SWEPT) -> cpue_pop
#add zero catch stations to bcs=T/F datasets
cpue_pop %>% 
  filter(bcs == FALSE) %>%
  right_join(tanner_haul %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
  replace_na(list(cpue = 0)) %>%
  replace_na(list(ncrab = 0)) %>%
  replace_na(list(bcs = FALSE)) %>%
  rbind(cpue_pop %>% 
          filter(bcs == TRUE) %>%
          right_join(tanner_haul %>% 
                       mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
                       filter(HAUL_TYPE ==3) %>%
                       distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
          replace_na(list(cpue = 0)) %>%
          replace_na(list(ncrab = 0)) %>%
          replace_na(list(bcs = TRUE))) -> catch_pop

#BCS prevelance of entire population
catch_pop %>%
  right_join(tanner_strata %>%
               rename(GIS_STATION=STATION_ID, YEAR=SURVEY_YEAR)) %>%
  group_by(YEAR, STRATUM, bcs) %>%
  summarise(total_area = mean(TOTAL_AREA),
            mean_cpue = mean(cpue),
            abundance_mil = mean(total_area) * mean_cpue / 1000000) %>%
  group_by(YEAR, bcs) %>%
  #sum across strata
  summarise(Total_abun=sum(abundance_mil)) %>% 
  filter(!is.na(bcs)) %>%
  #calculate prevalence 
  group_by(YEAR) %>%
  summarise(Perc_Prevalance = (Total_abun[bcs==TRUE]/((Total_abun[bcs==FALSE])+(Total_abun[bcs==TRUE])))*100) %>%
  mutate(Maturity = rep("Population")) -> popprev

#join datasets
popprev %>%
  full_join(immprev) %>% 
  full_join(matprev) %>%
  filter(YEAR > 1988) %>%
  pivot_wider(names_from = "Maturity", values_from = "Perc_Prevalance") -> bcd

#Write csv for snow crab indicator 
write.csv(bcd, file="./output/bcd_prev.csv")

###################################################
year = 2024

#Combined Plot 
bcd %>%
  pivot_longer(2:4, names_to="Maturity", values_to="Perc_prev") %>%
    ggplot(aes(x = YEAR, y = Perc_prev, group = as.factor(Maturity))) +
    geom_point(aes(colour = Maturity), size=3) +
    geom_line(aes(colour = Maturity), size=1) +
    labs(y = "Disease Prevalence (%)", x = "") +
    theme_bw() +
    theme(legend.text=element_text(size=11)) +
    theme(axis.title.y = element_text(size=14)) +
    theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=12)) +
    theme(legend.title= element_blank()) +
    scale_x_continuous(limits = c(1988, year), breaks = seq(1990, year, 5))

#Faceted plot 
bcd %>%
  pivot_longer(2:4, names_to="Maturity", values_to="Perc_prev") %>%
  ggplot(aes(x = YEAR, y = Perc_prev)) +
  geom_point(aes(colour = Maturity), size=3) +
  geom_line(aes(colour = Maturity), size=1) +
  labs(y = "Disease Prevalence", x = "") +
  theme_bw() +
  theme(legend.text=element_text(size=11)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(axis.text.x=element_text(size=10), axis.text.y=element_text(size=12)) +
  theme(legend.title= element_blank()) +
  scale_x_continuous(limits = c(1988, year), breaks = seq(1990, year, 5)) +
  facet_wrap(~Maturity)

#Just immature
bcd %>%
  pivot_longer(2:4, names_to="Maturity", values_to="Perc_prev") %>%
  filter(Maturity=="Immature") %>%
  ggplot() +
  geom_point(aes(x = YEAR, y = Perc_prev), color="blue") +
  geom_line(aes(x = YEAR, y = Perc_prev), color="blue") +
  labs(y = "Disease Prevalence (%)", x = "") +
  theme_bw() +
  theme(axis.title.y = element_text(size=14)) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12)) +
  geom_hline(aes(yintercept = mean(Perc_prev, na.rm=TRUE)), linetype = 5)+
  scale_x_continuous(limits = c(1988, year), breaks = seq(1990, year, 5))
ggsave(path="./figs", "bcd_imm_prev.png")

