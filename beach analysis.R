#Beach Monitoring Program Analysis
#Detection weekly at 40 beach sites vs. 3x per summer lake monitoring
library(tidyverse)
library(lubridate)
setwd("C:/Users/grace/Box/Iowa_Mircocystin_withWalter")

beach = read_csv("iowa_mc_beachdata.csv") %>%
  group_by(year, name, latitude, longitude) %>%
  summarize(times_detect = sum(detect)) %>%
  ungroup() 
# write.csv(beach, file = 'beach_detect.csv')

lake = read_csv("Iowa Lakes Dataset 2000-2022.csv") %>%
  filter(year>=2017) %>%
  filter(analyte=='Microcystin') %>%
  group_by(year, name) %>%
  summarize(times_detect = sum(result>0.6)) %>%
  ungroup() %>%
  rename(lake_name = name,
         lake_detect = times_detect)
# write.csv(lake, file = 'lake_detect.csv')

#using the two output files, munged lake names offline
beach_lake = read_csv('beach_detect.csv') %>%
  select(-latitude, -longitude) %>%
  rename(lake_name = beach_lake) %>%
  
  # Come together, right now, over me(tadata)
  left_join(., lake, by = c('year', 'lake_name')) %>%
  
  # Drop the lake years when the lake wasn't sampled due to the pandemic
  drop_na(lake_detect) %>%
  
  # Let's get logical
  mutate(beach_logical = case_when(beach_detect>0 ~ 1, TRUE ~ 0),
         lake_logical = case_when(lake_detect>0 ~ -1, TRUE ~ 0),
         compare = beach_logical + lake_logical) %>%
  group_by(lake_name, year) %>%
  summarize(avg_compare = mean(compare)) %>%
  ungroup() %>%
  
  # Let's make some categories
  mutate(category = case_when(
    .$avg_compare == -1 ~ "Detect in Lake, not Beach",
    .$avg_compare == (-1/3) ~ 'Detected in Both',
    .$avg_compare == 0 ~ 'Detected in Both',
    .$avg_compare == 0.5 ~ 'Detected in Both',
    .$avg_compare == 0.75  ~ 'Detected in Both',
    .$avg_compare == 1 ~ "Detected in Beach, not Lake")) %>%
  filter(year<=2021)
  

barplot(c(112, 26, 1), names = c("Detected both\nBeach & Lake",
                                 "Detected at\nBeach, not Lake", 
                                 "Detected in\nLake, not Beach"),
        ylab = "Number of Lake Years", ylim = c(0,125),
        col = c("#44AA99","#6EA8C5","#DDCC77"))
abline(0,0)
text(0.7, 118, "81%")
text(1.95, 20, "11%") #transient bloom at beach
lines(c(1.4,2.4), c(16,16))
text(1.95, 30, "7%") #shouldn't have missed
text(3.15, 5, "1%")
