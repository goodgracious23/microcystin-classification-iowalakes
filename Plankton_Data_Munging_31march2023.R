#Zooplankton and Phytoplankton Munging
library(tidyverse)
setwd("C:/Users/grace/Box/Iowa_Mircocystin_withWalter/Old Analyses")

#2017 Zooplankton ===================================================
zoop17 = read.csv("2017 Zooplankton.csv") %>%
  rename(LakeID = "Lake.ID",
         SampleID = "Sample.ID")

daphnia17 = zoop17 %>%
  group_by(Genus, LakeID) %>%
  summarize(Daphnia = mean(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  filter(Genus=="Daphnia") %>%
  select(-Genus)

division17 = zoop17 %>%
  group_by(Division, LakeID, SampleID) %>%
  summarize(sum_biomass = sum(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  select(-SampleID) %>%
  group_by(Division, LakeID) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(mean_biomass)) %>%
  pivot_wider(id_cols = LakeID,
              names_from = Division,
              values_from = mean_biomass) %>% replace(is.na(.), 0)

meanZoops17 = left_join(division17, daphnia17, by = "LakeID") %>% replace(is.na(.), 0)

total17 = zoop17 %>%
  group_by(LakeID, SampleID) %>%
  summarize(sum_biomass = sum(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  group_by(LakeID) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanZoops17_FINAL = left_join(meanZoops17, total17, by = "LakeID") %>% replace(is.na(.), 0)
# write.csv(meanZoops17_FINAL, file = "2017 mean zooplankton.csv")

#2017 PHYTOPLANKTON
phyto17 = read.csv("2017 Phytoplankton.csv")
microcystis17 = phyto17 %>%
  group_by(Genus, Lake.ID) %>%
  summarize(Microcystis = mean(Phytoplankton.Biomass..mg.L., na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Genus=="Microcystis") %>%
  select(-Genus)

divisionPhyto17 = phyto17 %>%
  group_by(Division, Lake.ID, Sample.ID) %>%
  summarize(sum_biomass = sum(Phytoplankton.Biomass..mg.L.)) %>%
  ungroup() %>%
  select(-Sample.ID) %>%
  group_by(Division, Lake.ID) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = Lake.ID, names_from = Division, values_from = mean_biomass)

meanPhytos17 = left_join(divisionPhyto17, microcystis17, by = "Lake.ID") %>% 
  replace(is.na(.), 0)

totalPhyto17 = phyto17 %>%
  group_by(Lake.ID, Sample.ID) %>%
  summarize(sum_biomass = sum(Phytoplankton.Biomass..mg.L.)) %>%
  ungroup() %>%
  group_by(Lake.ID) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanPhytos17_FINAL = left_join(meanPhytos17, totalPhyto17, by = "Lake.ID") %>% 
  replace(is.na(.), 0)
# write.csv(meanPhytos17_FINAL, file = "2017 mean phytoplankton.csv")



#2018 Zooplankton ====================================
zoop18 = read.csv("2018 Zooplankton.csv") %>%
  rename(LakeID = "Lake.ID",
         SampleID = "Sample.ID")

daphnia18 = zoop18 %>%
  group_by(Genus, LakeID) %>%
  summarize(Daphnia = mean(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  filter(Genus=="Daphnia") %>%
  select(-Genus)

division18 = zoop18 %>%
  group_by(Division, LakeID, SampleID) %>%
  summarize(sum_biomass = sum(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  select(-SampleID) %>%
  group_by(Division, LakeID) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(mean_biomass)) %>%
  pivot_wider(id_cols = LakeID,
              names_from = Division,
              values_from = mean_biomass) %>% replace(is.na(.), 0)

meanZoops18 = left_join(division18, daphnia18, by = "LakeID") %>% replace(is.na(.), 0)

total = zoop18 %>%
  group_by(LakeID, SampleID) %>%
  summarize(sum_biomass = sum(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  group_by(LakeID) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanZoops18_FINAL = left_join(meanZoops18, total, by = "LakeID") %>% replace(is.na(.), 0)
# write.csv(meanZoops18_FINAL, file = "2018 mean zooplankton.csv")


#2018 PHYTOPLANKTON
phyto18 = read.csv("2018 Phytoplankton.csv")
microcystis18 = phyto18 %>%
  group_by(Genus, Lake.ID) %>%
  summarize(Microcystis = mean(Phytoplankton.Biomass..mg.L., na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Genus=="Microcystis") %>%
  select(-Genus)

divisionPhyto18 = phyto18 %>%
  group_by(Division, Lake.ID, Sample.ID) %>%
  summarize(sum_biomass = sum(Phytoplankton.Biomass..mg.L.)) %>%
  ungroup() %>%
  select(-Sample.ID) %>%
  group_by(Division, Lake.ID) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = Lake.ID, names_from = Division, values_from = mean_biomass)

meanPhytos18 = left_join(divisionPhyto18, microcystis18, by = "Lake.ID") %>% replace(is.na(.), 0)

totalPhyto18 = phyto18 %>%
  group_by(Lake.ID, Sample.ID) %>%
  summarize(sum_biomass = sum(Phytoplankton.Biomass..mg.L.)) %>%
  ungroup() %>%
  group_by(Lake.ID) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanPhytos18_FINAL = left_join(meanPhytos18, totalPhyto18, by = "Lake.ID") %>% 
  replace(is.na(.), 0)
# write.csv(meanPhytos18_FINAL, file = "2018 mean phytoplankton.csv")



#2019 Zooplankton ===================================
zoop19 = read.csv("2019 ALM Zooplankton.csv")
daphnia19 = zoop19 %>%
  group_by(Genus, Lake.ID) %>%
  summarize(Daphnia = mean(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  filter(Genus=="Daphnia") %>%
  select(-Genus)

division = zoop19 %>%
  group_by(Division, Lake.ID, Sample.ID) %>%
  summarize(sum_biomass = sum(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  select(-Sample.ID) %>%
  group_by(Division, Lake.ID) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = Lake.ID, names_from = Division, values_from = mean_biomass) %>%
  select(-'NA')

meanZoops19 = left_join(daphnia19, division, by = "Lake.ID")

total = zoop19 %>%
  group_by(Lake.ID, Sample.ID) %>%
  summarize(sum_biomass = sum(Zooplankton_Biomass..ug.L.)) %>%
  ungroup() %>%
  group_by(Lake.ID) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanZoops19_FINAL = left_join(meanZoops19, total, by = "Lake.ID") %>% replace(is.na(.), 0)
# write.csv(meanZoops19_FINAL, file = "2019 mean zooplankton.csv")


#2019 PHYTOPLANKTON
phyto19 = read.csv("Phytoplankton 2019 ALM.csv")
microcystis19 = phyto19 %>%
  group_by(Genus, Lake.Number) %>%
  summarize(Microcystis = mean(Biomass..mg.L., na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Genus=="Microcystis") %>%
  select(-Genus)

divisionPhyto = phyto19 %>%
  group_by(Division, Lake.Number, Sample.ID) %>%
  summarize(sum_biomass = sum(Biomass..mg.L.)) %>%
  ungroup() %>%
  select(-Sample.ID) %>%
  group_by(Division, Lake.Number) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = Lake.Number, names_from = Division, values_from = mean_biomass)

meanPhytos19 = left_join(divisionPhyto, microcystis19, by = "Lake.Number") %>% replace(is.na(.), 0)

totalPhyto = phyto19 %>%
  group_by(Lake.Number, Sample.ID) %>%
  summarize(sum_biomass = sum(Biomass..mg.L.)) %>%
  ungroup() %>%
  group_by(Lake.Number) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanPhytos19_FINAL = left_join(meanPhytos19, totalPhyto, by = "Lake.Number") %>% replace(is.na(.), 0)
# write.csv(meanPhytos19_FINAL, file = "2019 mean phytoplankton.csv")


#2020 Zooplankton =====================================
zoop20 = read.csv("2020 Zooplankton Report.csv") %>%
  rename(LakeID = "Lake.ID",
         SampleID = "Sample.ID")

daphnia20 = zoop20 %>%
  group_by(Genus, LakeID) %>%
  summarize(Daphnia = mean(Biomass..mg.L.)*1000) %>%
  ungroup() %>%
  filter(Genus=="Daphnia") %>%
  select(-Genus)

division20 = zoop20 %>%
  group_by(Division, LakeID, SampleID) %>%
  summarize(sum_biomass = sum(Biomass..mg.L.)*1000) %>%
  ungroup() %>%
  select(-SampleID) %>%
  group_by(Division, LakeID) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(mean_biomass)) %>%
  pivot_wider(id_cols = LakeID,
              names_from = Division,
              values_from = mean_biomass) %>% replace(is.na(.), 0)

meanZoops20 = left_join(division20, daphnia20, by = "LakeID") %>% replace(is.na(.), 0)

total20 = zoop20 %>%
  group_by(LakeID, SampleID) %>%
  summarize(sum_biomass = sum(Biomass..mg.L.)*1000) %>%
  ungroup() %>%
  group_by(LakeID) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanZoops20_FINAL = left_join(meanZoops20, total20, by = "LakeID") %>% replace(is.na(.), 0)
# write.csv(meanZoops20_FINAL, file = "2020 mean zooplankton.csv")

#2020 PHYTOPLANKTON
phyto20 = read.csv("2020 Phytoplankton Report.csv")
microcystis20 = phyto20 %>%
  group_by(Genus, Lake.Number) %>%
  summarize(Microcystis = mean(Biomass..mg.L., na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Genus=="Microcystis") %>%
  select(-Genus)

divisionPhyto = phyto20 %>%
  group_by(Division, Lake.Number, Sample.ID) %>%
  summarize(sum_biomass = sum(Biomass..mg.L.)) %>%
  ungroup() %>%
  select(-Sample.ID) %>%
  group_by(Division, Lake.Number) %>%
  summarize(mean_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = Lake.Number, names_from = Division, values_from = mean_biomass)

meanPhytos20 = left_join(divisionPhyto, microcystis20, by = "Lake.Number") %>% replace(is.na(.), 0)

totalPhyto = phyto20 %>%
  group_by(Lake.Number, Sample.ID) %>%
  summarize(sum_biomass = sum(Biomass..mg.L.)) %>%
  ungroup() %>%
  group_by(Lake.Number) %>%
  summarize(Total_biomass = mean(sum_biomass, na.rm = TRUE)) %>%
  ungroup()

meanPhytos20_FINAL = left_join(meanPhytos20, totalPhyto, by = "Lake.Number") %>% replace(is.na(.), 0)

# write.csv(meanPhytos20_FINAL, file = "2020 mean phytoplankton.csv")
