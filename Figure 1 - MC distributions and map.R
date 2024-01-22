#Read in the site data
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggmap)
library(rgdal)
library(raster)
library(rgeos)

setwd("C:/Users/grace/Box/Iowa_Mircocystin_withWalter/")

lakes = read_csv("Lake_Characteristics.csv") %>%
  rename(name = Lake_Name)

chem = read_csv("Iowa Lakes Dataset 2000-2022.csv") %>%
  # make the sample date something we can work with
  mutate(date = mdy_hm(sampleDate),
         date = as.POSIXct(date),
         year = year(date)) %>%
  # filter to 2017-2021
  filter(year>=2017) %>%
  filter(year<2022) %>%
  # pivot from long to wide format and ditch unwanted columns
  pivot_wider(id_cols = c(name, year, date), 
              names_from = analyte, 
              values_from = result) %>%
  # Combine TP data with two different name
  # mutate(TP = case_when(is.na(.$`Phosphate-phosphorus (AS P)`) ~ 
  #                         `Total Phosphorus (AS P)`, 
  #                       TRUE ~ `Phosphate-phosphorus (AS P)`)) %>%
  # select(-`Phosphate-phosphorus (AS P)`, -`Total Phosphorus (AS P)`) %>%
  # Convert microcystin minimum to 2017 minimum of 0.6 ug/L
  mutate(Microcystin = if_else(Microcystin < 0.6, 0.6, Microcystin))
# Make year a factor for ggplotting
chem$year.factor = as.factor(chem$year)


# Palette of color blind friendly shades
pal <- c("#44AA99","#6EA8C5","#DDCC77","#CC6677", "#9E5B93")
col17 = rgb(68, 170, 153, max = 255, alpha = 150)
col18 = rgb(110, 168, 197, max = 255, alpha = 150)
col19 = rgb(221, 204, 119, max = 255, alpha = 150)
col20 = rgb(204, 102, 119, max = 255, alpha = 150)
col21 = rgb(158, 91, 147, max = 255, alpha = 150)

mc_summary = chem %>%
  group_by(name, year) %>%
  summarize(mc_mean = mean(Microcystin, na.rm = TRUE),
            spcond_mean = mean(`Specific conductance`),
            tds_mean = mean(`Total dissolved solids`))%>%
  ungroup()

mc17 = mc_summary %>% filter(year == 2017)
mc18 = mc_summary %>% filter(year == 2018)
mc19 = mc_summary %>% filter(year == 2019)
mc20 = mc_summary %>% filter(year == 2020)
mc21 = mc_summary %>% filter(year == 2021)

#====== RANK PLOT =======#
windows(height = 3.25, width = 3.25)
par(omi = c(0.4,0.1,0,0), mai = c(0.2,0.4,0.1,0.1))
plot(rank(mc17$mc_mean, ties.method = 'random'), log10(mc17$mc_mean), 
     pch = 19, cex = 0.8, col = col17, ylim = c(-0.3, 2), xlim = c(0,136), 
     yaxt = 'n', xaxt = 'n', xlab = "", ylab = "")

#Bespoke gridlines
abline(log10(1), 0, lty = 3, lwd = 0.7, col = "gray75")
abline(log10(10), 0, lty = 3, lwd = 0.7, col = "gray75")
abline(log10(100), 0, lty = 3, lwd = 0.7, col = "gray75")
lines(c(50, 50), c(-10,1000), lty = 3, lwd = 0.7, col = "gray75")
lines(c(100, 100), c(-10,1000), lty = 3, lwd = 0.7, col = "gray75")
lines(c(0, 0), c(-10,1000), lty = 3, lwd = 0.7, col = "gray75")

#Recreational action limit (total microcystin = 8 ug/L)
# abline(log10(8), 0, lty = 2, lwd = 2, col = "gray30")
# text(20, log10(5.5), "Recreation\nThreshold", cex = 0.8, font = 3)

#Arts and Crafts
legend("topleft", legend = c('2017', '2018', '2019', '2020', '2021'),
       col = pal, bg = "white",
       pch = 15, pt.cex = 1.5, inset = 0.02, cex = 0.8)
axis(side = 2, at = c(log10(0.5), log10(0.6), log10(0.7), log10(0.8), log10(0.9),
                      log10(1), log10(2), log10(3), log10(4), log10(5),
                      log10(6), log10(7), log10(8), log10(9), log10(10),
                      log10(20), log10(30), log10(40), log10(50), log10(60),
                      log10(70), log10(80), log10(90), log10(100)), 
     labels = c("", "", "", "", "", 
                "1", "", "", "", "", "", "", "", "", "10",
                "", "", "", "", "", "", "", "", "100"), 
     las = 2, cex.axis = 0.7, mgp = c(0,0.4,0), tcl = -0.3)
axis(side = 1, at = c(0, 25, 50, 75, 100, 125), cex.axis = 0.7, 
     mgp = c(0,0.4,0), tcl = -0.2)

#Percent of lakes with detectable microcystin by year
lines(c(53, 53), c(-10, log10(0.6)), col = col17, lwd = 2) #47.6% of lakes w/ detectable
lines(c(66, 66), c(-10, log10(0.6)), col = col18, lwd = 2) #47.6% of lakes w/ detectable
lines(c(52, 52), c(-10, log10(0.6)), col = col19, lwd = 2) #59.4% of lakes w/ detectable
lines(c(34, 34), c(-10, log10(0.6)), col = col20, lwd = 2) #55.8% of lakes w/ detectable
lines(c(82, 82), c(-10, log10(0.6)), col = col21, lwd = 2) #37.9% of lakes w/ detectable

#Add in the rest of the years of sampling and put 2017 back on top
points(rank(mc21$mc_mean, ties.method = 'random'), log10(mc21$mc_mean),
       pch = 19, cex = 0.8, col = col21)
points(rank(mc18$mc_mean, ties.method = 'random'), log10(mc18$mc_mean),
       pch = 19, cex = 0.8, col = col18)
points(rank(mc17$mc_mean, ties.method = 'random'), log10(mc17$mc_mean),
       pch = 19, cex = 0.8, col = col17)
points(rank(mc19$mc_mean, ties.method = 'random'), log10(mc19$mc_mean),
       pch = 19, cex = 0.8, col = col19)
points(rank(mc20$mc_mean, ties.method = 'random'), log10(mc20$mc_mean),
       pch = 19, cex = 0.8, col = col20)



# ====== MAPPING =======#
#Calculate mean microcystin in 2017 for mapping
maps17 = read_csv("Iowa Lakes Dataset 2000-2022.csv") %>%
  # make the sample date something we can work with
  mutate(date = mdy_hms(sampleDate),
         date = as.POSIXct(date),
         year = year(date)) %>%
  # filter to 2017-2021
  filter(year==2017) %>%
  filter(analyte=="Microcystin") %>%
  group_by(siteID) %>%
  summarize(mc = mean(result, na.rm = TRUE),
            latitude = mean(latitude, na.rm = TRUE),
            longitude = mean(longitude, na.rm = TRUE)) %>%
  ungroup()

# Loading ecoregion map data
ecoreg.map <- readOGR("./reg7_eco_l4/reg7_eco_l4.shp")
ecoreg.map <- ecoreg.map[ecoreg.map$STATE_NAME=="Iowa",]
ecoreg.map$US_L4CODE[ecoreg.map$US_L4CODE=="52b"] <- "52c"
# Re-project to decimal degrees like a baddie
reproject_ecoreg <- spTransform(ecoreg.map, crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# Hey R, these columns are spatial data, ya heard?
coordinates(mapping17) <- c("longitude", "latitude")

#some amazing color ramps: https://carto.com/carto-colors/ 
ramp_emrld <- colorRampPalette(c('#d3f2a3','#97e196','#6cc08b','#4c9b82','#217a79','#105965','#074050'))
ramp_darkmint <- colorRampPalette(c('#d2fbd4','#a5dbc2',
                                    '#7bbcb0','#559c9e',
                                    '#3a7c89','#235d72','#123f5a'))

#Lake Locations - plotting the map
windows(height = 3.25, width = 3.25)
par(omi = c(0.1,0,0,0), mai = c(0.1,0.1,0.1,0.1))
plot(reproject_ecoreg, col = ramp_emrld(12))
points(mapping17$longitude, mapping17$latitude, 
       pch = 21, cex = 1,
       col = 'black',
       bg = rgb(255, 255, 255, max = 255, alpha = 150))

#SPECIFIC CONDUCTIVITY - plotting the map
windows(height = 4, width = 4)
par(omi = c(0,0,0.3,0), mai = c(0,0,0,0))
plot(reproject_ecoreg, col = ramp_emrld(12))
points(mapping17$longitude, mapping17$latitude, 
       pch = 21, 
       cex = mc17$tds_mean/median(mc17$tds_mean),
       col = 'black',
       bg = rgb(255, 255, 255, max = 255, alpha = 150))
legend('top', legend = c('200', '400', '600', '800'), 
       pch = 21, pt.cex = c(0.566, 1.13, 1.697, 2.26),
       col = 'black', bg = rgb(255, 255, 255, max = 255, alpha = 150),
       bty = 'n', horiz = TRUE, cex = 0.8, x.intersp = 1.5, xpd = TRUE)
mtext(side = 3, line = 0,'Total Dissolved Solids', font = 2)


combo = read_csv("Analysis_Data_06292023.csv") %>%
  filter(year==2017) %>%
  group_by(Lake_Name) %>%
  summarize(ecoregion = Ecoregion,
            tds = mean(`Total dissolved solids`)) %>%
  ungroup()

boxplot(combo$tds ~ combo$ecoregion)


#==================================
# Climate Figure
pal <- c("#44AA99","#6EA8C5","#DDCC77","#CC6677", "#9E5B93")
col17 = rgb(68, 170, 153, max = 255, alpha = 250)
col18 = rgb(110, 168, 197, max = 255, alpha = 250)
col19 = rgb(221, 204, 119, max = 255, alpha = 250)
col20 = rgb(204, 102, 119, max = 255, alpha = 250)
col21 = rgb(158, 91, 147, max = 255, alpha = 250)

ppt = read.csv("PRISM_StoryCounty_2017-2021.csv")

plot(c(1:12), cumsum(ppt[ppt$waterYear==2017, 'ppt_mm']), 
     type = 'l', col = col17, lwd = 4,
     xlim = c(1,14), ylim = c(0, 1400),
     xaxt = 'n', xlab = 'Month', ylab = "Cumulative Preciptiation (mm)")
axis(side = 1, at = c(2, 4, 6, 8, 10, 12), 
     labels = c("Nov", "Jan", "Mar", "May", "July", "Sept"))
# Sampling period
polygon(c(8.5, 12, 12, 8.5), c(0,0,1400,1400), 
        col = 'gray85', border = NA)
text(10.25, 1300, "Sampling\nPeriod")
# Drawing each sampling year's precip
lines(c(1:12), cumsum(ppt[ppt$waterYear==2017, 'ppt_mm']), 
      type = 'l', col = col17, lwd = 4)
lines(c(1:12), cumsum(ppt[ppt$waterYear==2018, 'ppt_mm']), 
      type = 'l', col = col18, lwd = 4)
lines(c(1:12), cumsum(ppt[ppt$waterYear==2019, 'ppt_mm']), 
      type = 'l', col = col19, lwd = 4)
lines(c(1:12), cumsum(ppt[ppt$waterYear==2020, 'ppt_mm']), 
      type = 'l', col = col20, lwd = 4)
lines(c(1:12), cumsum(ppt[ppt$waterYear==2021, 'ppt_mm']), 
      type = 'l', col = col21, lwd = 4)
#Labeling the Sampling Years
text(13, 727, "2017", col = col17, font = 2)
text(13, 1309, "2018", col = col18, font = 2)
text(13, 1029, "2019", col = col19, font = 2)
text(13, 1029, "2019", col = col19, font = 2)
text(13, 800, "2020", col = col20, font = 2)
text(13, 550, "2021", col = col21, font = 2)
