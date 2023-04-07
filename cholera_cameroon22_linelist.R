######################################
# CHOLERA OUTBREAK, CAMEROON, 2022   #
# OUTBREAK DESCRIPTION               #
######################################
# last update Brecht Ingelbeen 08 March 2022: create script
#### install and load packages ####
# install.packages("pacman")
pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,ggmap,osmdata,scales,zoo,reshape2,tidyr,stringr,wesanderson,tidyr, knitr, epitools, naniar, survey, ggpattern)

### import linelist ####
ll <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Cholera/Cameroon 2022 outbreak/linelist.csv", sep=";")

# clean data
# age
ll$agenum <- as.numeric(ll$Age)
ll$agenum[is.na(ll$agenum)] <- as.numeric(gsub("y", "", ll$Age[is.na(ll$agenum)]))
ll$agenum[is.na(ll$agenum)] <- as.numeric(gsub("+", "", ll$Age[is.na(ll$agenum)]))
ll$agenum[is.na(ll$agenum)] <- floor(as.numeric(gsub("MTHS", "", ll$Age[is.na(ll$agenum)])) / 12)
ll$agenum[is.na(ll$agenum)] <- floor(as.numeric(gsub(" MTHS", "", ll$Age[is.na(ll$agenum)])) / 12)
ll$agenum[is.na(ll$agenum)] <- floor(as.numeric(gsub("m", "", ll$Age[is.na(ll$agenum)])) / 12)
ll$agenum[grepl("1YR", ll$Age)==T&is.na(ll$agenum)] <- 1
ll$agenum[grepl("4YR", ll$Age)==T&is.na(ll$agenum)] <- 4
ll$agenum[ll$Age=="50+"] <- 50
ll$agenum[ll$Age=="82+"] <- 82
ll$agenum[ll$Age=="2+"] <- 2
ll$agenum[ll$Age=="60+"] <- 60
ll$agenum[ll$Age=="70+"] <- 70
ll$agenum[ll$Age=="89+"] <- 89
ll$agenum[ll$Age=="1y6m"] <- 1
table(ll$Age[is.na(ll$agenum)])
table(ll$agenum, useNA = "always")
hist(ll$agenum)
# agegroups - none over 110 yo
ll$agegr <- cut(ll$agenum, breaks = seq(0, 100, 10), labels = paste0(seq(0,90,10), "-", seq(10, 100, 10)-1))
table(ll$agegr)

# sex
ll$Sex[ll$Sex=="m"] <- "M" 
table(ll$Sex)
# date onset
ll$dstart <- as.Date(ll$Date.of.onset.of.symptoms, format = "%d/%m/%Y")

# health district
ll$healthdistrict <- tolower(ll$Health.district.notifying)
ll$healthdistrict_simpl <- ll$healthdistrict
ll$healthdistrict_simpl[ll$healthdistrict=="kumba south"] <- "kumba"
ll$healthdistrict_simpl[ll$healthdistrict=="kumba north"] <- "kumba"

# variable risk: water
table(ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement....., useNA = "always")
ll$water <- NA
ll$water[ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....!=""] <- "no"
ll$water[ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....=="Death on arrival"] <- NA
ll$water[grepl("1",ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....)==T] <- "yes"
ll$water[grepl("Water",ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....)==T] <- "yes"
table(ll$water, useNA = "always")
# variable risk: sanitation
ll$sanitation <- NA
ll$sanitation[ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....!=""] <- "no"
ll$sanitation[ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....=="Death on arrival"] <- NA
ll$sanitation[grepl("2",ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....)==T] <- "yes"
table(ll$sanitation, useNA = "always")
# variable risk: hygiene
ll$hygiene <- NA
ll$hygiene[ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....!=""] <- "no"
ll$hygiene[ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....=="Death on arrival"] <- NA
ll$hygiene[grepl("3",ll$Observations..Facteur.de.risque...1.eau..2.assainissement..3..hygiÃ.ne..4..autres.saison..surpeuplement..dÃ.placement.....)==T] <- "yes"
table(ll$hygiene, useNA = "always")

# profession
table(ll$Profession)
ll$Profession <- tolower(ll$Profession)
ll$occupation[ll$Profession!=""] <- "other"
ll$occupation[grepl("trad",ll$Profession)==T] <- "trade"
ll$occupation[grepl("busine",ll$Profession)==T] <- "trade"
ll$occupation[grepl("buisne",ll$Profession)==T] <- "trade"
ll$occupation[grepl("unempl",ll$Profession)==T] <- "unemployed"
ll$occupation[grepl("driv",ll$Profession)==T] <- "transportation"
ll$occupation[grepl("timb",ll$Profession)==T] <- "farming-forestry"
ll$occupation[grepl("forestr",ll$Profession)==T] <- "farming-forestry"
ll$occupation[grepl("agric",ll$Profession)==T] <- "farming-forestry"
ll$occupation[grepl("mechani",ll$Profession)==T] <- "technical"
ll$occupation[grepl("fish",ll$Profession)==T] <- "fishing"
ll$occupation[grepl("farm",ll$Profession)==T] <- "farming-forestry"
ll$occupation[grepl("poultry",ll$Profession)==T] <- "farming-forestry"
ll$occupation[grepl("techni",ll$Profession)==T] <- "technical"
ll$occupation[grepl("plumb",ll$Profession)==T] <- "technical"
ll$occupation[grepl("stude",ll$Profession)==T] <- "student"
ll$occupation[grepl("soldie",ll$Profession)==T] <- "military"
ll$occupation[grepl("millit",ll$Profession)==T] <- "military"
ll$occupation[grepl("milit",ll$Profession)==T] <- "military"
ll$occupation[grepl("marine",ll$Profession)==T] <- "military"
ll$occupation[grepl("retired",ll$Profession)==T] <- "retired"
ll$occupation[grepl("elder",ll$Profession)==T] <- "retired"
ll$occupation[grepl("pharmac",ll$Profession)==T] <- "HCW"
ll$occupation[grepl("nurs",ll$Profession)==T] <- "HCW"
ll$occupation[grepl("pharcy",ll$Profession)==T] <- "HCW"
ll$occupation[grepl("medica",ll$Profession)==T] <- "HCW"
ll$occupation[grepl("healthcare",ll$Profession)==T] <- "HCW"
ll$occupation[grepl("docto",ll$Profession)==T] <- "HCW"
ll$occupation[grepl("cook",ll$Profession)==T] <- "cook"
ll$occupation[grepl("cater",ll$Profession)==T] <- "cook"
ll$occupation[grepl("carter",ll$Profession)==T] <- "cook"
ll$occupation[grepl("chief",ll$Profession)==T] <- "cook"
ll$occupation[grepl("child",ll$Profession)==T] <- "child"
ll$occupation[grepl("neonat",ll$Profession)==T] <- "child"
ll$occupation[grepl("pupil",ll$Profession)==T] <- "child"
table(ll$occupation, useNA = "always")

# create variables Latitude and Longitude
ll <- separate(ll, col = "Latitude.Longitude", into = c("Latitude", "Longitude"), sep = ",")
ll$Latitude <- as.numeric(ll$Latitude)
ll$Longitude <- as.numeric(ll$Longitude)

# clean health area variable
ll$area <- tolower(ll$Health.area.of.origin)
ll$area[ll$Health.area.of.origin==""] <- tolower(ll$Health.area.notifying)
ll$area[ll$area=="buea-road"] <- "buea road"
ll$area[grepl("zone",ll$area)==T] <- "zone ii"
ll$area[grepl("zon 2",ll$area)==T] <- "zone ii"

table(ll$area, useNA = "always")
# rename areas that have errors compared to the names on the official list
ll$area[ll$area=="banga  bakunda"] <- "banga bakundu"
ll$area[ll$area=="cma limbe"] <- "sea port" # limbe is no health district, according to google maps the limbe CMA is in sea port

### epicurve by agegroup, locality, and occupation ####
# filter out outlier cases - probably a separate outbreak or imported cases
ll <- ll %>% filter(ll$dstart>"2021-07-01" & ll$dstart<"2022-09-01")

# 1. AGEGROUP
# summarize case counts per day by agegroup
dailycount <- ll %>%
  mutate(dstart = as.Date(dstart)) %>%
  count(agegr, dstart) %>%
  complete(agegr, dstart = seq(min(dstart), max(dstart), by = "day"), fill = list(n = 0))

# create histogram of daily counts stacked by age group
economist_colors <- c("#1A508B", "#A2C8EC", "orange","#C1292E", "yellow", "#ED7B84", "#0D2A3B", "#88B646", "purple", "darkgreen", "pink", "#D0A3A6", "#F1C75B", "#F2EDD7")
epicurve_agegroup <- ggplot(dailycount, aes(x = dstart, y = n, fill = factor(agegr))) +
  geom_col() +
  theme_bw() +
  labs(x = "Date of symptom onset", y = "Daily number of cases") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Age group", values = economist_colors)
epicurve_agegroup  
ggsave("epicurve_agegroup.jpg", epicurve_agegroup, dpi = 400, width = 6, height = 4)

# 2. HEALTH DISTRICT FACET OF STACKED AGE GROUPS
table(ll$REGION) # all in SW
table(ll$healthdistrict, useNA = "always") # all in SW

# first 10 cases # 
table(ll$occupation[ll$dstart<"2021-10-30"])

# summarize case counts per day by region
dailycountagebydistrict <- ll %>%
  filter(healthdistrict %in% c("bakassi", "buea", "ekondo titi", "limbe", "tiko")) %>%
  mutate(dstart = as.Date(dstart)) %>%
  count(healthdistrict, agegr, dstart) %>%
  complete(healthdistrict, dstart = seq(min(dstart), max(dstart), by = "day"), fill = list(n = 0))

# create histogram of daily counts stacked by age group and facetted by region
economist_colors <- c("#1A508B", "#A2C8EC", "orange","#C1292E", "yellow", "#ED7B84", "#0D2A3B", "#88B646", "purple", "darkgreen", "pink", "#D0A3A6", "#F1C75B", "#F2EDD7")
epicurve_age_bydistrict <- ggplot(dailycountagebydistrict, aes(x = dstart, y = n, fill = factor(agegr))) +
  geom_col() +
  facet_wrap(~ healthdistrict, scales = "free_y", ncol = 1) +
  theme_bw() +
  labs(x = "Date of symptom onset", y = "Daily number of cases") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Agegroup", values = economist_colors)
epicurve_age_bydistrict  
ggsave("epicurve_age_bydistrict.jpg", epicurve_age_bydistrict, dpi = 400, width = 6, height = 10)

# 3. RISK FACTORS
# 3.1. OCCUPATION
# summarize case counts per occupation by healthdistrict - removing health districts with little cases by region
dailycountbydistrict_occupation <- ll %>%
  filter(healthdistrict %in% c("bakassi", "buea", "ekondo titi", "limbe", "tiko")) %>%
  mutate(dstart = as.Date(dstart)) %>%
  count(healthdistrict, occupation, dstart) %>%
  complete(healthdistrict, dstart = seq(min(dstart), max(dstart), by = "day"), fill = list(n = 0))

# create histogram of daily counts stacked by region
economist_colors <- c("#1A508B", "#A2C8EC", "orange","#C1292E", "yellow", "#ED7B84", "grey","#88B646", "purple", "#D0A3A6",  "darkgreen", "#0D2A3B", "#F1C75B", "#F2EDD7")
epicurvebydistrict_occupation <- ggplot(dailycountbydistrict_occupation, aes(x = dstart, y = n, fill = factor(occupation))) +
  geom_col() +
  facet_wrap(~ healthdistrict, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(x = "Date of symptom onset", y = "Daily number of cases") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Occup", values = economist_colors)
epicurvebydistrict_occupation  
ggsave("epicurvebydistrict_occupation.jpg", epicurvebydistrict_occupation, dpi = 400, width = 6, height = 10)

### spread -> dotted map with distribution ####
# first 10 cases # 
table(ll$occupation[ll$dstart<"2021-10-30"])

# Set the map extent
my_bbox <- c(left = 8.4, bottom = 3.9, right = 9.6, top = 5.1)
# Get the map using the Stamen source
south_west_cameroon <- get_stamenmap(bbox = my_bbox, maptype = "toner-lite")

# summarise the number of cases by date by location
casecountbylocationandbystart <- ll %>%
  group_by(Latitude, Longitude) %>%
  summarize(dstartoutbreak = min(dstart), n=n()) 
casecountbylocationandbystart

arealocation <- ll %>%
 group_by(area) %>%
 summarize(meanlat=mean(Latitude), meanlon=mean(Longitude)) 
arealocation

districtlocation <- ll %>%
  group_by(healthdistrict_simpl) %>%
  summarize(meanlat=mean(Latitude), meanlon=mean(Longitude)) 
districtlocation

# Create a ggplot object and add a point layer using the Latitude.Longitude variable
dottedmap <- ggmap(south_west_cameroon) +
  geom_point(data = casecountbylocationandbystart, aes(x = Longitude, y = Latitude, color = dstartoutbreak, size = n)) +
  geom_text(data = districtlocation, aes(x = meanlon, y = meanlat,  
               label = healthdistrict_simpl), size = 4, nudge_x = 0.03, nudge_y = 0.03) +
  labs(size = "n", color = "dstartoutbreak") +
  scale_color_date(date_labels = "%m/%d/%Y", name = "dstartoutbreak", low = "red", high = "blue") 
dottedmap
ggsave("dottedmap.jpg", dottedmap, dpi = 300, width = 8, height = 8)

### attack rates by area ####
# import population data
pop <- read_excel("Populations Cibles 2021_Bon_Diffuser.xlsx")
pop$area <- tolower(pop$`Aire de santé`)
pop$healthdistrict <- tolower(pop$District)
pop$pop <- tolower(pop$`Population 2021 estimée (Les deux sexe)`)
pop <- pop %>% select(healthdistrict, area, pop)
table(pop$area)
# replace underscores by space
pop$area <- gsub("_", " ", pop$area)
# summarize n cases by area
casesperarea <- ll %>%
  group_by(area) %>%
  summarise(n = n())
casesperarea
# merge with pop data
attackratebyarea_matched <- merge(casesperarea, pop, by = "area", all.x = T)
attackratebyarea <- merge(casesperarea, pop, by = "area", all = T)
# no matches
attackratebyarea_matched$area[is.na(attackratebyarea_matched$pop)]
table(ll$area) # most still don't match. what's the issue here?

### description of case characteristics ####
# 1. age
round(prop.table(table(ll$agegr))*100,2)
# 2. sex
table(ll$Sex)
prop.table(table(ll$Sex))

