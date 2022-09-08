library(data.table)
library(tidyverse)

###########################################################
#   Mesure predator density or presence/absence by season
###########################################################

#-----------------------#
#   Snowly owl density  #
#-----------------------#

### Vérifier avec Gilles les données -> Avoir la bonne aire de recherche

owl_threshold <- 0.00015

owlDensity <- fread("data_raw/owl_nest_lemming_density_gauthier.csv")[,-9] %>%
  mutate(Nest_density_C1 = Owl_C1 / (C1_area_searched*100)) %>%
  select(year = Year, Owl_C1, Nest_density_C1) %>% 
  add_column(owl = ifelse(.$Nest_density_C1 > owl_threshold, 1, 0)) %>% 
  mutate(Season = "S", period = "P1")


#-----------------------------#
#   Arctic fox reproduction   #
#-----------------------------#

fox <- fread("data_raw/NordicanaD_arctic_and_red_fox_den_monitoring_data_1993-2019_BERTEAUX.txt", stringsAsFactors = T)

Den_ID_C1 <- paste0(rep("FOX", 99), sprintf("%03d", 1:99))

fox_threshold <- 0.1

fox_repro <- fox[which(fox$Den_ID %in% Den_ID_C1 & fox$Species != "Red fox"),] %>%
  filter(Year >= 2005) %>% # No lemming density data before 2005
  select(Year, Den_ID, Status) %>% 
  group_by(Year, Status) %>% 
  summarise(nstatus = length(Status)) %>% 
  pivot_wider(names_from = Status, values_from = nstatus) %>%
  replace(is.na(.), 0) %>% 
  mutate(sample_size = sum(NR, N, R)) %>% 
  mutate_at(vars(NR, N, R), funs(./sample_size)) %>% 
  select(Year, N, R, NR, sample_size) %>% 
  mutate(NR_sum = sum(N, R)) %>% 
  add_column(fox_repro = ifelse(.$NR_sum > fox_threshold, 1, 0)) %>% 
  select(Year, sample_size, NR_sum, fox_repro) %>% 
  mutate(Season = "S", period = "P1")


#----------------------------#
#   Long-tailed jaeger       #
#----------------------------#

### Vérifier avec Yannick et noter l'unité spatiale (50 ha ou km?). Vérifier aussi pour le seuil

jaeger_threshold <- 0.001

jaeger <- read.delim("data_raw/nordicanad_ltja_nesting_summary_data_2004_2019_gauthier.txt") %>% 
  filter(Sector == "CAMP 1") %>% 
  group_by(Year) %>% 
  summarise(nest_count = length(Year)) %>% 
  complete(Year = full_seq(Year, 1), fill = list(nest_count = 0)) %>% 
  mutate(nest_density = nest_count/5000) %>% 
  add_column(jaeger = ifelse(.$nest_density > jaeger_threshold, 1, 0)) %>% 
  mutate(Season = "S", period = "P1")

#-------------------------#
#   Weasel observations   #
#-------------------------#

weasel <- read.csv2("data_raw/ST_hermines_1991-2019.csv")[, -1] %>% 
  add_column(weasel = ifelse(.$Index >= 0.5, 1, 0)) %>% 
  mutate(Season = "S", period = "P1")

plot(Index ~ Year, weasel, type = "l", lwd = 2, col = "blue", ylim = c(0, 3))
points(Index ~ Year, weasel, pch = 19)
lines(min ~ Year, weasel)
lines(max ~ Year, weasel)

abline(h = 0.5, lty = 2)
abline(h = 1.0, lty = 2)
abline(h = 1.5, lty = 2)

# ###
# ### Summer observation
# ###
# 
# Ermine <- c("Ermine", "Ermine ")
# 
# WSummer_threshold <- 1.5
# 
# WeaselSummer <- read.csv2("data/NordicanaD daily wildlife observations 2007-2019_GAUTHIER.csv", sep = ",") %>%
#   filter(Sector == "CAMP 1", Observed_sp %in% Ermine) %>% 
#   group_by(Year) %>% 
#   summarise(obsEvent_count = length(Year)) %>% 
#   complete(Year = 2007:2019, fill = list(obsEvent_count = 0)) %>% 
#   add_column(weasel = ifelse(.$obsEvent_count > WSummer_threshold, 1, 0)) %>% 
#   mutate(Season = "S", period = "P1")
# 
# ###
# ### Winter observation
# ###
# 
# WWinter_threshold <- 0.03
# 
# WeaselWinter <- read.delim("data/lemming/nordicanad_lemming_winter_nests_data_2007_2019_gauthier.txt") %>% 
#   subset(., select = c(1, 9, 15))
# 
# ## Will only keep nest found during systematic search. Cannot separate nest found opportunistically at Camp 1 from other camps without Camp 1 gps polygon
# # Shorten transect and grid names to agregate
# AREA <- c("TH", "TR", "TM", "LG2", "LG1", "LX")
# 
# for(i in 1:(length(AREA))){
#   WeaselWinter[which(str_detect(WeaselWinter$Area_Name, AREA[i])), 2] <- AREA[i]
# }
# 
# WeaselWinter <- WeaselWinter[which(WeaselWinter$Area_Name %in% AREA),]
# 
# # Set predation to numeric
# for (i in 1:nrow(WeaselWinter)) {
#   if(WeaselWinter[i, 3] == "Unk") WeaselWinter[i, 3] <- "No"
# }
# 
# WeaselWinter %<>% group_by(Year, Predation) %>% 
#   summarise(nstatus = length(Predation)) %>% 
#   pivot_wider(names_from = Predation, values_from = nstatus) %>% 
#   replace(is.na(.), 0) %>% 
#   mutate(sample_size = sum(No, Yes, na.rm = T)) %>% 
#   mutate_at(vars(No, Yes), funs(./sample_size)) %>% 
#   add_column(weasel = ifelse(.$Yes > WWinter_threshold, 1, 0)) %>% 
#   mutate(Season = "W", period = "P3")
# 
# 
# WeaselWinter$Year <- WeaselWinter$Year - 1 # Assign winter nest predation at year before (Years start with summer)
# 
# # Regroup weasel data
# WeaselALL <- rbind(WeaselSummer[, c(1,3:5)], WeaselWinter[, c(1,5:7)])

#---------------------------------#
#   Presence-absence dataframe    #
#---------------------------------#

Predator <- data.frame(Season  = c(rep(c("S", "W"), 26), "S"),
                       period  = c(rep(c("P1", "P3"), 26), "P1"),
                       year    = c(rep(c(1993:2018), each = 2), 2019))

Predator %<>% left_join(owlDensity[, c(1,4:6)], by = c("Season", "year", "period")) %>% 
  left_join(fox_repro[, c(1, 4:6)], by = c("Season", "year" = "Year", "period")) %>% 
  left_join(jaeger[, c(1, 4:6)], by = c("Season", "year" = "Year", "period")) %>% 
  left_join(weasel[, c(1, 10:12)], by = c( "year" = "Year", "period","Season")) %>% 
  relocate(year, period, Season)

# Remove NA caused by season only (keep when there was no observations)
Predator[is.na(Predator$owl), "owl"] <- 0
Predator[seq(24,52, by = 2), "jaeger"] <- 0
Predator[seq(26,52, by = 2), "fox_repro"] <- 0
Predator[is.na(Predator$weasel), "weasel"] <- 0

saveRDS(Predator, "data_clean/5_PredatorPresence.rds")


#--------------------------#
#   Abundance dataframe    #
#--------------------------#

PredatorAbundance <- data.frame(Season  = c(rep(c("S", "W"), 26), "S"),
                                period  = c(rep(c("P1", "P3"), 26), "P1"),
                                year    = c(rep(c(1993:2018), each = 2), 2019))

PredatorAbundance %<>% left_join(owlDensity[, c(1,3,5,6)], by = c("Season", "year", "period")) %>% 
  left_join(fox_repro[, c(1,3,5,6)], by = c("Season", "year" = "Year", "period")) %>%
  left_join(jaeger[, c(1,3,5,6)], by = c("Season", "year" = "Year", "period")) %>%
  left_join(weasel[, c(1, 3, 11, 12)], by = c( "year" = "Year", "period", "Season")) %>% 
  relocate(year, period, Season) %>% 
  rename(owl = Nest_density_C1, jaeger = nest_density, fox_repro = NR_sum, weasel = Index)

# Remove NA caused by season only (keep when there was no observations)
PredatorAbundance[is.na(PredatorAbundance$owl), "owl"] <- 0
PredatorAbundance[seq(24,52, by = 2), "jaeger"] <- 0
PredatorAbundance[seq(26,52, by = 2), "fox_repro"] <- 0
PredatorAbundance[is.na(PredatorAbundance$weasel), "weasel"] <- 0

saveRDS(PredatorAbundance, "data_clean/5_PredatorAbundance.rds")


#-----------------------------#
#   Mean predator density     #
#-----------------------------#

PredDensity <- list(Owl = mean(owlDensity$Nest_density_C1[which(owlDensity$Nest_density_C1 > owl_threshold)]),
                    Fox = 0.0008,
                    Jaeger = mean(jaeger$nest_density[which(jaeger$nest_density > jaeger_threshold)]))

saveRDS(PredDensity, "data_clean/5_PredMeanDensity.rds")

