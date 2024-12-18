library(data.table)
library(tidyverse)

###########################################################
#   Mesure predator density or presence/absence by season
###########################################################

#-----------------------#
#   Snowy owl density  #
#-----------------------#
# Set a threshold of more than 1 owl nest
owl_threshold <- 0.00015

# Load and format owl data
owlDensity <- fread("data_raw/owl_nest_lemming_density_gauthier.csv")[,-9] %>%
  mutate(Nest_density_C1 = Owl_C1 / (C1_area_searched*100)) %>%
  select(year = Year, Owl_C1, Nest_density_C1) %>% 
  add_column(owl = ifelse(.$Nest_density_C1 > owl_threshold, 1, 0)) %>% 
  mutate(Season = "S", period = "P1")


#-----------------------------#
#   Arctic fox reproduction   #
#-----------------------------#
# Load fox data
fox <- fread("data_raw/NordicanaD_arctic_and_red_fox_den_monitoring_data_1993-2019_BERTEAUX.txt", stringsAsFactors = T)

# Create ID for den in main research area
Den_ID_C1 <- paste0(rep("FOX", 99), sprintf("%03d", 1:99))

# Set a threshold of at least 10% of reproductive den
fox_threshold <- 0.1

# Format fox data 
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
# Set a threshold of more than 1 jaeger nest
jaeger_threshold <- 0.001

# Load and format jaeger data
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
# Load weasel data, threshold presence at a Index of more than 0.5
weasel <- read.csv2("data_raw/ST_hermines_1991-2019.csv")[, -1] %>% 
  add_column(weasel = ifelse(.$Index >= 0.5, 1, 0)) %>% 
  mutate(Season = "S", period = "P1")


#---------------------------------#
#   Presence-absence dataframe    #
#---------------------------------#
# Build the predator presence dataset
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
# Buil the predator abundance dataframe 
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
# Measure the mean predator density when present in the summer
PredDensity <- list(Owl = mean(owlDensity$Nest_density_C1[which(owlDensity$Nest_density_C1 > owl_threshold)]),
                    Fox = 0.0008,
                    Jaeger = mean(jaeger$nest_density[which(jaeger$nest_density > jaeger_threshold)]))

saveRDS(PredDensity, "data_clean/5_PredMeanDensity.rds")
