library(data.table)
library(tidyverse)

# Create data on lemming mass and density time series

# ----------------------------#
#   Lemming adult mean mass   #
#-----------------------------#

# Load lemming data
lemming_data <- fread("data_raw/nordicanad_lemming_live_trapping_data_2004_2019_gauthier.txt") %>% 
  filter(str_detect(Site_Name, c("LG1|LG2")))

# remove uppercase in species name
lemming_data$Species <- tolower(lemming_data$Species)

# remove lemming sp. and unknown
lemming_data <- subset(lemming_data, Species %in% c("brown lemming", "collared lemming"))

# Get lemming mean mass to use in biomass conversion
lem_mass <- signif(mean(lemming_data$Mass, na.rm = TRUE), 4)

saveRDS(lem_mass, "data_clean/2_1_lemmingMass.rds") 


#-------------------------------------------------   -----#
#   Aggregate lemming density by year, period and grids   #
#-------------------------                            ----#

# Pick and format reconstructed lemming densities from the snap trap time series
reconstructed <- read.csv2("data_raw/Densites_1993-2019.csv", sep = ",") %>% 
  filter(Year >= 1993 & Year < 2004) %>% 
  rename(year = Year, LG1 = Both.Wet.habitat, LG2 = Both.Mesic.habitat) %>% 
  select(year, LG1, LG2) %>% 
  pivot_longer(cols = c("LG1", "LG2"), names_to = "grid", values_to = "density") %>% 
  add_column(period = "P1", se = 0, cumulmonth = 0) %>% 
  mutate(cumulyear = year - min(.$year)) %>%
  mutate(biomass = density * lem_mass) %>% 
  select(year, period, grid, density, se, cumulyear, cumulmonth, biomass)

# Get starting year of snap trap time series
min.year <- min(reconstructed$year)

# Lemming time series in the wet grid (LG1)
LG1 <- read.csv2("data_raw/densite_brun_variable_2004_2019.csv", sep = ",", dec = ".") %>% 
  filter(grid == "LG1") %>% 
  select(year, period, grid, density, se) %>% 
  group_by(year, period, grid) %>% 
  summarise(density = sum(density), se = sum(se)) %>%  # Sum both lemming denisty
  filter(period %in% c("P1", "P2", "P3")) %>% 
  mutate(cumulyear = year - min.year) %>% 
  add_column(cumulmonth = 0) %>% 
  mutate(density = density + 0.025) %>% 
  mutate(biomass = density * lem_mass) # Transform density in biomass

# Lemming time series in the mesic grid (LG2)
LG2 <- read.csv2("data_raw/densite_brun_variable_2004_2019.csv", sep = ",", dec = ".") %>% 
  filter(grid == "LG2") %>% 
  select(year, period, grid, density, se) %>% 
  group_by(year, period, grid) %>% 
  summarise(density = sum(density), se = sum(se)) %>% 
  filter(period %in% c("P1", "P2", "P3")) %>% 
  mutate(cumulyear = year - min.year) %>% 
  add_column( cumulmonth = 0) %>% 
  mutate(density = density + 0.025) %>% 
  mutate(biomass = density * lem_mass)

# min se applied where no se
min.se <- min(min(LG1[which(LG1$se > 0), "se"]), min(LG2[which(LG2$se > 0), "se"])) / 2

LG1[which(LG1$se == 0), "se"] <- min.se
LG2[which(LG2$se == 0), "se"] <- min.se


# Function to add the month and years where missing in the dataset
cumultime <- function(x){
  # Add cumulmonth
  
  for (i in 1:nrow(x)) {
    if (x[i, "period"] == "P1"){ 
      x[i, "cumulmonth"] <- 6 + x[i, "cumulyear"]*12
      
    } else if (x[i, 2] == "P2") { 
      x[i, "cumulmonth"] <- 7 + x[i, "cumulyear"]*12
      
    } else if (x[i, 2] == "P3") { 
      x[i, "cumulmonth"] <- 8 + x[i, "cumulyear"]*12
    }
  }
  
  return(x)
}

LG1 <- cumultime(LG1)
LG2 <- cumultime(LG2)
reconstructed <- cumultime(reconstructed)


#---------------------------#
#   Reconstructed density   #
#---------------------------#

# Correct the 2010 density because of heavy snow cover in June
density.2010 <- rbind(LG1[which(LG1$year == 2010), ], 
                      LG2[which(LG2$year == 2010), ]) %>% 
  filter(period %in% c("P1", "P2")) %>% 
  summarise(density = mean(density))

# Group data set and keep relevant column
lemmingMeanDensity <- rbind(LG1, LG2, reconstructed) %>% 
  group_by(year) %>% 
  summarise(density = mean(density, na.rm = TRUE)) %>% 
  mutate(biomass = density * lem_mass) %>% 
  mutate(d_estimate = ifelse(year >= 2004, "secr", "log-log"))

saveRDS(lemmingMeanDensity, "data_clean/2_lemmingMeanDensity.rds")
