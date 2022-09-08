library(tidyverse)

lem_mass <- readRDS("data_clean/2_1_lemmingMass.rds")

#########################################################
#   Aggregate lemming density by year, period and grids
#########################################################

reconstructed <- read.csv2("data_raw/Densites_1993-2019.csv", sep = ",") %>% 
  filter(Year >= 1993 & Year < 2004) %>% 
  rename(year = Year, LG1 = Both.Wet.habitat, LG2 = Both.Mesic.habitat) %>% 
  select(year, LG1, LG2) %>% 
  pivot_longer(cols = c("LG1", "LG2"), names_to = "grid", values_to = "density") %>% 
  add_column(period = "P1", se = 0, cumulmonth = 0) %>% 
  mutate(cumulyear = year - min(.$year)) %>%
  mutate(biomass = density * lem_mass) %>% 
  select(year, period, grid, density, se, cumulyear, cumulmonth, biomass)

min.year <- min(reconstructed$year)
  

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

density.2010 <- rbind(LG1[which(LG1$year == 2010), ], 
                      LG2[which(LG2$year == 2010), ]) %>% 
  filter(period %in% c("P1", "P2")) %>% 
  summarise(density = mean(density))

lemmingMeanDensity <- rbind(LG1, LG2, reconstructed) %>% 
  group_by(year) %>% 
  summarise(density = mean(density, na.rm = TRUE)) %>% 
  mutate(biomass = density * lem_mass) %>% 
  mutate(d_estimate = ifelse(year >= 2004, "secr", "log-log"))


saveRDS(lemmingMeanDensity, "data_clean/2_lemmingMeanDensity.rds")
