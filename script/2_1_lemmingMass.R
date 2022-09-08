library(data.table)
library(tidyverse)

library(ggplot2)

# ----------------------------#
#   Lemming adult mean mass   #
#-----------------------------#

lemming_data <- fread("data_raw/nordicanad_lemming_live_trapping_data_2004_2019_gauthier.txt") %>% 
  filter(str_detect(Site_Name, c("^LG1", "^LG2")))

lemming_data$Species <- tolower(lemming_data$Species)

lemming_data <- subset(lemming_data, Species %in% c("brown lemming", "collared lemming"))

# ggplot(lemming_data, aes(Mass, fill = Species)) + 
#   geom_histogram(alpha = 0.5, na.rm = TRUE, position = "identity")

# Lemming mass to use in conversion
lem_mass <- signif(mean(lemming_data$Mass, na.rm = TRUE), 4)

saveRDS(lem_mass, "data_clean/2_1_lemmingMass.rds") 
