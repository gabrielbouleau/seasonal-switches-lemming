library(tidyverse)

# Test the glm for reconstructed density and predator group

#-------------------#
#   Predator data   #
#-------------------#
predator <- readRDS("data_clean/5_PredatorPresence.rds")

threshold <- readRDS("data_clean/predator_threshold.rds")

#------------------#
#   Lemming data   #
#------------------#
lemmingDensity <- readRDS("data_clean/2_lemmingMeanDensity.rds")[, c(1,2)]

predator <- predator[which(predator$Season == "S"),]

predator <- left_join(predator, lemmingDensity, by = "year")

rm(lemmingDensity)


#----------------------------------#
#   Diet and energitic need data   #
#----------------------------------#

pred <- read.csv2("data_raw/Sauvé_Legagneux_alpha2.csv", sep = ",")[, c(1:6)]

pred[, "Q_g"] <- pred$Q_B * pred$mass_g # Get Q
pred[, "Q_d_g"] <- pred$Q_g * pred$d # Pondaration for pred diet
pred[, "Q_d_ind"] <- pred$Q_d_g / pred$Lemming_g # Transform mass into lemming number

# For attack rate: divide energetic need by threshold for high state
# For low state or winter, use cross-multiplication to find attack rate slope
# This assume that the reduction in attack rate is proportional to the energetic need
# This also keep the relationship between the attack rate for one species intact

pred[, "R"] <- NA

# pred[which(pred$specie == "owl"), "R"] <- threshold$owl$mu
# pred[which(pred$specie == "jaeger"), "R"] <- threshold$jaeger$mu

# For weasel, use the mean density of predator during high state
pred[which(pred$specie == "weasel" & pred$season == "Hsummer"), "R"] <- mean(predator[which(predator$weasel == 1), "density"])
pred[which(pred$specie == "owl"), "R"] <- mean(predator[which(predator$owl == 1), "density"])
pred[which(pred$specie == "jaeger"), "R"] <- mean(predator[which(predator$jaeger == 1), "density"])

# Compute attack rate
pred[, "alpha"] <- pred$Q_d_ind / pred$R # Divide by prey density (Sauv? 2020)


# Change fox attack rate for Beardsell (2021)
pred[which(pred$specie == "arctic fox" & pred$season == "Hsummer"), "alpha"] <- 0.012 * 100 * 12 * 365

# Cross-multiplication
pred[which(pred$specie == "arctic fox" & pred$season == "Lsummer"), "alpha"] <- pred[6, "Q_d_ind"] * pred[8, "alpha"] / pred[8, "Q_d_ind"]
pred[which(pred$specie == "arctic fox" & pred$season == "winter"), "alpha"] <- pred[7, "Q_d_ind"] * pred[8, "alpha"] / pred[8, "Q_d_ind"]

pred[which(pred$specie == "weasel" & pred$season == "Lsummer"), "alpha"] <- pred[4, "Q_d_ind"] * pred[3, "alpha"] / pred[3, "Q_d_ind"]
pred[which(pred$specie == "weasel" & pred$season == "winter"), "alpha"] <- pred[5, "Q_d_ind"] * pred[3, "alpha"] / pred[3, "Q_d_ind"]



saveRDS(pred, "data_clean/7_meca_attack_rate.RDS")
