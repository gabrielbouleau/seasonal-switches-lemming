# Estimation of the L*
# Use a binomial regression to find the inflexion point
# Inference of predator presence before sensus

library(data.table)
library(tidyverse)

#--------------------------------------------#
#   Load and set lemming and predator data   #
#--------------------------------------------#
# Load lemming time series
lemmingDensity <- readRDS("data_clean/2_lemmingMeanDensity.rds")[, c(1,2)]

# Predator presence/absence time series
predator <- readRDS("data_clean/5_PredatorPresence.rds") %>% 
  filter(period == "P1") %>% 
  dplyr::select(year, fox_repro, jaeger, owl, weasel)

predator <- left_join(lemmingDensity, predator, by = "year")

rm(lemmingDensity)


#------------------------#
#   Run binomial model   #
#------------------------#
# Owl
glm1 <- glm(owl ~ density, data = predator, family = "binomial")

# Fox
glm2 <- glm(fox_repro ~ density, data = predator, family = "binomial")

# jaeger -- warning fitted probabilities numerically 0 or 1 occurred
glm3 <- glm(jaeger ~ density, data = predator, family = "binomial")

# weasel -- not significant
glm4 <- glm(weasel ~ density, data = predator, family = "binomial")

# Save model results in a list
binomial.models <- list(owl = glm1, fox = glm2, jaeger = glm3, weasel = glm4)
saveRDS(binomial.models, "data_clean/binomial_glm.rds")


#------------------------------------------------------#
#   Compute mu and sd for each logistic distribution   #
#             around the inflection point              #
#------------------------------------------------------#

# Owl
mu1 <- unname(-coef(glm1)[1] / coef(glm1)[2])
sd1 <- unname(sqrt(((1/coef(glm1)[2])^2 * pi^2) / 3))

# Fox
mu2 <- unname(-coef(glm2)[1] / coef(glm2)[2])
sd2 <- unname(sqrt(((1/coef(glm2)[2])^2 * pi^2) / 3))

# Jaeger
mu3 <- unname(-coef(glm3)[1] / coef(glm3)[2])
sd3 <- unname(sqrt(((1/coef(glm3)[2])^2 * pi^2) / 3))

# Weasel
mu4 <- unname(-coef(glm4)[1] / coef(glm4)[2])
sd4 <- unname(sqrt(((1/coef(glm4)[2])^2 * pi^2) / 3))


# Save threshold RDS
threshold <- list(owl = list(mu = mu1, sd = sd1),
                  fox = list(mu = mu2, sd = sd2),
                  jaeger = list(mu = mu3, sd = sd3),
                  weasel = list(mu = mu4, sd = sd4))

saveRDS(threshold, "data_clean/predator_threshold.rds")


#---------------------------------------------------------------#
#   Inferred presence absence of predator before census began   #
#---------------------------------------------------------------#
# Keep years without census data
fox <- predator[which(is.na(predator$fox_repro)), c("year", "fox_repro", "density")]

jaeger <- predator[which(is.na(predator$jaeger)), c("year", "jaeger", "density")]


#---------------------------------------#
#   Predict density with binomial glm   #
#---------------------------------------#

fox$fox_repro <- round(predict(binomial.models$fox, newdata = fox, type = "response"))

jaeger$jaeger <- round(predict(binomial.models$jaeger, newdata = jaeger, type = "response"))

#  Save to rds
saveRDS(list(fox = fox, jaeger = jaeger), "data_clean/Infered_predator_binom.rds")
