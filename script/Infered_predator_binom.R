#################################################################
#   Infered presence absence of predator before census began    #
#################################################################

#-------------------#
#   Predator data   #
#-------------------#
predator <- readRDS("data_clean/5_PredatorPresence.rds")

#------------------#
#   Lemming data   #
#------------------#

lemmingDensity <- readRDS("data_clean/2_lemmingMeanDensity.rds")[, c(1,2)]

predator <- left_join(predator, lemmingDensity, by = "year")
predator[which(predator$period == "P3"), "density"] <- NA

predator <- predator[which(predator$Season == "S"), c("year", "fox_repro", "jaeger", "density")]

rm(lemmingDensity)


#-------------------------------------#
#   Keep years without census data    #
#-------------------------------------#

fox <- predator[which(is.na(predator$fox_repro)), c("year", "fox_repro", "density")]

jaeger <- predator[which(is.na(predator$jaeger)), c("year", "jaeger", "density")]


#---------------------------------------#
#   Predict density with binomial glm   #
#---------------------------------------#

binomial.models <- readRDS("data_clean/binomial_glm.rds")

fox$fox_repro <- round(predict(binomial.models$fox, newdata = fox, type = "response"))

jaeger$jaeger <- round(predict(binomial.models$jaeger, newdata = jaeger, type = "response"))


#---------------#
#   Save rds    #
#---------------#


saveRDS(list(fox = fox, jaeger = jaeger), "data_clean/Infered_predator_binom.rds")
