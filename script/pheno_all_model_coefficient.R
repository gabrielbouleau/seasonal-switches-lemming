library(tidyverse)
library(viridis)

# Test the glm for reconstructed density and predator group

#-------------------#
#   Predator data   #
#-------------------#
predator <- readRDS("script/Clean_Rdata/5_PredatorPresence.rds")

predator_infered <- readRDS("script/Clean_Rdata/Infered_predator_binom.rds")



#------------------#
#   Lemming data   #
#------------------#
lemmingDensity <- readRDS("script/Clean_Rdata/2_lemmingMeanDensity.rds")[, c(1,2)]

predator <- left_join(predator, lemmingDensity, by = "year")
predator[which(predator$period == "P3"), "density"] <- NA

predator <- predator[which(predator$Season == "S"),]



#-------------------------------------#
#   Mesure growth during each year    #
#-------------------------------------#
Growth <- lemmingDensity

Growth[, "Growth"] <- 0

for (i in 1:(nrow(Growth) - 1)) {
  Growth[i, "Growth"] <- log(Growth[i+1, "density"] / Growth[i, "density"]) # either divide by 1 or by 12 for monthly growth
}


#-----------------------------------------#
#   Merge predator and predator_infered   #
#-----------------------------------------#

predator <- predator[which(predator$Season == "S"), ]

predator[1:nrow(predator_infered$fox), "fox_repro"] <- predator_infered$fox$fox_repro
predator[1:nrow(predator_infered$jaeger), "jaeger"] <- predator_infered$jaeger$jaeger


predator <- left_join(predator, Growth[, -2], by = "year")


#------------------------------#
#   Lunch glm gaussian log !   #
#------------------------------#
glm01.log <- glm(exp(Growth) ~ weasel + jaeger, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm02.log <- glm(exp(Growth) ~ fox_repro + weasel, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm03.log <- glm(exp(Growth) ~ fox_repro + weasel + jaeger, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm04.log <- glm(exp(Growth) ~ owl + weasel + jaeger, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm05.log <- glm(exp(Growth) ~ owl + fox_repro + weasel, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm06.log <- glm(exp(Growth) ~ jaeger, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm07.log <- glm(exp(Growth) ~ owl + fox_repro + jaeger + weasel, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm08.log <- glm(exp(Growth) ~ fox_repro + jaeger, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm09.log <- glm(exp(Growth) ~ owl + jaeger, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm10.log <- glm(exp(Growth) ~ owl + fox_repro + jaeger, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm11.log <- glm(exp(Growth) ~ fox_repro, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm12.log <- glm(exp(Growth) ~ owl + weasel, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm13.log <- glm(exp(Growth) ~ weasel, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm14.log <- glm(exp(Growth) ~ owl + fox_repro, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm15.log <- glm(exp(Growth) ~ owl, data = predator[-nrow(predator), ], family = gaussian(link = "log"))

glm.log.list <- mget(ls(pattern = "glm"))


#------------------------------#
#   Extract stat form models   #
#------------------------------#

glm.stat <- matrix(NA, nrow = length(glm.log.list), ncol = 12)

colnames(glm.stat) <- c("int", "int.sd", 
                        "owl", "owl.sd",
                        "fox", "fox.sd",
                        "jaeger", "jaeger.sd",
                        "weasel", "weasel.sd",
                        "log.like", "delta.AIC")

for (i in 1:length(glm.log.list)) {
  
  # Intercept
    
    # mu
    glm.stat[i, "int"] <- glm.log.list[[i]]$coefficients[1]
  
    # sd
    glm.stat[i, "int.sd"] <- summary(glm.log.list[[i]])$coefficients[which(names(glm.log.list[[i]]$coefficients) == "(Intercept)"), "Std. Error"]
  
  # Owl
  if("owl" %in% names(coef(glm.log.list[[i]]))){
    # mu
    glm.stat[i, "owl"] <- glm.log.list[[i]]$coefficients[which(names(glm.log.list[[i]]$coefficients) == "owl")]
    
    # sd
    glm.stat[i, "owl.sd"] <- summary(glm.log.list[[i]])$coefficients[which(names(glm.log.list[[i]]$coefficients) == "owl"), "Std. Error"]
    
  }
    
  # Fox
  if("fox_repro" %in% names(coef(glm.log.list[[i]]))){
    # mu
    glm.stat[i, "fox"] <- glm.log.list[[i]]$coefficients[which(names(glm.log.list[[i]]$coefficients) == "fox_repro")]
    
    # sd
    glm.stat[i, "fox.sd"] <- summary(glm.log.list[[i]])$coefficients[which(names(glm.log.list[[i]]$coefficients) == "fox_repro"), "Std. Error"]
  }
    
  # jaeger
  if("jaeger" %in% names(coef(glm.log.list[[i]]))){
    # mu
    glm.stat[i, "jaeger"] <- glm.log.list[[i]]$coefficients[which(names(glm.log.list[[i]]$coefficients) == "jaeger")]
    
    # sd
    glm.stat[i, "jaeger.sd"] <- summary(glm.log.list[[i]])$coefficients[which(names(glm.log.list[[i]]$coefficients) == "jaeger"), "Std. Error"]
  }
    
  # weasel
  if("weasel" %in% names(coef(glm.log.list[[i]]))){
    # mu
    glm.stat[i, "weasel"] <- glm.log.list[[i]]$coefficients[which(names(glm.log.list[[i]]$coefficients) == "weasel")]
    
    # sd
    glm.stat[i, "weasel.sd"] <- summary(glm.log.list[[i]])$coefficients[which(names(glm.log.list[[i]]$coefficients) == "weasel"), "Std. Error"]
  }
  
  # Log-likelihood
  glm.stat[i, "log.like"] <- logLik(glm.log.list[[i]])[1]
  
  # Delta AIC
  if(i == 1){
    glm.stat[i, "delta.AIC"] <- 0
    base.AIC <- glm.log.list[[i]]$aic
    
  } else {
    glm.stat[i, "delta.AIC"] <- glm.log.list[[i]]$aic - base.AIC
    
  }

}


#--------------------#
#   Compute 95% CI   #
#--------------------#


CI <- matrix(NA, nrow = length(glm.log.list), ncol = 10)

colnames(CI) <- c("lwr.int", "upr.int",
                  "lwr.owl", "upr.owl",
                  "lwr.fox", "upr.fox",
                  "lwr.jaeger", "upr.jaeger",
                  "lwr.weasel", "upr.weasel")


for (i in 1:length(glm.log.list)) {
  
  # int
  CI[i, "lwr.int"] <- glm.stat[i, "int"] - 1.96 * glm.stat[i, "int.sd"]
  CI[i, "upr.int"] <- glm.stat[i, "int"] + 1.96 * glm.stat[i, "int.sd"]
  
  # owl
  CI[i, "lwr.owl"] <- glm.stat[i, "owl"] - 1.96 * glm.stat[i, "owl.sd"]
  CI[i, "upr.owl"] <- glm.stat[i, "owl"] + 1.96 * glm.stat[i, "owl.sd"]
  
  # fox
  CI[i, "lwr.fox"] <- glm.stat[i, "fox"] - 1.96 * glm.stat[i, "fox.sd"]
  CI[i, "upr.fox"] <- glm.stat[i, "fox"] + 1.96 * glm.stat[i, "fox.sd"]
  
  # int
  CI[i, "lwr.jaeger"] <- glm.stat[i, "jaeger"] - 1.96 * glm.stat[i, "jaeger.sd"]
  CI[i, "upr.jaeger"] <- glm.stat[i, "jaeger"] + 1.96 * glm.stat[i, "jaeger.sd"]

  # int
  CI[i, "lwr.weasel"] <- glm.stat[i, "weasel"] - 1.96 * glm.stat[i, "weasel.sd"]
  CI[i, "upr.weasel"] <- glm.stat[i, "weasel"] + 1.96 * glm.stat[i, "weasel.sd"]

}
