library(tidyverse)
library(jsonlite)
library(tikzDevice)

# Color scale
color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(2,4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)

#-------------------#
#   Predator data   #
#-------------------#
predator <- readRDS("data_clean/5_PredatorPresence.rds")

predator_infered <- readRDS("data_clean/Infered_predator_binom.rds")


#------------------#
#   Lemming data   #
#------------------#
lemmingDensity <- readRDS("data_clean/2_lemmingMeanDensity.rds")[, c(1,2)]

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

predator <- predator[-nrow(predator), ]


#---------------------------------#
#   Predict mechanistic growth    #
#---------------------------------#

meca <- readRDS("data_clean/7_R2_meca.RDS")

# Add growth rate prediction
predator[, "Growth_meca"] <- 0

for (i in 1:nrow(predator)) {
  
  if(predator[i, "fox_repro"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        if(predator[i, "weasel"] == 1){
          
          predator[i, "Growth_meca"] <- meca$OFJW
          
        } else { predator[i, "Growth_meca"] <- meca$OFJ }
        
      } else if (predator[i, "weasel"] == 1){
        
        predator[i, "Growth_meca"] <- meca$FJW
        
      } else { predator[i, "Growth_meca"] <- meca$FJ}
      
    } else { predator[i, "Growth_meca"] <- meca$Fox}
    
  } else if(predator[i, "weasel"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        predator[i, "Growth_meca"] <- meca$OJW
        
      } else { predator[i, "Growth_meca"] <- meca$JW}
      
    } else { predator[i, "Growth_meca"] <- meca$Weasel}
    
  } else if (predator[i, "owl"] == 1){
    
    predator[i, "Growth_meca"] <- meca$Owl
    
  } else if (predator[i, "jaeger"] == 1){
    
    predator[i, "Growth_meca"] <- meca$Jaeger
    
  } else {predator[i, "Growth_meca"] <- meca$Lem_alone}
}


#-----------------------------------#
#   Predict phenomenologic growth   #
#-----------------------------------#

pheno <- readRDS("data_clean/glm1_log.RDS")

predator[, "Growth_pheno"] <- predict(pheno, newdata = predator, type = "response")


#---------------------------------------------------------#
#   Difference between obs and pred by predation group    #
#---------------------------------------------------------#

for (i in 1:nrow(predator)) {
  
  pred <- NULL
  
  if(predator[i, "owl"] == 1) pred <- paste0(pred, "O")
  
  if(predator[i, "fox_repro"] == 1) pred <- paste0(pred, "F")
  
  if(predator[i, "jaeger"] == 1) pred <- paste0(pred, "J")
  
  if(predator[i, "weasel"] == 1) pred <- paste0(pred, "E")
  
  if(is.null(pred)) pred <- "none"
  
  predator[i, "pred_class"] <- pred
  
}

rm(pred)

predator$pred_class <- factor(predator$pred_class, levels = c("none", "E", "JE", "OJE", "FJ", "OFJ", "FJE", "OFJE"))


tikz("results/FigS7_Growth_by_group.tex", width = 7, height = 7, standAlone = TRUE)

par(mar = c(7.1, 6.1, 4.1, 2.1))

boxplot(log(Growth_meca) ~ pred_class, data = predator, border = color[3],
        ylab = NA, xlab = NA, ylim = c(-4, 5.5), lwd = 2, cex.axis = 1.5)

mtext("Exponential growth rate", 2, cex = 2, line = 3.5)
mtext("Predation assemblages", 1, cex = 2, line = 4)

boxplot(log(Growth_pheno) ~ pred_class, data = predator, border = color[4], lwd = 2, cex.axis = 1.5, add = TRUE)

legend(5.8, 5.5, legend = c("Mechanistic", "Phenomenologic", "Observed values"), pch = c(NA, NA, 19), 
       lwd = c(2, 2, NA), box.lty = 0, col = c(color[c(3,4)], "black"), cex = 1.2)

abline(h = 0, lty = 2, col = "grey")

points(Growth ~ pred_class, data = predator, pch = 19)

dev.off()
