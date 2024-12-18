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
meca_se <- readRDS("data_clean/7_R2_var_meca.RDS")

# Add growth rate prediction + confidence interval
predator[, "meca_fit"] <- 0
predator[, "meca_se"] <- 0

for (i in 1:nrow(predator)) {
  
  if(predator[i, "fox_repro"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        if(predator[i, "weasel"] == 1){
          
          predator[i, "meca_fit"] <- meca$OFJW
          predator[i, "meca_se"] <- meca_se$OFJW
          
        } else { 
          predator[i, "meca_fit"] <- meca$OFJ
          predator[i, "meca_se"] <- meca_se$OFJ
        }
        
      } else if (predator[i, "weasel"] == 1){
        
        predator[i, "meca_fit"] <- meca$FJW
        predator[i, "meca_se"] <- meca_se$FJW
        
      } else { 
        predator[i, "meca_fit"] <- meca$FJ
        predator[i, "meca_se"] <- meca_se$FJ
      }
      
    } else { 
      predator[i, "meca_fit"] <- meca$Fox
      predator[i, "meca_se"] <- meca_se$Fox
    }
    
  } else if(predator[i, "weasel"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        predator[i, "meca_fit"] <- meca$OJW
        predator[i, "meca_se"] <- meca_se$OJW
        
      } else { 
        predator[i, "meca_fit"] <- meca$JW
        predator[i, "meca_se"] <- meca_se$JW
      }
      
    } else { 
      predator[i, "meca_fit"] <- meca$Weasel
      predator[i, "meca_se"] <- meca_se$Weasel}
    
  } else if (predator[i, "owl"] == 1){
    
    predator[i, "meca_fit"] <- meca$Owl
    
  } else if (predator[i, "jaeger"] == 1){
    
    predator[i, "meca_fit"] <- meca$Jaeger
    
  } else {
    predator[i, "meca_fit"] <- meca$Lem_alone
    predator[i, "meca_se"] <- meca_se$Lem_alone}
}

predator <- mutate(predator,
                   meca_fit = log(meca_fit))

predator <- mutate(predator,
                   meca_upr = meca_fit + (1.96 * meca_se),
                   meca_lwr = meca_fit - (1.96 * meca_se))

#-----------------------------------#
#   Predict phenomenologic growth   #
#-----------------------------------#

pheno <- readRDS("data_clean/glm1_log.RDS")

ilink <- family(pheno)$linkinv

predator <- bind_cols(predator, setNames(as_tibble(predict(pheno, predator, se.fit = TRUE)[1:2]), 
                                         c("pheno_fit_link", "pheno_se_link")))

predator <- mutate(predator,
                   pheno_upr = log(ilink(pheno_fit_link + (1.96 * pheno_se_link))),
                   pheno_lwr = log(ilink(pheno_fit_link - (1.96 * pheno_se_link))))


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

pred_var <- predator |> 
            distinct(pred_class, .keep_all = TRUE) |> 
            arrange(pred_class)

tikz("results/FigS7_Growth_by_group.tex", width = 7, height = 7, standAlone = TRUE)

par(mar = c(7.1, 6.1, 4.1, 2.1), lend=1)

boxplot(meca_fit ~ pred_class, data = predator, border = NA,
        ylab = NA, xlab = NA, ylim = c(-3.5, 5.5), lwd = 2, cex.axis = 1.5)

mtext("Exponential growth rate", 2, cex = 2, line = 3.5)
mtext("Predation assemblages", 1, cex = 2, line = 4)

# boxplot(pheno_fit_link ~ pred_class, data = predator, border = color[4], lwd = 2, cex.axis = 1.5, add = TRUE)

for (i in 1:nrow(pred_var)) {
  
  polygon(x = c(i, i+0.4, i+0.4, i), y = c(rep(pred_var[i, "pheno_lwr"], 2),  rep(pred_var[i, "pheno_upr"], 2)),
          col = adjustcolor(color[4], alpha = 0.5), border = NA)
  segments(x0 = i, x1 = i+0.4, y0 = pred_var[i, "pheno_fit_link"],
           col = adjustcolor(color[4]), lwd = 6)
  
  polygon(x = c(i-0.4, i, i, i-0.4), y = c(rep(pred_var[i, "meca_lwr"], 2),  rep(pred_var[i, "meca_upr"], 2)),
          col = adjustcolor(color[3], alpha = 0.5), border = NA)
  segments(x0 = i, x1 = i-0.4, y0 = pred_var[i, "meca_fit"],
           col = adjustcolor(color[3]), lwd = 6)
  
  
} 


legend(5.8, 5.5, legend = c("Mechanistic", "Phenomenological", "Observed values"), pch = c(NA, NA, 19), 
       lwd = c(2, 2, NA), box.lty = 0, col = c(color[c(3,4)], "black"), cex = 1.2)

abline(h = 0, lty = 2, col = "grey")

points(Growth ~ pred_class, data = predator, pch = 19)

dev.off()
