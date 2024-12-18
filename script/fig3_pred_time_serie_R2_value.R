library(jsonlite)
library(tidyverse)
library(tikzDevice)

####################################
#   Presence-absence time-serie    #
####################################

color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(2,4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)



#-------------------#
#   Predator data   #
#-------------------#
predator <- readRDS("data_clean/2_PredatorPresence.rds")

predator_infered <- readRDS("data_clean/3_infered_predator_binom.rds")



#------------------#
#   Lemming data   #
#------------------#

lemmingDensity <- readRDS("data_clean/1_lemmingMeanDensity.rds")[, c(1,2)]

predator <- left_join(predator, lemmingDensity, by = "year")

predator <- predator[which(predator$Season == "S"),]



#-------------------------------------#
#   Measure growth during each year    #
#-------------------------------------#
Growth <- lemmingDensity

Growth[, "Growth"] <- 0

for (i in 1:(nrow(Growth) - 1)) {
  Growth[i, "Growth"] <- log(Growth[i+1, "density"] / Growth[i, "density"]) # either divide by 1 or by 12 for monthly growth
}

# Position the growth bar of the barplot between years
# to illustrate where the growth happens
Growth$year <- Growth$year + 0.5 



#-------------------------------#
#   Arrange data for plotting   #
#-------------------------------#

# Create vector of point type based on presence/absence
fox_vec <- predator$fox_repro
fox_vec[which(fox_vec == 1)] <- 19
fox_vec[which(fox_vec == 0)] <- 1

jaeger_vec <- predator$jaeger
jaeger_vec[which(jaeger_vec == 1)] <- 19
jaeger_vec[which(jaeger_vec == 0)] <- 1

owl_vec <- predator$owl
owl_vec[which(owl_vec == 1)] <- 19
owl_vec[which(owl_vec == 0)] <- 1

weasel_vec <- predator$weasel
weasel_vec[which(weasel_vec == 1)] <- 19
weasel_vec[which(weasel_vec == 0)] <- 1

# For Inferred presence, set shape as square (aka pch = 0 or 15)
predator_infered$fox$fox_repro[which(predator_infered$fox$fox_repro == 1)] <- 15

predator_infered$jaeger$jaeger[which(predator_infered$jaeger$jaeger == 1)] <- 15

# Presence absence threshold 
threshold <- readRDS("data_clean/3_predator_threshold.rds")



#--------------------------------------------#
#   Create predator + lemming time-series    #
#--------------------------------------------#

tikz("fig3_pred_time_serie_R2_value.tex", height = 6, width = 12, standAlone = TRUE)

layout(matrix(c(1,1,1,1,3,3,
                1,1,1,1,3,3,
                2,2,2,2,3,3), ncol = 6, byrow = TRUE))

## First plot : time-serie
par(mar = c(2.5, 6.5, 5.1, 2.5))

plot(density ~ year, data = predator, type = "b", lwd = 1.9, 
     ylim = c(-0.5, 10), xaxt = "n", ylab = "Lemming density (n/ha)", bty ="L",
     cex.lab = 2, cex.axis = 1.5)

axis(side = 1, at = seq(1993, 2020, by = 2), cex.axis = 1.5)

points(density ~ year, data = predator, pch = 19, cex = 4.8, col = "white") # White circle to interupt the line

# Thresholds
abline(h = threshold$jaeger$mu, col = color[4], lty = 2, lwd = 1.5) # jeager
abline(h = threshold$fox$mu, col = color[3], lty = 2, lwd = 1.5) # fox
abline(h = threshold$owl$mu, col = color[2], lty = 2, lwd = 1.5) # owl

# Observed points
points(c(predator$density - 0.15) ~ c(predator$year - 0.16), pch = jaeger_vec, col = color[4], cex = 1.5) # Jaeger
points(c(predator$density - 0.15) ~ c(predator$year + 0.16), pch = fox_vec, col = color[3], cex = 1.5) # Fox
points(c(predator$density + 0.15) ~ c(predator$year - 0.16), pch = owl_vec, col = color[2], cex = 1.5) # Owl
points(c(predator$density + 0.15) ~ c(predator$year + 0.16), pch = weasel_vec, col = color[5], cex = 1.5) # Owl

# Infered points
points(c(predator_infered$jaeger$density - 0.15) ~ c(predator_infered$jaeger$year - 0.16), pch = predator_infered$jaeger$jaeger, col = color[4], cex = 1.5)
points(c(predator_infered$fox$density - 0.15) ~ c(predator_infered$fox$year + 0.16), pch = predator_infered$fox$fox_repro, col = color[3], cex = 1.5)

legend(2003, 10, legend = c("Owl", "Fox", "Jaeger", "Ermine"), col = color[c(2,3,4,5)], lwd = 2, bty = "n", cex = 1.8)

pos <- legend(2008, 10, legend = c("Observed / Infered Presence", "Observed / Infered Absence", "Threshold"),
              lty = c(NA, NA, 2), lwd = c(NA, NA, 1.5), bty = "n", cex = 1.8)

points(x = rep(pos$text$x, times = 2) - c(1.6, 0.8), #c(0.85, 0.35)
       y = rep(pos$text$y, times = 2), cex = 1.9, pch = c(16, 0, NA, 15, 1, NA))

mtext("A)", adj = -0.1, line = 2, cex = 2)


#--------------------------------------#
#    Second plot : Growth histogram    #
#--------------------------------------#

par(mar = c(5.5, 6.5, 3.5, 2.5), bty = "o")

# Fake plot
barplot(Growth ~ year, data = Growth[-nrow(Growth),], ylim = c(-5, 5), col = NA, border = NA,
        xaxt = "n", yaxt = "n", space = 0, cex.lab = 2.5, ylab = "Growth rate")

axis(side = 2, at = seq(-5, 5, by = 2.5), cex.axis = 2)

abline(h = c(seq(-5, 5, by = 2.5)), col = color[1], lty = 2)

# To have the bars above the horizontal grey lines
barplot(Growth ~ year, data = Growth[-nrow(Growth),], xlab = NA, xaxt = "n", yaxt = "n", 
        col = color[1], space = 0, add = TRUE, border = NA)

mtext("B)", adj = -0.1, line = 3, cex = 2)


#-------------------------------------------#
#    Third graph : R2 values by predator    #
#-------------------------------------------#

par(mar = c(13.1, 4.5, 13.1, 0.5), bty = "o")

meca <- readRDS("data_clean/5_R2_meca.RDS")

#------------------------------------------------------------#
#   Phenomenological - Create a predicted value data frame   #
#------------------------------------------------------------#

glm1 <- readRDS("data_clean/4_glm_log.RDS")

pheno <- data.frame(owl =       c(1,0,0, 1,0,0,0,0,1,1,0),
                   fox_repro = c(0,1,0, 1,0,0,1,0,1,0,1),
                   jaeger =    c(0,0,1, 1,1,0,1,0,1,1,1),
                   weasel =    c(0,0,0, 1,1,0,0,1,0,1,1))

for (i in 1:nrow(pheno)) {
  
  pclass <- NULL
  
  if(pheno[i, "owl"] == 1) pclass <- paste0(pclass, "O")
  
  if(pheno[i, "fox_repro"] == 1) pclass <- paste0(pclass, "F")
  
  if(pheno[i, "jaeger"] == 1) pclass <- paste0(pclass, "J")
  
  if(pheno[i, "weasel"] == 1) pclass <- paste0(pclass, "W")
  
  if(is.null(pclass)) pclass <- "none"
  
  pheno[i, "pred_class"] <- pclass
  
}

pheno$pred_class <- factor(pheno$pred_class, levels = c("none", "O", "F", "J","W", "JW", "OJW", "FJ", "OFJ", "FJW", "OFJW"))

pheno <- pheno[order(pheno$pred_class), ]


pheno[, "pred_growth"] <- predict(glm1, newdata = pheno, type = "response")

pheno <- pheno[, c(5,6)]

#-------------------#
#   Initiate plot   #
#-------------------#

plot(NULL, xlim=c(1, 10), ylim=c(0.1 , 110), xaxt = "n", yaxt = "n",
     xlab = "Predator assemblages", ylab = "$R_i$ values", log = "y", cex.lab = 2)

# Line of R2 = 1, below this point cycle happens
abline(h = 1, lty = 2)

#Owl
points(0.9, meca$Owl, pch = 19, col = color[2])
segments(x0 = 0.9, y0 = meca$Owl,  y1 = meca$Lem_alone, col = color[2])

points(1.1, pheno[which(pheno$pred_class == "O"), 2], pch = 19, col = color[2])
segments(x0 = 1.1, y0 = pheno[which(pheno$pred_class == "O"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = color[2], lty = 2)


# Fox
points(1.9, meca$Fox, pch = 19, col = color[3])
segments(x0 = 1.9, y0 = meca$Fox, y1 = meca$R1_F, col = color[3])

points(2.1, pheno[which(pheno$pred_class == "F"), 2], pch = 19, col = color[3])
segments(x0 = 2.1, y0 = pheno[which(pheno$pred_class == "F"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = color[3], lty = 2)


# Jaeger
points(2.9, meca$Jaeger, pch = 19, col = color[4])
segments(x0 = 2.9, y0 = meca$Jaeger,  y1 = meca$Lem_alone, col = color[4])

points(3.1, pheno[which(pheno$pred_class == "J"), 2], pch = 19, col = color[4])
segments(x0 = 3.1, y0 = pheno[which(pheno$pred_class == "J"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = color[4], lty = 2)


# Weasel
points(3.9, meca$Weasel, pch = 19, col = color[5])
segments(x0 = 3.9, y0 = meca$Weasel, y1 = meca$R1_W, col = color[5])

points(4.1, pheno[which(pheno$pred_class == "W"), 2], pch = 19, col = color[5])
segments(x0 = 4.1, y0 = pheno[which(pheno$pred_class == "W"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = color[5], lty = 2)


# JW
points(4.9, meca$JW, pch = 19, col = "black")
segments(x0 = 4.9, y0 = meca$JW,  y1 = meca$R1_W, col = "black")

points(5.1, pheno[which(pheno$pred_class == "JW"), 2], pch = 19, col = "black")
segments(x0 = 5.1, y0 = pheno[which(pheno$pred_class == "JW"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = "black", lty = 2)


# OJW
points(5.9, meca$OJW, pch = 19, col = "black")
segments(x0 = 5.9, y0 = meca$OJW, y1 = meca$R1_W, col = "black")

points(6.1, pheno[which(pheno$pred_class == "OJW"), 2], pch = 19, col = "black")
segments(x0 = 6.1, y0 = pheno[which(pheno$pred_class == "OJW"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = "black", lty = 2)


# FJ
points(6.9, meca$FJ, pch = 19, col = "black")
segments(x0 = 6.9, y0 = meca$FJ,  y1 = meca$R1_F, col = "black")

points(7.1, pheno[which(pheno$pred_class == "FJ"), 2], pch = 19, col = "black")
segments(x0 = 7.1, y0 = pheno[which(pheno$pred_class == "FJ"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = "black", lty = 2)


# OFJ
points(7.9, meca$OFJ, pch = 19, col = "black")
segments(x0 = 7.9, y0 = meca$OFJ, y1 = meca$R1_F, col = "black")

points(8.1, pheno[which(pheno$pred_class == "OFJ"), 2], pch = 19, col = "black")
segments(x0 = 8.1, y0 = pheno[which(pheno$pred_class == "OFJ"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = "black", lty = 2)


# FJW
points(8.9, meca$FJW, pch = 19, col = "black")
segments(x0 = 8.9, y0 = meca$FJW,  y1 = meca$R1_FW, col = "black")

points(9.1, pheno[which(pheno$pred_class == "FJW"), 2], pch = 19, col = "black")
segments(x0 = 9.1, y0 = pheno[which(pheno$pred_class == "FJW"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = "black", lty = 2)


# OFJW
points(9.9, meca$OFJW, pch = 19, col = "black")
segments(x0 = 9.9, y0 = meca$OFJW, y1 = meca$R1_FW, col = "black")

points(10.1, pheno[which(pheno$pred_class == "OFJW"), 2], pch = 19, col = "black")
segments(x0 = 10.1, y0 = pheno[which(pheno$pred_class == "OFJW"), 2],  y1 = pheno[which(pheno$pred_class == "none"), 2], col = "black", lty = 2)


# Lemming
R1_vector <- c(meca$Lem_alone, meca$R1_F, meca$Lem_alone, rep(meca$R1_W, 3), rep(meca$R1_F, 2), rep(meca$R1_FW, 2))

points(R1_vector ~ c(0.9:9.9) , pch = 19)

points(rep(pheno$pred_growth[1], 10) ~ c(1.1:10.1), pch = 19)


legend(0.3 ~ 1, legend = c("Mechanistic", "Phenomenological"),
       col = c("black", "black"), lwd = 2, bty = "n", lty = c(1, 2), cex = 1.5)

text(4, 0.7, "Decline", font = 2, cex = 1.5)
text(4, 1.6, "Growth", font = 2, cex = 1.5)

axis(1, at = c(1:10), labels = c("O", "F", "J","E", "JE", "OJE", "FJ", "OFJ", "FJE", "OFJE"), cex.axis = 1.2)
axis(2, at = c(0.1, 1, 10, 100), cex.axis = 2)

mtext("C)", adj = -0.1, line = 3, cex = 2)

dev.off()

