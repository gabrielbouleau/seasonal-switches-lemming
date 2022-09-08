library(jsonlite)
library(tikzDevice)


#################################
#  Plot predator DCR per month 
#################################

color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)

lem_mass <- readRDS("data_clean/2_1_lemmingMass.rds")

#----------------------#
#  Load predator data  #
#----------------------#

Density <- readRDS("data_clean/5_PredMeanDensity.rds")

pred <- readRDS("data_clean/7_meca_attack_rate.RDS")
pred[, "Q_d"] <- pred$Q_B * pred$d / lem_mass # Lemming eaten by predator mass
pred[, "Q_density"] <- pred$alpha * c(Density$Owl, Density$Jaeger, 0.4/100, 0.02/100, 0.4/100, rep(Density$Fox, 3)) # Lemming eaten by population

lemming <- c(0:10)


#---------------------------#
#  Load predator threshold  #
#---------------------------#

threshold <- readRDS("data_clean/predator_threshold.rds")

fox_thresh <- seq(threshold$fox$mu, 10, length.out = 10)
fox_low <- seq(0, threshold$fox$mu, length.out = 10)
owl_thresh <- seq(threshold$owl$mu, 10, length.out = 10)
jaeger_thresh <- seq(threshold$jaeger$mu, 10, length.out = 10)


#------------------------------------#
#  Plot monthly lemming consumption  #
#------------------------------------#
tikz("results/figS3_consumption_rate.tex", height = 10, width = 10, standAlone = TRUE)

par(mfrow = c(2,2), mar = c(7.65, 7.65, 6.15, 3.15), mgp = c(3, 1.5, 0))

# Lemming mass consumption per mass of predator
plot(pred[8, "Q_d"]*fox_thresh ~ fox_thresh, type = "l",
     ylab = NA, xlab = NA,
     col = color[2], lwd = 4, cex.axis = 2,
     ylim = c(0, 50), xlim = c(0, 10))

mtext("Annual lemming consumption per predator mass\n(Lemming/g)", 2, cex = 1.5, line = 4)
mtext("Lemming density\n(ind/ha)", 1, cex = 1.5, line = 5)

lines(pred[6, "Q_d"]*fox_low ~ fox_low, col = color[2], lwd = 2)

lines(pred[7, "Q_d"]*lemming ~ lemming, col = color[2], lty = 2, lwd = 2)

lines(pred[1, "Q_d"]*owl_thresh ~ owl_thresh, col = color[1], lwd = 4)

lines(pred[2, "Q_d"]*jaeger_thresh ~ jaeger_thresh, col = color[3], lwd = 4)

lines(pred[3, "Q_d"]*lemming ~ lemming, col = color[4], lwd = 4)

lines(pred[4, "Q_d"]*lemming ~ lemming, col = color[4], lwd = 2)

lines(pred[5, "Q_d"]*lemming ~ lemming, col = color[4], lty = 2, lwd = 2)

mtext("A)", adj = -0.1, line = 2, cex = 2)


# Lemming mass consumption for 1 predator
plot(pred[8, "alpha"]*fox_thresh ~ fox_thresh, type = "l",
     ylab = NA, xlab = NA,
     col = color[2], lwd = 4, cex.axis = 2,
     ylim = c(0, 53000), xlim = c(0, 10))

mtext("Annual lemming consumption per individual\n(Lemming/ind)", 2, cex = 1.5, line = 4)
mtext("Lemming density\n(ind/ha)", 1, cex = 1.5, line = 5)

lines(pred[6, "alpha"]*fox_low ~ fox_low, col = color[2], lwd = 2)

lines(pred[7, "alpha"]*lemming ~ lemming, col = color[2], lty = 2, lwd = 2)

lines(pred[1, "alpha"]*owl_thresh ~ owl_thresh, col = color[1], lwd = 4)

lines(pred[2, "alpha"]*jaeger_thresh ~ jaeger_thresh, col = color[3], lwd = 4)

lines(pred[3, "alpha"]*lemming ~ lemming, col = color[4], lwd = 4)

lines(pred[4, "alpha"]*lemming ~ lemming, col = color[4], lwd = 2)

lines(pred[5, "alpha"]*lemming ~ lemming, col = color[4], lty = 2, lwd = 2)

mtext("B)", adj = -0.1, line = 2, cex = 2)


# Add legend
plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, bty = "n")

legend(0.6, 1.3, legend = c("Fox high summer", "Fox low summer", "Fox winter", "Owl high summer", "Jaeger high summer", "Ermine high summer", "Ermine low summer", "Ermine winter"),
       col = c(rep(color[2], 3), color[1], color[3], rep(color[4], 3)), lty = c(1,1,2,1,1,1,1,2), lwd = c(4,2,2,4,4,4,2,2), bty = "n", cex = 2)



# Same but with attack rate corrected for predator densities
plot(pred[8, "Q_density"]*fox_thresh ~ fox_thresh, type = "l",
     ylab = NA, xlab = NA,
     col = color[2], lwd = 4, cex.axis = 2, 
     ylim = c(0, 55), xlim = c(0, 10))

mtext("Annual lemming consumption\n(Lemming/ha)", 2, cex = 1.5, line = 4)
mtext("Lemming density\n(ind/ha)", 1, cex = 1.5, line = 5)

lines(pred[6, "Q_density"]*fox_low ~ fox_low, col = color[2], lwd = 2)

lines(pred[7, "Q_density"]*lemming ~ lemming, col = color[2], lty = 2, lwd = 2)

lines(pred[1, "Q_density"]*owl_thresh*2 ~ owl_thresh, col = color[1], lwd = 4)

lines(pred[2, "Q_density"]*jaeger_thresh*2 ~ jaeger_thresh, col = color[3], lwd = 4)

lines(pred[3, "Q_density"]*lemming ~ lemming, col = color[4], lwd = 4)

lines(pred[4, "Q_density"]*lemming ~ lemming, col = color[4], lwd = 2)

lines(pred[5, "Q_density"]*lemming ~ lemming, col = color[4], lty = 2, lwd = 2)

mtext("C)", adj = -0.1, line = 2, cex = 2)


dev.off()