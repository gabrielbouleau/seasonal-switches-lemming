library(tidyverse)
library(jsonlite)
library(tikzDevice)


# MUST BE RUN BEFOR THIS SCRIPT :
# predator_binomial_threshold.r

#############################
#   Predator density plot   #
#############################

color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)

#-------------------#
#   Predator data   #
#-------------------#

predator <- readRDS("data_clean/5_PredatorAbundance.rds")

#------------------#
#   Lemming data   #
#------------------#

lemmingDensity <- readRDS("data_clean/2_lemmingMeanDensity.rds")[, c(1,2)]

predator <- left_join(predator, lemmingDensity, by = "year")
predator[which(predator$period == "P3"), "density"] <- NA

rm(lemmingDensity)

# Presence absence threshold 
# MUST RUN THE SCRIP OF PREDATOR_TRESHOLD BEFORE THE PLOT !!! 
threshold <- readRDS("data_clean/predator_threshold.rds")

#---------------#
#   Snowy owl   #
#---------------#

tikz("results/figS5_predator_time_series_density.tex", height = 6, width = 10, standAlone = TRUE) 

par(mfrow = c(2, 4), mar = c(7.1, 6.1, 7.1, 1), mgp = c(3.6, 1, 0))

# Plot nest density vs years
barplot(owl ~ year, data = predator[which(predator$Season == "S"),], 
        col = color[1], border = NA, las = 2,
        xlab = "", ylab = "", ylim = c(0, 0.0025), cex.axis = 1.2, cex.names = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 4.5, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Snowy owl", adj = 3.5, line = 4, cex = 2)

mtext("A)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = 0.00015, lty = 2)



# Plot nest density vs lemming density
plot(owl ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[1],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("B)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$owl$mu, lty = 2)
abline(h = 0.00015, lty = 2)
text(threshold$owl$mu + 2, 0.002, paste("L* =", signif(threshold$owl$mu, 2)), font = 2, cex = 1.5)


#---------------#
#   Arctic fox  #
#---------------#
fox_threshold <- 0.1

# Plot active den proportion vs years
barplot(fox_repro ~ year, data = predator[which(predator$Season == "S"),],
        col = color[2], border = NA, las = 2,
        xlab = "", ylab = "", ylim = c(0, 0.35), cex.axis = 1.2, cex.names = 1.4)

mtext("Reproductive den\nproportion", side = 2, line = 3, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Arctic fox", adj = 3, line = 4, cex = 2)

mtext("C)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = fox_threshold, lty = 2)

text(9, 0.12, "Discretisation threshold", font = 2, cex = 1.2)


# Plot active den proportion vs mean june lemming density
plot(fox_repro ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[2],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Reproductive den\nproportion", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("D)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$fox$mu, lty = 2)
abline(h = fox_threshold, lty = 2)
text(threshold$fox$mu + 2, 0.28, paste("L* =", signif(threshold$fox$mu, 2)), font = 2, cex = 1.5)

text(5, 0.25, "True presence", cex = 1.2)
text(5, 0.05, "False presence", cex = 1.2)
text(0.8, 0.05, "True \nabsence", cex = 1.2)
text(0.8, 0.25, "False \nabsence", cex = 1.2)


#----------------------------#
#   Long-tailed jaeger       #
#----------------------------#
jaeger_threshold <- 0.001

# Plot nest density vs years
barplot(jaeger ~ year, data = predator[which(predator$Season == "S"),], col = color[3],
        xlab = "", ylab = "", border = NA, las = 2, ylim = c(0, 0.012), cex.axis = 1.2, cex.names = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 4, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Long-tailed jaeger", adj = -2, line = 4, cex = 2)

mtext("E)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = jaeger_threshold, lty = 2)


# Plot nest density vs lemming density
plot(jaeger ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[3],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("F)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$jaeger$mu, lty = 2)
abline(h = jaeger_threshold, lty = 2)
text(threshold$jaeger$mu + 2, 0.010, paste("L* =", signif(threshold$jaeger$mu, 2)), font = 2, cex = 1.5)


#----------------#
#   Weasel       #  
#----------------#

# Plot weasel index vs years
barplot(weasel ~ year, data = predator[which(predator$Season == "S"),], col = color[4], 
        xlab = "", ylab = "", border = NA, las = 2, ylim = c(0, 3), cex.axis = 1.2, cex.names = 1.4)

mtext("Abundance index", side = 2, line = 4, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Ermine", adj = 1.9, line = 4, cex = 2)

mtext("G)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = 0.5, lty = 2)


# Plot weasel index vs lemming density
plot(weasel ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[4],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Abundance index", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("H)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$weasel$mu, lty = 2)
abline(h = 0.5, lty = 2)

text(4.5, 2.7, "Binomial threshold not conclusive", cex = 1.2)


dev.off()
