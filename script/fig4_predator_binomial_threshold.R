
library(data.table)
library(tidyverse)
library(jsonlite)
library(tikzDevice)

# Color with transparency
color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)


#--------------------------------#
#   Load and set predator data   #
#--------------------------------#

lemmingDensity <- readRDS("data_clean/1_lemmingMeanDensity.rds")[, c(1,2)]

# Predator presence/absence (from script 'PredatorDensity.r')
predator <- readRDS("data_clean/5_PredatorPresence.rds") %>% 
  filter(period == "P1") %>% 
  dplyr::select(year, fox_repro, jaeger, owl, weasel)

predator <- left_join(lemmingDensity, predator, by = "year")

rm(lemmingDensity)

threshold <- readRDS("data_clean/3_predator_threshold.rds")
glm <- readRDS("data_clean/3_binomial_glm.rds")

#--------------------------------------------#
#   Plot binomial model + inflection point   #
#--------------------------------------------#

tikz("fig4_predator_binomial_threshold.tex", height = 10, width = 12, standAlone = TRUE)
# Investigate the two outlier point in the owl binomial point

set.seed(22)

par(mfrow = c(2, 2), mar = c(6.1, 9.1, 4.1, 2.1))

# Owl
plot(1, type = "n", 
     xlim = c(0, 10), ylim = c(0, 1),
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "")

axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))

mtext("Lemming density (n/ha)", side = 1, line = 3, cex = 2)
mtext("Owl discretised\npresence", side = 2, line = 3, cex = 2)


# abline(v = rnorm(1000, threshold$owl$mu, threshold$owl$sd), col = color[1])
abline(v = threshold$owl$mu + qnorm(0.975)*threshold$owl$sd, lty = 2, lwd = 1.7)
abline(v = threshold$owl$mu - qnorm(0.975)*threshold$owl$sd, lty = 2, lwd = 1.7)

points(owl ~ density, data = predator)

# text(predator$density[c(19)], predator$owl[c(19)], labels = predator$year[c(19)], pos = 2, cex = 1.5)

curve(predict(glm$owl, data.frame(density = x), type = "response"), add = TRUE, lwd = 4, col = color[1])
abline(v = threshold$owl$mu, lwd = 2)


# Fox
plot(1, type = "n", 
     xlim = c(0, 10), ylim = c(0, 1),
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "")

axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))

mtext("Lemming density (n/ha)", side = 1, line = 3, cex = 1.7)
mtext("Fox discretised\nreproduction", side = 2, line = 3, cex = 1.7)


# abline(v = rnorm(1000, threshold$fox$mu, threshold$fox$sd), col = color[2])
abline(v = threshold$fox$mu + qnorm(0.975)*threshold$fox$sd, lty = 2, lwd = 2)
abline(v = threshold$fox$mu - qnorm(0.975)*threshold$fox$sd, lty = 2, lwd = 2)

points(fox_repro ~ density, data = predator)

# text(predator$density[18], predator$fox_repro[c(18)], labels = predator$year[18], pos = 2, cex = 1.5)

curve(predict(glm$fox, data.frame(density = x), type = "response"), add = TRUE, lwd = 4, col = color[2])
abline(v = threshold$fox$mu, lwd = 2)


# Jaeger
plot(1, type = "n", 
     xlim = c(0, 10), ylim = c(0, 1),
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "")

axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))


mtext("Lemming density (n/ha)", side = 1, line = 3, cex = 1.7)
mtext("Jeager discretised\npresence", side = 2, line = 3, cex = 1.7)


# abline(v = rnorm(1000, threshold$jaeger$mu, threshold$jaeger$sd), col = color[3])
abline(v = threshold$jaeger$mu + qnorm(0.975)*threshold$jaeger$sd, lty = 2, lwd = 2)
abline(v = threshold$jaeger$mu - qnorm(0.975)*threshold$jaeger$sd, lty = 2, lwd = 2)

points(jaeger ~ density, data = predator)

curve(predict(glm$jaeger, data.frame(density = x), type = "response"), add = TRUE, lwd = 4, col = color[3])
abline(v = threshold$jaeger$mu, lwd = 2)


# Weasel
plot(1, type = "n",
     xlim = c(0, 10), ylim = c(0, 1),
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "")

axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))

mtext("Lemming density (n/ha)", side = 1, line = 3, cex = 1.7)
mtext("Ermine discretised\npresence", side = 2, line = 3, cex = 1.7)


# abline(v = rnorm(1000, threshold$weasel$mu, threshold$weasel$sd), col = color[4])
abline(v = threshold$weasel$mu + qnorm(0.975)*threshold$weasel$sd, lty = 3, lwd = 2)
abline(v = threshold$weasel$mu - qnorm(0.975)*threshold$weasel$sd, lty = 3, lwd = 2)

points(weasel ~ density, data = predator)

curve(predict(glm$weasel, data.frame(density = x), type = "response"), add = TRUE, lwd = 4, col = color[4])
abline(v = threshold$weasel$mu, lwd = 2)

dev.off()
