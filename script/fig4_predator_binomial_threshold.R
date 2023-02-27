# Test for estimation of the L*
# Use a binomial regression to find the inflexion point
# Based on the methods explained in the script 'binomial_inflection_variation.r'

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

lemmingDensity <- readRDS("data_clean/2_lemmingMeanDensity.rds")[, c(1,2)]

# Predator presence/absence (from script '5_PredatorDensity.r')
predator <- readRDS("data_clean/5_PredatorPresence.rds") %>% 
  filter(period == "P1") %>% 
  dplyr::select(year, fox_repro, jaeger, owl, weasel)

predator <- left_join(lemmingDensity, predator, by = "year")

rm(lemmingDensity)



# #------------------------#
# #   Run binomial model   #
# #------------------------#
# 
# # Owl
# glm1 <- glm(owl ~ density, data = predator, family = "binomial")
# 
# # Fox
# glm2 <- glm(fox_repro ~ density, data = predator, family = "binomial")
# 
# # jaeger -- fitted probabilities -- not significant
# glm3 <- glm(jaeger ~ density, data = predator, family = "binomial")
# 
# # weasel -- not significant
# glm4 <- glm(weasel ~ density, data = predator, family = "binomial")
# 
# binomial.models <- list(owl = glm1, fox = glm2, jaeger = glm3, weasel = glm4)
# 
# # saveRDS(binomial.models, "data_clean/binomial_glm.rds")
# 
# 
# #------------------------------------------------------#
# #   Compute mu and sd for each logistic distribution   #
# #             around the inflection point              #
# #------------------------------------------------------#
# 
# # Owl
# mu1 <- unname(-coef(glm1)[1] / coef(glm1)[2])
# sd1 <- unname(sqrt(((1/coef(glm1)[2])^2 * pi^2) / 3))
# 
# # Fox
# mu2 <- unname(-coef(glm2)[1] / coef(glm2)[2])
# sd2 <- unname(sqrt(((1/coef(glm2)[2])^2 * pi^2) / 3))
# 
# # Jaeger
# mu3 <- unname(-coef(glm3)[1] / coef(glm3)[2])
# sd3 <- unname(sqrt(((1/coef(glm3)[2])^2 * pi^2) / 3))
# 
# # Weasel
# mu4 <- unname(-coef(glm4)[1] / coef(glm4)[2])
# sd4 <- unname(sqrt(((1/coef(glm4)[2])^2 * pi^2) / 3))
# 
# 
# # Save threshold RDS
# threshold <- list(owl = list(mu = mu1, sd = sd1),
#                   fox = list(mu = mu2, sd = sd2),
#                   jaeger = list(mu = mu3, sd = sd3),
#                   weasel = list(mu = mu4, sd = sd4))
# # saveRDS(threshold, "data_clean/predator_threshold.rds")

threshold <- readRDS("data_clean/predator_threshold.rds")
glm <- readRDS("data_clean/binomial_glm.rds")

#--------------------------------------------#
#   Plot binomial model + inflection point   #
#--------------------------------------------#

tikz("results/fig4_predator_binomial_threshold.tex", height = 10, width = 12, standAlone = TRUE)
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

mtext("Lemming density (n/ha)", side = 1, line = 3, cex = 1.7)
mtext("Owl discretised\npresence", side = 2, line = 3, cex = 1.7)


# abline(v = rnorm(1000, threshold$owl$mu, threshold$owl$sd), col = color[1])
abline(v = threshold$owl$mu + qnorm(0.975)*threshold$owl$sd, lty = 2, lwd = 2)
abline(v = threshold$owl$mu - qnorm(0.975)*threshold$owl$sd, lty = 2, lwd = 2)

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
