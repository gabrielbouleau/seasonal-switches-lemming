# Test for estimation of the L*
# Use a binomial regression to find the inflexion point
# Based on the methods explained in the script 'binomial_inflection_variation.r'

library(data.table)
library(tidyverse)
library(jsonlite)

# # Color with transparency
# color <- fromJSON(txt = "script/Clean_Rdata/colorJSON.json")
# color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(4:7),]
# color <- rgb(color[, 1], color[, 2], color[,3], max = 255, alpha = 15)



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



#------------------------#
#   Run binomial model   #
#------------------------#

# Owl
glm1 <- glm(owl ~ density, data = predator, family = "binomial")

# Fox
glm2 <- glm(fox_repro ~ density, data = predator, family = "binomial")

# jaeger -- fitted probabilities -- not significant
glm3 <- glm(jaeger ~ density, data = predator, family = "binomial")

# weasel -- not significant
glm4 <- glm(weasel ~ density, data = predator, family = "binomial")

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

# #--------------------------------------------#
# #   Plot binomial model + inflection point   #
# #--------------------------------------------#
# 
# pdf("results/clean_results/predator_binomial_threshold.pdf", height = 10, width = 12)
# # Investigate the two outlier point in the owl binomial point
# 
# set.seed(22)
# 
# par(mfrow = c(2, 2), mar = c(6.1, 7.1, 4.1, 2.1))
# 
# # Owl
# plot(1, type = "n", 
#      xlim = c(0, 10), ylim = c(0, 1),
#      yaxt = "n", xaxt = "n",
#      ylab =""  , xlab = "")
# 
# axis(1, cex.axis = 1.5)
# axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))
# 
# mtext("Lemming density (Ind/ha)", side = 1, line = 3, cex = 1.5)
# mtext("Owl discretised\npresence", side = 2, line = 3, cex = 1.5)
# 
# 
# abline(v = rnorm(1000, mu1, sd1), col = color[1])
# abline(v = mu1 + qnorm(0.975)*sd1, lty = 2, lwd = 1.6)
# abline(v = mu1 - qnorm(0.975)*sd1, lty = 2, lwd = 1.6)
# 
# points(owl ~ density, data = predator)
# 
# text(predator$density[c(19)], predator$owl[c(19)], labels = predator$year[c(19)], pos = 2)
# 
# curve(predict(glm1, data.frame(density = x), type = "response"), add = TRUE, lwd = 1.6)
# abline(v = mu1, lwd = 1.6)
# 
# text(6, 0.4, labels = c(paste("mu =", signif(mu1, 5), 
#                               "\n",
#                               "std.dev =", signif(sd1, 5),
#                               "\n",
#                               "95% conf. int. =", signif(mu1 - qnorm(0.975)*sd1, 5), "-", signif(mu1 + qnorm(0.975)*sd1, 5))))
# 
# # Fox
# plot(1, type = "n", 
#      xlim = c(0, 10), ylim = c(0, 1),
#      yaxt = "n", xaxt = "n",
#      ylab =""  , xlab = "")
# 
# axis(1, cex.axis = 1.5)
# axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))
# 
# mtext("Lemming density (Ind/ha)", side = 1, line = 3, cex = 1.5)
# mtext("Fox discretised\nreproduction", side = 2, line = 3, cex = 1.5)
# 
# 
# abline(v = rnorm(1000, mu2, sd2), col = color[2])
# abline(v = mu2 + qnorm(0.975)*sd2, lty = 2, lwd = 1.6)
# abline(v = mu2 - qnorm(0.975)*sd2, lty = 2, lwd = 1.6)
# 
# points(fox_repro ~ density, data = predator)
# 
# text(predator$density[18], predator$fox_repro[c(18)], labels = predator$year[18], pos = 2)
# 
# curve(predict(glm2, data.frame(density = x), type = "response"), add = TRUE, lwd = 1.6)
# abline(v = mu2, lwd = 1.6)
# 
# text(7.5, 0.6, labels = c(paste("mu =", signif(mu2, 5), 
#                               "\n",
#                               "std.dev =", signif(sd2, 5),
#                               "\n",
#                               "95% conf. int. =", signif(mu2 - qnorm(0.975)*sd2, 5), "-", signif(mu2 + qnorm(0.975)*sd2, 5))))
# 
# # Jaeger
# plot(1, type = "n", 
#      xlim = c(0, 10), ylim = c(0, 1),
#      yaxt = "n", xaxt = "n",
#      ylab =""  , xlab = "")
# 
# axis(1, cex.axis = 1.5)
# axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))
# 
# 
# mtext("Lemming density (Ind/ha)", side = 1, line = 3, cex = 1.5)
# mtext("Jeager discretised\npresence", side = 2, line = 3, cex = 1.5)
# 
# 
# abline(v = rnorm(1000, mu3, sd3), col = color[3])
# abline(v = mu3 + qnorm(0.975)*sd3, lty = 2, lwd = 1.6)
# abline(v = mu3 - qnorm(0.975)*sd3, lty = 2, lwd = 1.6)
# 
# points(jaeger ~ density, data = predator)
# 
# curve(predict(glm3, data.frame(density = x), type = "response"), add = TRUE, lwd = 1.6)
# abline(v = mu3, lwd = 1.6)
# 
# text(7.5, 0.6, labels = c(paste("mu =", signif(mu3, 5), 
#                               "\n",
#                               "std.dev =", signif(sd3, 5),
#                               "\n",
#                               "95% conf. int. =", signif(mu3 - qnorm(0.975)*sd3, 5), "-", signif(mu3 + qnorm(0.975)*sd3, 5))))
# 
# # Weasel
# plot(1, type = "n",
#      xlim = c(0, 10), ylim = c(0, 1),
#      yaxt = "n", xaxt = "n",
#      ylab =""  , xlab = "")
# 
# axis(1, cex.axis = 1.5)
# axis(2, cex.axis = 1.5, at = c(0, 0.5, 1))
# 
# mtext("Lemming density (Ind/ha)", side = 1, line = 3, cex = 1.5)
# mtext("Weasel discretised\npresence", side = 2, line = 3, cex = 1.5)
# 
# 
# abline(v = rnorm(1000, mu4, sd4), col = color[4])
# abline(v = mu4 + qnorm(0.975)*sd4, lty = 2, lwd = 1.6)
# abline(v = mu4 - qnorm(0.975)*sd4, lty = 2, lwd = 1.6)
# 
# points(weasel ~ density, data = predator)
# 
# curve(predict(glm4, data.frame(density = x), type = "response"), add = TRUE, lwd = 1.6)
# abline(v = mu4, lwd = 1.6)
# 
# text(6, 0.4, labels = c(paste("mu =", signif(mu4, 5), 
#                                 "\n",
#                                 "std.dev =", signif(sd4, 5),
#                                 "\n",
#                                 "95% conf. int. =", signif(mu4 - qnorm(0.975)*sd4, 5), "-", signif(mu4 + qnorm(0.975)*sd4, 5))))
# 
# 
# dev.off()
