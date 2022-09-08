library(tidyverse)
library(viridis)

# Test the glm for reconstructed density and predator group

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

hist(Growth$Growth, breaks = 15)

plot(ecdf(Growth$Growth))

hist(exp(Growth$Growth), breaks = 15)

plot(ecdf(exp(Growth$Growth)))


# La distribution des données log est pas tant normal
# Celle des données non-logué ressemble à une gamma?


#-----------------------------------------#
#   Merge predator and predator_infered   #
#-----------------------------------------#

predator <- predator[which(predator$Season == "S"), ]

predator[1:nrow(predator_infered$fox), "fox_repro"] <- predator_infered$fox$fox_repro
predator[1:nrow(predator_infered$jaeger), "jaeger"] <- predator_infered$jaeger$jaeger


predator <- left_join(predator, Growth[, -2], by = "year")

predator[, "pred_indice"] <- rowSums(predator[, c(4:7)])

predator[, "pred_absolute"] <- ifelse(predator$pred_indice >= 1, 1, 0)




#------------------------------#
#   Lunch glm gaussian log !   #
#------------------------------#
glm1.log <- glm(exp(Growth) ~ owl + weasel + jaeger + fox_repro, data = predator[-nrow(predator), ], family = gaussian(link = "log"))


#---------------------------------#
#   Selected complete log model   #
#---------------------------------#

# Make shure the diagnostic plot are ok. The residual vs fitted seems to have increasing variance.

saveRDS(glm1.log, "data_clean/glm1_log.RDS")

# #---------------------------------#
# #   Time-serie comparison plot    #
# #---------------------------------#
# pdf("results/proof_other/regression_plot/glm_log_all_pred.pdf", height = 5, width = 8)
# 
# plot(Growth ~ year, data = glm.log.predict, pch = 19, type = "b", 
#      main = paste0(c(glm.log.list[[1]]$call[[2]]), ", ", c(glm.log.list[[1]]$call[[3]])), ylab = "Continuous Growth")
# 
# abline(h = 0, lty = 2, col = "lightgrey")
# 
# points(log(glm.1.log) ~ year, data = glm.log.predict, col = viridis(4)[3], type = "b")
# 
# dev.off()
# 
# 
# 
# #-----------------------------------#
# #   Predicted vs observed R2 plot   #
# #-----------------------------------#
# pdf("results/proof_other/regression_plot/glm_log_all_R2.pdf", width = 5, height = 5)
# 
# plot(log(glm.1.log) ~ Growth, data = glm.log.predict, xlim = c(-5, 5), ylim = c(-5, 5),
#      ylab = "Predicted Growth", xlab = "Measured Growth", col = viridis(4)[1])
# 
# abline(h = 0, v = 0, lty = 2, col = "lightgrey")
# abline(0,1, lty = 2)
# 
# text(-2.7, 4.5, paste0("R-square value : ", signif(cor(glm.log.predict$Growth, glm.log.predict$glm.1.log)^2, digits = 3)))
# 
# dev.off()
# 
# 
# 
# #-----------------------------------#
# #   Effect size by predator group   #
# #-----------------------------------#
# effect_df <- predator[, c(1, 4:9)]
# 
# for (i in 1:(nrow(effect_df)-1)) {
#   
#   pred <- NULL
#   
#   if(effect_df[i, "owl"] == 1) pred <- paste0(pred, "O")
#   
#   if(effect_df[i, "fox_repro"] == 1) pred <- paste0(pred, "F")
#   
#   if(effect_df[i, "jaeger"] == 1) pred <- paste0(pred, "J")
#   
#   if(effect_df[i, "weasel"] == 1) pred <- paste0(pred, "W")
#   
#   if(is.null(pred)) pred <- "none"
#   
#   effect_df[i, "pred_class"] <- pred
#   
# }
# 
# 
# effect_df$pred_class <- factor(effect_df$pred_class, levels = c("none", "W", "JW", "OJW", "FJ", "OFJ", "FJW", "OFJW"))
# 
# effect_df <- cbind(effect_df, glm.log.predict[, "glm.1.log"]) %>% 
#   rename(glm1.log = "glm.log.predict[, \"glm.1.log\"]")
# 
# 
# # Match color to level
# color <- with(effect_df, 
#               data.frame(pred_class = levels(pred_class), 
#                          color = viridis(nlevels(pred_class))))
# 
# 
# pdf("results/proof_other/regression_plot/glm_log_all_pred_effect.pdf", width = 7, height = 7)
# 
# boxplot(glm1.log ~ pred_class, data = effect_df, log = "y", ylim = c(0.01, 150), border = color$color,
#         ylab = "Growth", xlab = "Predation group")
# 
# legend(5.5, 140, legend = c("Observed Growth", "Model prediction"), pch = c(1, NA), lwd = c(NA, 2), box.lty = 0)
# 
# abline(h = 1, lty = 2)
# 
# points(exp(Growth) ~ pred_class, data = effect_df, col = color$color[match(effect_df$pred_class, color$pred_class)])
# 
# dev.off()
# 
# 
# 
# ##################### 
# # Tried to plot the confidence intervals. Gone wild
# 
# # Predict mean + confidence interval for each predator group
# newdat <- unique(effect_df[, c(2:5, 8)])[-9, ]
# newdat$pred_class <- factor(newdat$pred_class, levels = c("none", "W", "JW", "OJW", "FJ", "OFJ", "FJW", "OFJW"))
# 
# ilink <- family(glm1.log)$linkinv # Isolate inverse link function
# 
# ci.factor <- qnorm(1 - (1 - 0.95)/2) # 95% CI
# 
# # calculate CIs:
# fit <- predict(glm1.log, newdata = newdat,  se.fit = TRUE)[1:2]
# 
# newdat[, "fit"] <- ilink(fit$fit)
# 
# newdat[, "lwr"] <- ilink(fit$fit - (ci.factor * fit$se.fit))
# 
# newdat[, "upr"] <- ilink(fit$fit + (ci.factor * fit$se.fit))
# 
# 
# plot.default(newdat$fit ~ newdat$pred_class, type = "p", axes = FALSE,
#              ylab = "Growth", xlab = "Predation group", pch = 19, log = "y",
#              ylim = c(min(newdat$lwr), max(newdat$upr)))
# 
# axis(side = 1, at = as.numeric(newdat$pred_class), labels = newdat$pred_class)
# axis(side = 2)
# 
# points(newdat$lwr ~ newdat$pred_class, pch = 19, col = "blue")
# points(newdat$upr ~ newdat$pred_class, pch = 19, col = "red")
# 
# box() # draw a box around plot
# 
# # What to do with this insane variation ???
# 
# ######################
# 
# 
# 
# #-----------------------------------------------#
# #   Observed density vs projected time serie    #
# #-----------------------------------------------#
# pdf("results/proof_other/regression_plot/glm_log_all_density_plot.pdf", height = 5, width = 8)
# 
# # Empty plot
# plotrix::gap.plot(0, gap = c(10, 25), gap.axis = "y",
#                   ylim = c(0, 27), xlim = c(1993, 2019), type = "b", 
#                   ytics = seq(0, 27, by = 2), xtics = c(1993:2019),
#                   ylab = "Density (ha)", xlab = "Year")
# 
# seg <- data.frame(x0 = effect_df$year[-27], x1 = effect_df$year[-1],
#                   y0 = effect_df$density[-27], y1 = (effect_df$density * effect_df$glm1.log)[-27])
# 
# # Add segement with type = "b
# for (i in 1:nrow(seg)) {
#   
#   plotrix::gap.plot(y = seg[i, 3:4], x = seg[i, 1:2], gap = c(10, 25), gap.axis = "y", type = "b", add = TRUE,
#                     col = color[6, 2], ylim = c(0, 27), xlim = c(1993, 2019))
# }
# 
# # Link the point abive gap
# X <- c(2006, 2007)
# Y <- c(0.52150000, 26.7560428 - 15)
# 
# points(X, Y, type = "b", xlim = c(1993, 2019), ylim = c(0, 27),
#        xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, col = color[6, 2])
# 
# rm(X, Y)
# 
# # Plot Observed density
# plotrix::gap.plot(y = effect_df$density, x = effect_df$year, gap = c(10, 25), gap.axis = "y",
#                   ylim = c(0, 27), type = "b", ytics = seq(0, 27, by = 2), pch = 19, add = TRUE)
# 
# # Plot predicted point above black point
# plotrix::gap.plot(y = (effect_df$density * effect_df$glm1.log)[-27], x = effect_df$year[-1],
#                   gap = c(10, 25), gap.axis = "y", col = color[6, 2], add = TRUE)
# 
# # Correct axis gap
# abline(h = seq(10, 10.3, 0.001), col = "white")
# plotrix::axis.break(axis = 2, breakpos = 10.15, style = "slash")       
# plotrix::axis.break(axis = 4, breakpos = 10.15, style = "slash")
# 
# # Legend
# legend(2011, 12, legend = c("Observed densities", "Predicted densities"), pch = c(19, 1),
#        col = c("black", color[6, 2]), box.lty = 0)
# 
# dev.off()
# 
# 
# 
# #---------------------------------------------#
# #   Plot predicted density vs observed plot   #
# #---------------------------------------------#
# for (i in 1:(nrow(effect_df) -1)) {
#   effect_df[i+1, "predict_density"] <- effect_df[i, "density"] * effect_df[i, "glm1.log"]
# }
# 
# 
# pdf("results/proof_other/regression_plot/glm_log_all_density_cor.pdf", width = 5, height = 5)
# 
# plot(predict_density ~ density, data = effect_df, log = "xy", 
#      xlim = c(0.03, 26), ylim = c(0.03, 26), col = color[1, 2],
#      ylab = "Predicted density", xlab = "Observed density")
# 
# abline(h = 1, v = 1, lty = 2, col = "lightgrey")
# abline(0,1, lty = 2)
# 
# text(0.2, 20, paste0("R-square value : ", signif(cor(log(effect_df$density[-1]), log(effect_df$predict_density)[-1])^2, digits = 3)))
# 
# dev.off()
# 
# 
# 
# 
