
#### COMMENT THE SCRIPT

#
#   Create the R2 cumulative plot for the phenomeno approach
#

glm1 <- readRDS("data_clean/glm1_log.RDS")

pred <- data.frame(owl =       c(1,0,0, 1,0,0,0,0,1,1,0),
                   fox_repro = c(0,1,0, 1,0,0,1,0,1,0,1),
                   jaeger =    c(0,0,1, 1,1,0,1,0,1,1,1),
                   weasel =    c(0,0,0, 1,1,0,0,1,0,1,1))

for (i in 1:nrow(pred)) {
  
  pclass <- NULL
  
  if(pred[i, "owl"] == 1) pclass <- paste0(pclass, "O")
  
  if(pred[i, "fox_repro"] == 1) pclass <- paste0(pclass, "F")
  
  if(pred[i, "jaeger"] == 1) pclass <- paste0(pclass, "J")
  
  if(pred[i, "weasel"] == 1) pclass <- paste0(pclass, "W")
  
  if(is.null(pclass)) pclass <- "none"
  
  pred[i, "pred_class"] <- pclass
  
}

pred$pred_class <- factor(pred$pred_class, levels = c("none", "O", "F", "J","W", "JW", "OJW", "FJ", "OFJ", "FJW", "OFJW"))

pred <- pred[order(pred$pred_class), ]


pred[, "pred_growth"] <- exp(predict(glm1, newdata = pred))

saveRDS(pred[, c(5,6)], "data_clean/11_R2_pheno.RDS")

# #--------------------------#
# #   Plot of growth rates   #
# #--------------------------#
# 
# pdf("results/proof_other/regression_plot/glm_log_all_R2_cumulative.pdf", width = 8, height = 6)
# 
# plot.default(pred$pred_growth[-1] ~ pred$pred_class[-1], log = "y", pch = 19, 
#              ylab = "Growth", xlab = "Predation group", axes = FALSE, 
#              ylim = c(min(pred$pred_growth), max(pred$pred_growth)),
#              col = c(rev(viridis::viridis(4)), rep("black", 6)))
# 
# axis(side = 1, at = as.numeric(pred$pred_class), labels = pred$pred_class)
# axis(side = 2)
# 
# box()
# 
# abline(h = 1, lty = 2)
# 
# segments(x0 = as.numeric(pred$pred_class)[-1], y0 = rep(pred$pred_growth[1], 10), y1 = pred$pred_growth[-1],
#          col = c(rev(viridis::viridis(4)), rep("black", 6)))
# 
# points(rep(pred$pred_growth[1], 10) ~ pred$pred_class[-1], pch = 19)
# 
# 
# legend(0.5 ~ 2, legend = c("Owl", "Fox", "Jaeger", "Weasel"),
#        col = rev(viridis::viridis(4)), lwd = 2, bty = "n")
# 
# dev.off()