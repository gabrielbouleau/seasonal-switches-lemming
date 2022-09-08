# library(jsonlite)

# #-----------------------------------------------------------------------------#
# #   Lolipop plot of mechanistic predator effect on lemming growth rate (R2)   #
# #-----------------------------------------------------------------------------#
# 
# ## Couleurs
# color <- fromJSON(txt = "script/Clean_Rdata/colorJSON.json")
# color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)
# color <- rgb(color[,1], color[, 2], color[, 3], maxColorValue = 255)

#-------------#
#  Load data  #
#-------------#

# Minimum lemming growth rate = 0.5
r <- 4.62

pred <- readRDS("data_clean/7_meca_attack_rate.RDS")

Density <- readRDS("data_clean/5_PredMeanDensity.rds")

q <- 0.75


#------------------------------------#
#   Additive growth rate reduction   #
#------------------------------------#

Lem_alone <- exp(r * q) * exp(r * (1 - q))

Owl <- exp(r * q) * exp((r - pred[1, "alpha"] * 2*Density$Owl) * (1 - q))

Fox <- exp((r - pred[7, "alpha"] * Density$Fox) * q) * exp((r - pred[8, "alpha"] * Density$Fox) * (1 - q))

Jaeger <- exp(r * q) * exp((r - pred[2, "alpha"] * 2*Density$Jaeger) * (1 - q))

# Density in high year: 0.4 ind/km2, low year 0.02 ind/km2
Weasel <- exp((r - pred[5, "alpha"] * 0.4/100) * q) * exp((r - pred[3, "alpha"] * 0.4/100) * (1 - q))

JW <- exp((r - pred[5, "alpha"] * 0.4/100) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
                                                            pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
  
OJW <- exp((r - pred[5, "alpha"] * 0.4/100) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
                                                             pred[1, "alpha"] * 2*Density$Owl +
                                                             pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))

FJ <- exp((r - pred[7, "alpha"] * Density$Fox) * q) * exp((r - (pred[8, "alpha"] * Density$Fox +
                                                                pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))

OFJ <- exp((r - pred[7, "alpha"] * Density$Fox) * q) * exp((r - (pred[8, "alpha"] * Density$Fox +
                                                                 pred[2, "alpha"] * 2*Density$Jaeger +
                                                                 pred[1, "alpha"] * 2*Density$Owl)) * (1 - q))

FJW <- exp((r - (pred[5, "alpha"] * 0.4/100 +
                 pred[7, "alpha"] * Density$Fox)) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
                                                                   pred[8, "alpha"] * Density$Fox +
                                                                   pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
OFJW <- exp((r - (pred[5, "alpha"] * 0.4/100 +
                    pred[7, "alpha"] * Density$Fox)) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
                                                                        pred[8, "alpha"] * Density$Fox +
                                                                        pred[1, "alpha"] * 2*Density$Owl +
                                                                        pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
# Value of R1 with weasel and fox not repro
R1_FW <- exp((r - (pred[5, "alpha"] * 0.02/100 +
                   pred[7, "alpha"] * Density$Fox)) * q) * exp((r - (pred[4, "alpha"] * 0.02/100 +
                                                                     pred[6, "alpha"] * Density$Fox)) * (1 - q))

R1_F <- exp((r - pred[7, "alpha"] * Density$Fox) * q) * exp((r - pred[6, "alpha"] * Density$Fox) * (1 - q))

R1_W <-exp((r - pred[5, "alpha"] * 0.02/100) * q) * exp((r - pred[4, "alpha"] * 0.02/100) * (1 - q))


R2_meca <- list(Lem_alone = Lem_alone, Owl = Owl, Fox = Fox, Jaeger = Jaeger, Weasel = Weasel, 
                JW = JW, OJW = OJW, FJ = FJ, OFJ = OFJ, FJW = FJW, OFJW = OFJW, R1_FW = R1_FW,
                R1_F = R1_F, R1_W = R1_W)

saveRDS(R2_meca, "data_clean/7_R2_meca.RDS")

#####
# Jaeger_Fox <- exp((r - pred[6, "alpha"] * Density$Fox) * q) * exp((r - (pred[7, "alpha"] * Density$Fox +
#                                                                         pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
# 
# Jaeger_Fox_Owl <- exp((r - pred[6, "alpha"] * Density$Fox) * q) * exp((r - (pred[7, "alpha"] * Density$Fox +
#                                                                             pred[2, "alpha"] * 2*Density$Jaeger +
#                                                                             pred[1, "alpha"] * 2*Density$Owl)) * (1 - q))
# 
# Fox_Owl <- exp((r - pred[6, "alpha"] * Density$Fox) * q) * exp((r - (pred[7, "alpha"] * Density$Fox +
#                                                                      pred[1, "alpha"] * 2*Density$Owl)) * (1 - q))
# 
# Jaeger_Owl <- exp(r * q) * exp((r - (pred[1, "alpha"] * 2*Density$Owl +
#                                      pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
# 
# Weasel_Owl <- exp((r - pred[4, "alpha"] * 0.4/100) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
#                                                                     pred[1, "alpha"] * 2*Density$Owl)) * (1 - q))
# 
# Weasel_Jaeger <- exp((r - pred[4, "alpha"] * 0.4/100) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
#                                                                        pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
# 
# Weasel_Fox <- exp((r - (pred[4, "alpha"] * 0.4/100 +
#                         pred[6, "alpha"] * Density$Fox)) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
#                                                                           pred[7, "alpha"] * Density$Fox)) * (1 - q))
# 
# Weasel_Owl_Jaeger <- exp((r - pred[4, "alpha"] * 0.4/100) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
#                                                                            pred[1, "alpha"] * 2*Density$Owl +
#                                                                            pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
# 
# Weasel_Fox_Jaeger <- exp((r - (pred[4, "alpha"] * 0.4/100 +
#                                pred[6, "alpha"] * Density$Fox)) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
#                                                                                  pred[7, "alpha"] * Density$Fox +
#                                                                                  pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
# 
# Weasel_Fox_Owl <- exp((r - (pred[4, "alpha"] * 0.4/100 +
#                             pred[6, "alpha"] * Density$Fox)) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
#                                                                               pred[7, "alpha"] * Density$Fox +
#                                                                               pred[1, "alpha"] * 2*Density$Owl)) * (1 - q))
# 
# Weasel_Fox_Owl_Jaeger <- exp((r - (pred[4, "alpha"] * 0.4/100 +
#                                    pred[6, "alpha"] * Density$Fox)) * q) * exp((r - (pred[3, "alpha"] * 0.4/100 +
#                                                                                      pred[7, "alpha"] * Density$Fox +
#                                                                                      pred[1, "alpha"] * 2*Density$Owl +
#                                                                                      pred[2, "alpha"] * 2*Density$Jaeger)) * (1 - q))
#####
# 
# #--------------------------#
# #   Plot of growth rates   #
# #--------------------------#
# 
# pdf("results/clean_results/7_R2_cumulative_plot.pdf", width = 8, height = 6)
# 
# plot(NULL, xlim=c(0, 9), ylim=c(0.1 , Lem_alone + 20), xaxt = "n", 
#      xlab = "Predator groups", ylab = "R2 value", log = "y")
# 
# # Line of R2 = 1, below this point cycle happens
# abline(h = 1, lty = 2)
# 
# 
# points(0, Owl, pch = 19, col = color[4])
# segments(x0 = 0, y0 = Owl,  y1 = Lem_alone, col = color[4])
# 
# 
# points(1, Fox, pch = 19, col = color[5])
# segments(x0 = 1, y0 = Fox, y1 = R1_F, col = color[5])
# 
# 
# points(2, Jaeger, pch = 19, col = color[6])
# segments(x0 = 2, y0 = Jaeger,  y1 = Lem_alone, col = color[6])
# 
# 
# points(3, Weasel, pch = 19, col = color[7])
# segments(x0 = 3, y0 = Weasel, y1 = R1_W, col = color[7])
# 
# 
# points(4, JW, pch = 19, col = "black")
# segments(x0 = 4, y0 = JW,  y1 = R1_W, col = "black")
# 
# 
# points(5, OJW, pch = 19, col = "black")
# segments(x0 = 5, y0 = OJW, y1 = R1_W, col = "black")
# 
# 
# points(6, FJ, pch = 19, col = "black")
# segments(x0 = 6, y0 = FJ,  y1 = R1_F, col = "black")
# 
# 
# points(7, OFJ, pch = 19, col = "black")
# segments(x0 = 7, y0 = OFJ, y1 = R1_F, col = "black")
# 
# 
# points(8, FJW, pch = 19, col = "black")
# segments(x0 = 8, y0 = FJW,  y1 = R1_FW, col = "black")
# 
# 
# points(9, OFJW, pch = 19, col = "black")
# segments(x0 = 9, y0 = OFJW, y1 = R1_FW, col = "black")
# 
# 
# #####
# # points(4, Jaeger_Fox, pch = 19, col = color[5])
# # segments(4, Jaeger_Fox, y1 = Jaeger, col = color[5])
# # points(4, Jaeger, pch = 19, col = color[6])
# # segments(4, Jaeger, y1 = Lem_alone, col = color[6])
# # 
# # 
# # points(5, Fox_Owl, pch = 19, col = color[4])
# # segments(5, Fox_Owl, y1 = Fox, col = color[4])
# # points(5, Fox, pch = 19, col = color[5])
# # segments(5, Fox, y1 = Lem_alone, col = color[5])
# # 
# # 
# # points(6, Weasel_Fox, pch = 19, col = color[7])
# # segments(6, Weasel_Fox, y1 = Fox, col = color[7])
# # points(6, Fox, pch = 19, col = color[5])
# # segments(6, Fox, y1 = Lem_alone, col = color[5])
# # 
# # 
# # points(7, Jaeger_Owl, pch = 19, col = color[4])
# # segments(7, Jaeger_Owl, y1 = Jaeger, col = color[4])
# # points(7, Jaeger, pch = 19, col = color[6])
# # segments(7, Jaeger, y1 = Lem_alone, col = color[6])
# # 
# # 
# # points(8, Weasel_Owl, pch = 19, col = color[7])
# # segments(8, Weasel_Owl, y1 = Owl, col = color[7])
# # points(8, Owl, pch = 19, col = color[4])
# # segments(8, Owl, y1 = Lem_alone, col = color[4])
# # 
# # 
# # points(9, Weasel_Jaeger, pch = 19, col = color[7])
# # segments(9, Weasel_Jaeger, y1 = Jaeger, col = color[7])
# # points(9, Jaeger, pch = 19, col = color[6])
# # segments(9, Jaeger, y1 = Lem_alone, col = color[6])
# # 
# # 
# # points(10, Jaeger_Fox_Owl, pch = 19, col = color[4])
# # segments(10, Jaeger_Fox_Owl, y1 = Jaeger_Fox, col = color[4])
# # points(10, Jaeger_Fox, pch = 19, col = color[5])
# # segments(10, Jaeger_Fox, y1 = Jaeger, col = color[5])
# # points(10, Jaeger, pch = 19, col = color[6])
# # segments(10, Jaeger, y1 = Lem_alone, col = color[6])
# # 
# # 
# # points(11, Weasel_Fox_Owl, pch = 19, col = color[7])
# # segments(11, Weasel_Fox_Owl, y1 = Fox_Owl, col = color[7])
# # points(11, Fox_Owl, pch = 19, col = color[4])
# # segments(11, Fox_Owl, y1 = Fox, col = color[4])
# # points(11, Fox, pch = 19, col = color[5])
# # segments(11, Fox, y1 = Lem_alone, col = color[5])
# # 
# # 
# # points(12, Weasel_Fox_Jaeger, pch = 19, col = color[7])
# # segments(12, Weasel_Fox_Jaeger, y1 = Jaeger_Fox, col = color[7])
# # points(12, Jaeger_Fox, pch = 19, col = color[5])
# # segments(12, Jaeger_Fox, y1 = Jaeger, col = color[5])
# # points(12, Jaeger, pch = 19, col = color[6])
# # segments(12, Jaeger, y1 = Lem_alone, col = color[6])
# # 
# # 
# # points(13, Weasel_Owl_Jaeger, pch = 19, col = color[7])
# # segments(13, Weasel_Owl_Jaeger, y1 = Jaeger_Owl, col = color[7])
# # points(13, Jaeger_Owl, pch = 19, col = color[4])
# # segments(13, Jaeger_Owl, y1 = Jaeger, col = color[4])
# # points(13, Jaeger, pch = 19, col = color[6])
# # segments(13, Jaeger, y1 = Lem_alone, col = color[6])
# # 
# # 
# # points(14, Weasel_Fox_Owl_Jaeger, pch = 19, col = color[7])
# # segments(14, Weasel_Fox_Owl_Jaeger, y1 = Jaeger_Fox_Owl, col = color[7])
# # points(14, Jaeger_Fox_Owl, pch = 19, col = color[4])
# # segments(14, Jaeger_Fox_Owl, y1 = Jaeger_Fox, col = color[4])
# # points(14, Jaeger_Fox, pch = 19, col = color[5])
# # segments(14, Jaeger_Fox, y1 = Jaeger, col = color[5])
# # points(14, Jaeger, pch = 19, col = color[6])
# # segments(14, Jaeger, y1 = Lem_alone, col = color[6])
# #####
# 
# R1_vector <- c(Lem_alone, R1_F, Lem_alone, R1_W, R1_W, R1_W, R1_F, R1_F, R1_FW, R1_FW)
# 
# points(R1_vector ~ c(0:9) , pch = 19)
# 
# legend("bottomleft", legend = c("Owl", "Fox", "Jaeger", "Weasel"),
#        col = color[4:7], lwd = 2, bty = "n")
# 
# text(3, 0.5, "Negative \ngrowth", font = 2)
# text(3, 1.8, "Positive \ngrowth", font = 2)
# 
# text(5, Lem_alone + 30, "R1 values", font = 2)
# 
# axis(1, at = c(0:9), labels = c("O", "F", "J","W", "JW", "OJW", "FJ", "OFJ", "FJW", "OFJW"))
# 
# dev.off()
