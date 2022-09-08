library(jsonlite)
library(tidyverse)
library(tikzDevice)

source("script/fct/find_intersect_fct.R") # Function to find a, b parameters of a diagonal. Also find coordinate where it cross the R1 R2 curve

## Couleurs
color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)
color <- rgb(color[,1], color[, 2], color[, 3], maxColorValue = 255)

grey <- color[2]

color <- c(rev(color[4:7]), color[4:7])

# X and Y coordinates for the diagonal line
x1 <- 1
x2 <- 100
y1 <- 0.01
y2 <- 1


# R1, R2 and k values for the first plot
R1 <- seq(1, 100, length.out = 1000)
k <- c(15:2, 1/c(1:15))
R2 <- 1 / R1^k[1]
Lstar <- 2

tikz(file = "results/fig2_one_pred_phase_plot.tex", width = 10, height = 5, standAlone = TRUE)

#---------------------------------------#
# R2 vs R1 plot for different K value   #
#---------------------------------------#

# Set layout and margin
layout(matrix(c(5,5,1,1,1,1,6,6,
                4,4,1,1,1,1,7,7,
                3,3,1,1,1,1,8,8,
                2,2,1,1,1,1,9,9), 4, 8, TRUE))


par(mar = c(4,4,1.5,1), mgp = c(2.5, 1, 0))

# This plot the R1 and R2 values for K = 1
plot(R2 ~ R1, type = "l", log = "xy",
     ylim = c(0.01, 1), xlim = c(1, 100),
     yaxs = "i", xaxs = "i",
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "")

axis(1, cex.axis = 1.3)
axis(2, cex.axis = 1.3)

mtext("$R_2$", side = 2, line = 2.2, cex = 1.6)
mtext("$R_1$", side = 1, line = 2.6, cex = 1.6)

# Find other R1-R2 coordinates for other k
for(i in 2:length(k)){
  R2 <- 1 / R1^k[i]
  
  if(k[i] == 1){ ltype <- 2
  } else {ltype <- 1}
  
  if(k[i] < 1){lcolor <- grey
  } else {lcolor <- "black"}
  
  lines(R2 ~ R1, type = "l", lwd = 1.5, 
        lty = ltype, col = lcolor)
}

# Plot diagonal line
lines(x = c(x1, x2), y = c(y1, y2), lwd = 4)

# Illustrate the cycle dynamic at specific k value
k_vector <- c(5, 3, 2.5, 2, 1/2, 1/2.5, 1/3, 1/5)

# Add points at k value where the curves cross
diagonal <- matrix(rep(NA, 2*length(k_vector)), ncol = 2)
for (i in 1:length(k_vector)) {
  diagonal[i, ] <- find_intersect(x1, x2, y1, y2, k_vector[i])[1:2] # Find interect return R1 and R2 coordinates where it crosses k line and the diagonal
}
points(diagonal[,1], diagonal[, 2], pch = 19, cex = 1.5, col = color)



#---------------------------------------#
# Time series at different k values     #
#---------------------------------------#

# Plot cycle dynamic at different k value
par(mar = c(1.1,1.1,1.5,0.5))

k_vector_name <- c(5, 3, 2.5, 2, "1/2", "1/2.5", "1/3", "1/5")

for (i in 1:length(k_vector)) {
  
  L <- numeric(800)
  
  L[1] <- 1
  
  R_1 <- diagonal[i, 1] # Select the R1-R2 value at intersection
  
  R_2 <- diagonal[i, 2]
  
  for(j in 2:length(L)){
    if (L[j-1] < Lstar) {L[j] <- L[j-1]*R_1
    
    } else if (L[j-1] >= Lstar) {L[j] <- L[j-1]*R_2} 
  }
  
  if(i >= 5 ){lcolor <- grey
  } else {lcolor <- "black"}
  
  plot(L[23:43], type = "l", yaxt = "n", xaxt = "n", main = paste("k =", k_vector_name[i]), col = lcolor, cex.main = 1.5)
  points(L[23:43], pch = 19, col = color[i])
  abline(h = 2, lty = 2)

  if (i == 1){
    text(2, 1.8, "$L^*$", cex = 1.5, font = 2)
    mtext("years", side = 1)
    mtext("density", side = 2)}
  
}

dev.off()

# #-------------------------------------------------#
# # Plot of cycle lenght along the diagonal         #
# #-------------------------------------------------#
# 
# pdf(file = "paper_fig/supp1_1_period_plot.pdf", width = 8, height = 8)
# 
# par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1)) 
# 
# ab_values <- find_intersect(x1, x2, y1, y2, k_vector[1])[3:4]
# 
# R1[1] <- 1.05
# 
# R1 <- c(1.01, R1)
# 
# R2 <- exp(ab_values[2]) * R1^ab_values[1] # Formula of the diagonal for each R1. R1 and R2 are then coordinate of the diagonal
# 
# freq <- matrix(rep(NA, 3003), ncol = 3)
# 
# for (i in 1:length(R1)) {
#   
#   L <- numeric(1500)
#   
#   L[1] <- 1
#   
#   Lstar <- 2
#   
#   for(j in 2:length(L)){
#     if (L[j-1] < Lstar) {L[j] <- L[j-1]*R1[i]
#     
#     } else if (L[j-1] >= Lstar) {L[j] <- L[j-1]*R2[i]} 
#   }
#   
#   maxima <- which(diff(sign(diff(L[100:1500]))) == -2)+1
#   freq_plus <- max(diff(maxima))
#   freq_min  <- min(diff(maxima))
#   freq_mean <- mean(diff(maxima))
#   
#   freq[i, ] <- c(freq_plus, freq_mean, freq_min)
#   
# }
# 
# plot(freq[, 2] ~ R1, log = "xy", type = "l", lwd = 2,
#      ylab = "Mean period", xlab = "R1 value",
#      xaxs = "i", yaxs = "i", 
#      xlim = c(1, 100), ylim = c(1, 500))
# 
# 
# # Add horizontal line for period length
# 
# abline(h = c(3, 3.5, 4, 6), col = grey, lty = 2, lwd = 1.5)
# 
# 
# # Add the point on the diagonal in the frequency plot
# 
# freq_diag <- numeric(length(k_vector))
# 
# for (i in 1:length(freq_diag)) {
# 
#   L <- numeric(800)
#   
#   L[1] <- 1
#   
#   Lstar <- 2
#   
#   for(j in 2:length(L)){
#     if (L[j-1] < Lstar) {L[j] <- L[j-1]*diagonal[i, 1]
#     
#     } else if (L[j-1] >= Lstar) {L[j] <- L[j-1]*diagonal[i, 2]} 
#   }
#   
#   maxima <- which(diff(sign(diff(L[100:800]))) == -2)+1
# 
#   freq_mean <- mean(diff(maxima))
#   
#   freq_diag[i] <- freq_mean
#     
# }
# 
# points(freq_diag ~ diagonal[, 1], col = color, pch = 19, cex = 1.5)
# 
# dev.off()

