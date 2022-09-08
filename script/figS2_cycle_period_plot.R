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

# Illustrate the cycle dynamic at specific k value
k_vector <- c(5, 3, 2.5, 2, 1/2, 1/2.5, 1/3, 1/5)

# Add points at k value where the curves cross
diagonal <- matrix(rep(NA, 2*length(k_vector)), ncol = 2)
for (i in 1:length(k_vector)) {
  diagonal[i, ] <- find_intersect(x1, x2, y1, y2, k_vector[i])[1:2] # Find interect return R1 and R2 coordinates where it crosses k line and the diagonal
}

#-------------------------------------------------#
# Plot of cycle lenght along the diagonal         #
#-------------------------------------------------#

tikz(file = "results/figS2_cycle_period_plot.tex", width = 8, height = 8, standAlone = TRUE)

par(mfrow = c(1, 1), mar = c(6.1, 5.1, 4.1, 2.1)) 

ab_values <- find_intersect(x1, x2, y1, y2, k_vector[1])[3:4]

R1[1] <- 1.05

R1 <- c(1.01, R1)

R2 <- exp(ab_values[2]) * R1^ab_values[1] # Formula of the diagonal for each R1. R1 and R2 are then coordinate of the diagonal

freq <- matrix(rep(NA, 3003), ncol = 3)

for (i in 1:length(R1)) {
  
  L <- numeric(1500)
  
  L[1] <- 1
  
  Lstar <- 2
  
  for(j in 2:length(L)){
    if (L[j-1] < Lstar) {L[j] <- L[j-1]*R1[i]
    
    } else if (L[j-1] >= Lstar) {L[j] <- L[j-1]*R2[i]} 
  }
  
  maxima <- which(diff(sign(diff(L[100:1500]))) == -2)+1
  freq_plus <- max(diff(maxima))
  freq_min  <- min(diff(maxima))
  freq_mean <- mean(diff(maxima))
  
  freq[i, ] <- c(freq_plus, freq_mean, freq_min)
  
}

plot(freq[, 2] ~ R1, log = "xy", type = "l", lwd = 2,
     ylab =""  , xlab = "",
     yaxt = "n", xaxt = "n",
     xaxs = "i", yaxs = "i", 
     xlim = c(1, 100), ylim = c(1, 500))

axis(1, lwd.ticks = 1, labels = c(1,2,5,10,20,50,100), at = c(1,2,5,10,20,50,100), cex.axis = 1.5)
axis(2, lwd.ticks = 1, labels = c(1,2,5,10,20,50,100,200,500), c(1,2,5,10,20,50,100,200,500), cex.axis = 1.5)


mtext("Mean period", side = 2, line = 3, cex = 2)
mtext("$R_1$ value", side = 1, line = 3, cex = 2)


# Add horizontal line for period length
abline(h = c(3, 3.5, 4, 6), col = grey, lty = 2, lwd = 1.5)


# Add the point on the diagonal in the frequency plot
freq_diag <- numeric(length(k_vector))

for (i in 1:length(freq_diag)) {
  
  L <- numeric(800)
  
  L[1] <- 1
  
  Lstar <- 2
  
  for(j in 2:length(L)){
    if (L[j-1] < Lstar) {L[j] <- L[j-1]*diagonal[i, 1]
    
    } else if (L[j-1] >= Lstar) {L[j] <- L[j-1]*diagonal[i, 2]} 
  }
  
  maxima <- which(diff(sign(diff(L[100:800]))) == -2)+1
  
  freq_mean <- mean(diff(maxima))
  
  freq_diag[i] <- freq_mean
  
}

points(freq_diag ~ diagonal[, 1], col = color, pch = 19, cex = 1.5)

dev.off()
