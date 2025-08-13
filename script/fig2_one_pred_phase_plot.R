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

tikz(file = "fig2_one_pred_phase_plot.tex", width = 10, height = 5, standAlone = TRUE)

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

mtext("$R_1$", side = 2, line = 2.2, cex = 1.6)
mtext("$R_0$", side = 1, line = 2.6, cex = 1.6)

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
k_vector <- c(5, 10/3, 2.5, 2, 1/2, 1/2.5, 1/(10/3), 1/5)

# Add points at k value where the curves cross
diagonal <- matrix(rep(NA, 2*length(k_vector)), ncol = 2)
for (i in 1:length(k_vector)) {
  diagonal[i, ] <- find_intersect(x1, x2, y1, y2, k_vector[i])[1:2] # Find interect return R1 and R2 coordinates where it crosses k line and the diagonal
}
points(diagonal[,1], diagonal[, 2], pch = 19, cex = 1.5, col = color)

# labels the diagonals
text(x = c(3.8, 8, 75, 80, 80), 
     y = c(0.011, 0.011, 0.011, 0.095, 0.205), 
     labels = c("$k=3$", "$k=2$", "$k=1$", "$1/k=1/2$", "$1/k=1/3$"))

#---------------------------------------#
# Time series at different k values     #
#---------------------------------------#

# Plot cycle dynamic at different k value
par(mar = c(1.1,1.1,1.5,0.5))

k_vector_name <- c(5, 3, 2, 2, "1/2", "1/2", "1/3", "1/5")
P_vector_name <- c(6, "$4.\\overline{3}$", 3.5, 3, 3, 3.5, "$4.\\overline{3}$", 6)

for (i in 1:length(k_vector)) {
  
  L <- numeric(800)
  
  L[1] <- 1
  
  R_1 <- diagonal[i, 1] # Select the R1-R2 value at intersection
  
  R_2 <- diagonal[i, 2]
  
  for(j in 2:length(L)){
    if (L[j-1] < Lstar) {L[j] <- L[j-1]*R_1
    
    } else if (L[j-1] >= Lstar) {L[j] <- L[j-1]*R_2} 
  }
  
  if(i >= 5 ){
    lcolor <- grey
    
    plot(L[23:43], type = "l", yaxt = "n", xaxt = "n", main = paste0("P = ", P_vector_name[i], ", 1/k = ", k_vector_name[i]), col = lcolor, cex.main = 1.5)
    points(L[23:43], pch = 19, col = color[i])
    abline(h = 2, lty = 2)
  
  } else {
    lcolor <- "black"
    
    plot(L[23:43], type = "l", yaxt = "n", xaxt = "n", main = paste0("P = ", P_vector_name[i], ", k = ", k_vector_name[i]), col = lcolor, cex.main = 1.5)
    points(L[23:43], pch = 19, col = color[i])
    abline(h = 2, lty = 2)
    }

  if (i == 1){
    text(2, 1.8, "$L^*$", cex = 1.5, font = 2)
    mtext("years", side = 1)
    mtext("density", side = 2)}
  
}

dev.off()

