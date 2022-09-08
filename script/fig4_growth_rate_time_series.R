library(tikzDevice)
library(jsonlite)
library(tidyverse)

color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(2,4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)

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


#-----------------------------------------#
#   Merge predator and predator_infered   #
#-----------------------------------------#

predator <- predator[which(predator$Season == "S"), ]

predator[1:nrow(predator_infered$fox), "fox_repro"] <- predator_infered$fox$fox_repro
predator[1:nrow(predator_infered$jaeger), "jaeger"] <- predator_infered$jaeger$jaeger


predator <- left_join(predator, Growth[, -2], by = "year")

predator <- predator[-nrow(predator), ]


#---------------------------------#
#   Predict mechanistic growth    #
#---------------------------------#

meca <- readRDS("data_clean/7_R2_meca.RDS")

# Add growth rate prediction
predator[, "Growth_meca"] <- 0

for (i in 1:nrow(predator)) {
  
  if(predator[i, "fox_repro"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        if(predator[i, "weasel"] == 1){
          
          predator[i, "Growth_meca"] <- meca$OFJW
          
        } else { predator[i, "Growth_meca"] <- meca$OFJ }
        
      } else if (predator[i, "weasel"] == 1){
        
        predator[i, "Growth_meca"] <- meca$FJW
        
      } else { predator[i, "Growth_meca"] <- meca$FJ}
      
    } else { predator[i, "Growth_meca"] <- meca$Fox}
    
  } else if(predator[i, "weasel"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        predator[i, "Growth_meca"] <- meca$OJW
        
      } else { predator[i, "Growth_meca"] <- meca$JW}
      
    } else { predator[i, "Growth_meca"] <- meca$Weasel}
    
  } else if (predator[i, "owl"] == 1){
    
    predator[i, "Growth_meca"] <- meca$Owl
    
  } else if (predator[i, "jaeger"] == 1){
    
    predator[i, "Growth_meca"] <- meca$Jaeger
    
  } else {predator[i, "Growth_meca"] <- meca$R1_FW}
}


#-----------------------------------#
#   Predict phenomenologic growth   #
#-----------------------------------#

pheno <- readRDS("data_clean/glm1_log.RDS")

predator[, "Growth_pheno"] <- predict(pheno, newdata = predator, type = "response")


#---------------------------------#
#   Time-serie comparison plot    #
#---------------------------------#

tikz("results/fig4_growth_rate_time_series.tex", height = 5, width = 10, standAlone = TRUE)

# Set layout and margin
layout(matrix(c(1,1,1,2,
                1,1,1,3,
                1,1,1,4), 3, 4, TRUE))

par(mar = c(6.1, 6.1, 4.1, 0), mgp = c(3, 1.5, 0))

plot(Growth ~ year, data = Growth[-nrow(Growth),], 
     pch = 19, type = "b", 
     ylab = NA, xlab = NA, 
     lwd = 2, 
     ylim = c(-4.5, 5),
     xaxt = "n", yaxt = "n")

axis(1, at = seq(1993, 2020, 2), cex.axis = 1.5)
axis(2, at = c(-4, -2, 0, 2, 4), cex.axis = 1.5)

mtext("Exponential growth rate", 2, cex = 2, line = 3.5)
mtext("Year", 1, cex = 2, line = 4)

abline(h = 0, lty = 2, col = "grey", lwd = 2.5)

points(log(Growth_meca) ~ year, data = predator, col = color[3], type = "b", lwd = 1.2, pch = 0)

points(log(Growth_pheno) ~ year, data = predator, col = color[4], type = "b", lwd = 1.2, pch = 1)

legend("bottom", legend = c( "Empirical", "Mechanistic", "Phenomenologic"), ncol = 3, cex = 2,
       pt.bg = 'white', lty = 1, lwd = c(2, 1.2, 1.2), bty = "n", pch = c(19, 22, 21), col = c("black", color[3:4]))

#----------------------------#
#   TEST AREA : Phase plot   #
#----------------------------#

par(mgp = c(3, 1, 0))

col.transp <- function(color, n){
  
  col <- col2rgb(color)[,1]
  
  range.transp <- seq(10, 255, length.out = n) #35
  
  col.vec <- c()
  
  for (i in 1:n) {
    col.vec <- c(col.vec, rgb(col[1], col[2], col[3], range.transp[i], max = 255))
  }
  
  return(col.vec)
}

col.mec <- col.transp(color[3], n = (nrow(predator) - 1))
col.emp <- col.transp("black", n = (nrow(predator) - 1))
col.phe <- col.transp(color[4], n = (nrow(predator) - 1))

# Mechanistic phase plot
par(mar = c(0, 7.1, 4.1, 4.1))

plot(10,
     xlim = c(-5, 5), ylim = c(-5, 5),
     xlab = NA, ylab = NA, xaxt = "n", cex.axis = 1.2)

abline(h = 0, v = 0, lty = 2, col = "grey")

segments(x0 = head(log(predator$Growth_meca[-nrow(predator)]), -1),
         y0 = head(log(predator$Growth_meca[-1]), -1),
         x1 = tail(log(predator$Growth_meca[-nrow(predator)]), -1),
         y1 = tail(log(predator$Growth_meca[-1]), -1),
         lwd = 2, col = col.mec)

# points(log(predator$Growth_meca[-1]) ~ log(predator$Growth_meca[-nrow(predator)]), cex = 3, col = "white", pch = 19)
points(log(predator$Growth_meca[-1]) ~ log(predator$Growth_meca[-nrow(predator)]), cex = 1.5, col = col.mec, pch = 19)


# Empirical phase plot
par(mar = c(2, 7.1, 2, 4.1))

plot(10,
     xlim = c(-5, 5), ylim = c(-5, 5),
     xlab = NA, ylab = "$t+1$", xaxt = "n", cex.axis = 1.2, cex.lab = 2)

abline(h = 0, v = 0, lty = 2, col = "grey")

segments(x0 = head(predator$Growth[-nrow(predator)], -1),
         y0 = head(predator$Growth[-1], -1),
         x1 = tail(predator$Growth[-nrow(predator)], -1),
         y1 = tail(predator$Growth[-1], -1),
         lwd = 2, col = col.emp)

# points(predator$Growth[-1] ~ predator$Growth[-nrow(predator)], cex = 3, col = "white", pch = 19)
points(predator$Growth[-1] ~ predator$Growth[-nrow(predator)], cex = 1.5, col = col.emp, pch = 19)


# Phenomenologic phase plot
par(mar = c(4.1, 7.1, 0, 4.1))

plot(10,
     xlim = c(-5, 5), ylim = c(-5, 5),
     xlab = "$t$", ylab = NA, cex.axis = 1.2, cex.lab = 2)

abline(h = 0, v = 0, lty = 2, col = "grey")

segments(x0 = head(log(predator$Growth_pheno[-nrow(predator)]), -1),
         y0 = head(log(predator$Growth_pheno[-1]), -1),
         x1 = tail(log(predator$Growth_pheno[-nrow(predator)]), -1),
         y1 = tail(log(predator$Growth_pheno[-1]), -1),
         lwd = 2, col = col.phe)

# points(log(predator$Growth_pheno[-1]) ~ log(predator$Growth_pheno[-nrow(predator)]), cex = 3, col = "white", pch = 19)
points(log(predator$Growth_pheno[-1]) ~ log(predator$Growth_pheno[-nrow(predator)]), cex = 1.5, col = col.phe, pch = 19)

dev.off()
