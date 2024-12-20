library(tikzDevice)
library(jsonlite)
library(tidyverse)

color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(2,4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)

#-------------------#
#   Predator data   #
#-------------------#
predator <- readRDS("data_clean/2_PredatorPresence.rds")

predator_infered <- readRDS("data_clean/3_infered_predator_binom.rds")


#------------------#
#   Lemming data   #
#------------------#
lemmingDensity <- readRDS("data_clean/1_lemmingMeanDensity.rds")[, c(1,2)]

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

meca <- readRDS("data_clean/5_R2_meca.RDS")
meca_se <- readRDS("data_clean/5_R2_var_meca.RDS")

# Add growth rate prediction + confidence interval
predator[, "meca_fit"] <- 0
predator[, "meca_se"] <- 0

for (i in 1:nrow(predator)) {
  
  if(predator[i, "fox_repro"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        if(predator[i, "weasel"] == 1){
          
          predator[i, "meca_fit"] <- meca$OFJW
          predator[i, "meca_se"] <- meca_se$OFJW
          
        } else { 
          predator[i, "meca_fit"] <- meca$OFJ
          predator[i, "meca_se"] <- meca_se$OFJ
          }
        
      } else if (predator[i, "weasel"] == 1){
        
        predator[i, "meca_fit"] <- meca$FJW
        predator[i, "meca_se"] <- meca_se$FJW
        
      } else { 
        predator[i, "meca_fit"] <- meca$FJ
        predator[i, "meca_se"] <- meca_se$FJ
        }
      
    } else { 
      predator[i, "meca_fit"] <- meca$Fox
      predator[i, "meca_se"] <- meca_se$Fox
      }
    
  } else if(predator[i, "weasel"] == 1){
    
    if(predator[i, "jaeger"] == 1){
      
      if(predator[i, "owl"] == 1){
        
        predator[i, "meca_fit"] <- meca$OJW
        predator[i, "meca_se"] <- meca_se$OJW
        
      } else { 
        predator[i, "meca_fit"] <- meca$JW
        predator[i, "meca_se"] <- meca_se$JW
        }
      
    } else { 
      predator[i, "meca_fit"] <- meca$Weasel
      predator[i, "meca_se"] <- meca_se$Weasel}
    
  } else if (predator[i, "owl"] == 1){
    
    predator[i, "meca_fit"] <- meca$Owl
    
  } else if (predator[i, "jaeger"] == 1){
    
    predator[i, "meca_fit"] <- meca$Jaeger
    
  } else {
    predator[i, "meca_fit"] <- meca$R1_FW
    predator[i, "meca_se"] <- meca_se$Lem_alone}
}

predator <- mutate(predator,
                   meca_fit = log(meca_fit))

predator <- mutate(predator,
                   meca_upr = meca_fit + (1.96 * meca_se),
                   meca_lwr = meca_fit - (1.96 * meca_se))

#-----------------------------------#
#   Predict phenomenologic growth   #
#-----------------------------------#

pheno <- readRDS("data_clean/4_glm_log.RDS")

ilink <- family(pheno)$linkinv

predator <- bind_cols(predator, setNames(as_tibble(predict(pheno, predator, se.fit = TRUE)[1:2]), 
                                         c("pheno_fit_link", "pheno_se_link")))

predator <- mutate(predator,
                   pheno_upr = log(ilink(pheno_fit_link + (1.96 * pheno_se_link))),
                   pheno_lwr = log(ilink(pheno_fit_link - (1.96 * pheno_se_link))))


#---------------------------------#
#   Time-serie comparison plot    #
#---------------------------------#

tikz("fig5_growth_rate_time_series.tex", height = 5, width = 10, standAlone = TRUE)

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

points(meca_fit ~ year, data = predator, col = color[3], type = "b", lwd = 1.2, pch = 0)

polygon(x = c(predator$year, rev(predator$year)),
        y = c(predator$ meca_upr, rev(predator$meca_lwr)), 
        col = adjustcolor(color[3], alpha = 0.20), border = NA)

points(pheno_fit_link ~ year, data = predator, col = color[4], type = "b", lwd = 1.2, pch = 1)

polygon(x = c(predator$year, rev(predator$year)),
        y = c(predator$ pheno_upr, rev(predator$pheno_lwr)), 
        col = adjustcolor(color[4], alpha = 0.20), border = NA)

legend("bottom", legend = c( "Empirical", "Mechanistic", "Phenomenologic"), ncol = 3, cex = 2,
       pt.bg = 'white', lty = 1, lwd = c(2, 1.2, 1.2), bty = "n", pch = c(19, 22, 21), col = c("black", color[3:4]))

#----------------#
#   Phase plot   #
#----------------#

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

segments(x0 = head(predator$meca_fit[-nrow(predator)], -1),
         y0 = head(predator$meca_fit[-1], -1),
         x1 = tail(predator$meca_fit[-nrow(predator)], -1),
         y1 = tail(predator$meca_fit[-1], -1),
         lwd = 2, col = col.mec)

# points(log(predator$meca_fit[-1]) ~ log(predator$meca_fit[-nrow(predator)]), cex = 3, col = "white", pch = 19)
points(predator$meca_fit[-1] ~ predator$meca_fit[-nrow(predator)], cex = 1.5, col = col.mec, pch = 19)


# Empirical phase plot
par(mar = c(2, 7.1, 2, 4.1))

plot(10,
     xlim = c(-5, 5), ylim = c(-5, 5),
     xlab = NA, ylab = "Growth rate\nyear $t+1$", xaxt = "n", cex.axis = 1.2, cex.lab = 2)

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
     xlab = "year $t$", ylab = NA, cex.axis = 1.2, cex.lab = 2)

abline(h = 0, v = 0, lty = 2, col = "grey")

segments(x0 = head(predator$pheno_fit_link[-nrow(predator)], -1),
         y0 = head(predator$pheno_fit_link[-1], -1),
         x1 = tail(predator$pheno_fit_link[-nrow(predator)], -1),
         y1 = tail(predator$pheno_fit_link[-1], -1),
         lwd = 2, col = col.phe)

# points(log(predator$Growth_pheno[-1]) ~ log(predator$Growth_pheno[-nrow(predator)]), cex = 3, col = "white", pch = 19)
points(predator$pheno_fit_link[-1] ~ predator$pheno_fit_link[-nrow(predator)], cex = 1.5, col = col.phe, pch = 19)

dev.off()
