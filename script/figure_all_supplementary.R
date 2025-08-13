library(jsonlite)
library(tidyverse)
library(tikzDevice)
library(data.table)
library(MASS)
library(magrittr)

###################################
#                                 #
#   Supplementary files figures   #
#                                 #
###################################


##############################
#                            #
#   Figure S1 cycle period   #
#                            #
##############################

#------------#
#   Colors   #
#------------#
color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)
color <- rgb(color[,1], color[, 2], color[, 3], maxColorValue = 255)

grey <- color[2]

color <- c(rev(color[4:7]), color[4:7])

source("script/fct/find_intersect_fct.R") # Function to find a, b parameters of a diagonal. Also find coordinate where it cross the R1 R2 curve

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

tikz(file = "figS2_cycle_period_plot.tex", width = 8, height = 8, standAlone = TRUE)

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
mtext("$R_0$ value", side = 1, line = 3, cex = 2)


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


##################################
#                                #
#   Figure S3 Consumption rate   #
#                                #
##################################

color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)

lem_mass <- readRDS("data_clean/1_lemmingMass.rds")

#----------------------#
#  Load predator data  #
#----------------------#

Density <- readRDS("data_clean/2_PredMeanDensity.rds")

pred <- readRDS("data_clean/5_meca_attack_rate.RDS")
pred[, "Q_d"] <- pred$Q_B * pred$d / lem_mass # Lemming eaten by predator mass
pred[, "Q_density"] <- pred$alpha * c(Density$Owl, Density$Jaeger, 0.4/100, 0.02/100, 0.4/100, rep(Density$Fox, 3)) # Lemming eaten by population

lemming <- c(0:10)


#---------------------------#
#  Load predator threshold  #
#---------------------------#

threshold <- readRDS("data_clean/3_predator_threshold.rds")

fox_thresh <- seq(threshold$fox$mu, 10, length.out = 10)
fox_low <- seq(0, threshold$fox$mu, length.out = 10)
owl_thresh <- seq(threshold$owl$mu, 10, length.out = 10)
jaeger_thresh <- seq(threshold$jaeger$mu, 10, length.out = 10)


#------------------------------------#
#  Plot monthly lemming consumption  #
#------------------------------------#
tikz("figS3_consumption_rate.tex", height = 10, width = 10, standAlone = TRUE)

par(mfrow = c(2,2), mar = c(7.65, 7.65, 6.15, 3.15), mgp = c(3, 1.5, 0))

# Lemming mass consumption per mass of predator
plot(pred[8, "Q_d"]*fox_thresh ~ fox_thresh, type = "l",
     ylab = NA, xlab = NA,
     col = color[2], lwd = 4, cex.axis = 2,
     ylim = c(0, 50), xlim = c(0, 10))

mtext("Annual lemming consumption per predator mass\n(Lemming/g)", 2, cex = 1.5, line = 4)
mtext("Lemming density\n(ind/ha)", 1, cex = 1.5, line = 5)

lines(pred[6, "Q_d"]*fox_low ~ fox_low, col = color[2], lwd = 2)

lines(pred[7, "Q_d"]*lemming ~ lemming, col = color[2], lty = 2, lwd = 2)

lines(pred[1, "Q_d"]*owl_thresh ~ owl_thresh, col = color[1], lwd = 4)

lines(pred[2, "Q_d"]*jaeger_thresh ~ jaeger_thresh, col = color[3], lwd = 4)

lines(pred[3, "Q_d"]*lemming ~ lemming, col = color[4], lwd = 4)

lines(pred[4, "Q_d"]*lemming ~ lemming, col = color[4], lwd = 2)

lines(pred[5, "Q_d"]*lemming ~ lemming, col = color[4], lty = 2, lwd = 2)

mtext("A)", adj = -0.1, line = 2, cex = 2)


# Lemming mass consumption for 1 predator
plot(pred[8, "alpha"]*fox_thresh ~ fox_thresh, type = "l",
     ylab = NA, xlab = NA,
     col = color[2], lwd = 4, cex.axis = 2,
     ylim = c(0, 53000), xlim = c(0, 10))

mtext("Annual lemming consumption per individual\n(Lemming/ind)", 2, cex = 1.5, line = 4)
mtext("Lemming density\n(ind/ha)", 1, cex = 1.5, line = 5)

lines(pred[6, "alpha"]*fox_low ~ fox_low, col = color[2], lwd = 2)

lines(pred[7, "alpha"]*lemming ~ lemming, col = color[2], lty = 2, lwd = 2)

lines(pred[1, "alpha"]*owl_thresh ~ owl_thresh, col = color[1], lwd = 4)

lines(pred[2, "alpha"]*jaeger_thresh ~ jaeger_thresh, col = color[3], lwd = 4)

lines(pred[3, "alpha"]*lemming ~ lemming, col = color[4], lwd = 4)

lines(pred[4, "alpha"]*lemming ~ lemming, col = color[4], lwd = 2)

lines(pred[5, "alpha"]*lemming ~ lemming, col = color[4], lty = 2, lwd = 2)

mtext("B)", adj = -0.1, line = 2, cex = 2)


# Add legend
plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, bty = "n")

legend(0.6, 1.3, legend = c("Fox high summer", "Fox low summer", "Fox winter", "Owl high summer", "Jaeger high summer", "Ermine high summer", "Ermine low summer", "Ermine winter"),
       col = c(rep(color[2], 3), color[1], color[3], rep(color[4], 3)), lty = c(1,1,2,1,1,1,1,2), lwd = c(4,2,2,4,4,4,2,2), bty = "n", cex = 2)



# Same but with attack rate corrected for predator densities
plot(pred[8, "Q_density"]*fox_thresh ~ fox_thresh, type = "l",
     ylab = NA, xlab = NA,
     col = color[2], lwd = 4, cex.axis = 2, 
     ylim = c(0, 55), xlim = c(0, 10))

mtext("Annual lemming consumption\n(Lemming/ha)", 2, cex = 1.5, line = 4)
mtext("Lemming density\n(ind/ha)", 1, cex = 1.5, line = 5)

lines(pred[6, "Q_density"]*fox_low ~ fox_low, col = color[2], lwd = 2)

lines(pred[7, "Q_density"]*lemming ~ lemming, col = color[2], lty = 2, lwd = 2)

lines(pred[1, "Q_density"]*owl_thresh*2 ~ owl_thresh, col = color[1], lwd = 4)

lines(pred[2, "Q_density"]*jaeger_thresh*2 ~ jaeger_thresh, col = color[3], lwd = 4)

lines(pred[3, "Q_density"]*lemming ~ lemming, col = color[4], lwd = 4)

lines(pred[4, "Q_density"]*lemming ~ lemming, col = color[4], lwd = 2)

lines(pred[5, "Q_density"]*lemming ~ lemming, col = color[4], lty = 2, lwd = 2)

mtext("C)", adj = -0.1, line = 2, cex = 2)


dev.off()


########################################################
#                                                      #
#   Figure S4 mechanistic stochastic parameter plane   #
#                                                      #
########################################################

## Color
color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)
color <- rgb(color[,1], color[, 2], color[, 3], maxColorValue = 255)

### Mechanistic phase plot with stochasticity
# Winter length
q <- 0.75

#-------------------------------#
#   Lemming growth parameter    #
#-------------------------------#

# Monthly instantaneous growth rate
r.mean <- 4.62


r.high <- 6.15
r.low <- 3.08

r.sd <- (r.high - r.low)/4 # Have high and low approximately at 2 standard deviation from the mean

lemming.param <- list(s = list(mean = r.mean, sd = r.sd),
                      w = list(mean = r.mean, sd = r.sd))


#---------------------#
#   Owl parameters    #
#---------------------#

owl.p.mean <- 0.001375 # Get value from predator dataframe built in 5 + get sd
owl.p.sd <- 0.000611649

owl.a.mean <- 709.1973 # Get a values from Sauve-Legangeux
owl.a.sd <- 20/100 * owl.a.mean # No incertitude so add a 20% CV on the value

# Create list object for R.coordinates function
owl.param <- list(R1 = list(p = list(s = list(mean = 0, sd = 0),
                                     w = list(mean = 0, sd = 0)),
                            a = list(s = list(mean = 0, sd = 0),
                                     w = list(mean = 0, sd = 0))),
                  R2 = list(p = list(s = list(mean = owl.p.mean, sd = owl.p.sd),
                                     w = list(mean = 0, sd = 0)),
                            a = list(s = list(mean = owl.a.mean, sd = owl.a.sd),
                                     w = list(mean = 0, sd = 0))))


#------------------------#
#   jaeger parameters    #
#------------------------#

jaeger.p.mean <- 0.00498
jaeger.p.sd <- 0.002822843

jaeger.a.mean <- 312.0722
jaeger.a.sd <- 20/100 * jaeger.a.mean # No incertitude so add a 20% CV on the value

# Create list object for R.coordinates function
jaeger.param <- list(R1 = list(p = list(s = list(mean = 0, sd = 0),
                                        w = list(mean = 0, sd = 0)),
                               a = list(s = list(mean = 0, sd = 0),
                                        w = list(mean = 0, sd = 0))),
                     R2 = list(p = list(s = list(mean = jaeger.p.mean, sd = jaeger.p.sd),
                                        w = list(mean = 0, sd = 0)),
                               a = list(s = list(mean = jaeger.a.mean, sd = jaeger.a.sd),
                                        w = list(mean = 0, sd = 0))))


#---------------------#
#   fox parameters    #
#---------------------#

fox.p.mean <- 0.0008
fox.p.sd <- 20/100 * fox.p.mean # No incertitude on value so add a 20% CV

fox.a1.mean <- 1739.0000
fox.a1.sd <- 20/100 * fox.a1.mean # No incertitude so add a 20% CV on the value

fox.a2.mean <- 3595.7500
fox.a2.sd <- 20/100 * fox.a2.mean # No incertitude so add a 20% CV on the value

fox.aw.mean <- 1786.0000
fox.aw.sd <- 20/100 * fox.aw.mean # No incertitude so add a 20% CV on the value

# Create list object for R.coordinates function
fox.param <- list(R1 = list(p = list(s = list(mean = fox.p.mean, sd = fox.p.sd),
                                     w = list(mean = fox.p.mean, sd = fox.p.sd)),
                            a = list(s = list(mean = fox.a1.mean, sd = fox.a1.sd),
                                     w = list(mean = fox.aw.mean, sd = fox.aw.sd))),
                  R2 = list(p = list(s = list(mean = fox.p.mean, sd = fox.p.sd),
                                     w = list(mean = fox.p.mean, sd = fox.p.sd)),
                            a = list(s = list(mean = fox.a2.mean, sd = fox.a2.sd),
                                     w = list(mean = fox.aw.mean, sd = fox.aw.sd))))


#------------------------#
#   weasel parameters    #
#------------------------#

# Density in high year: 0.4 ind/km2, low year 0.02 ind/km2
weasel.p1.mean <- 0.02/100
weasel.p1.sd <- 20/100 * weasel.p1.mean # No incertitude on value so add a 20% CV

weasel.p2.mean <- 0.4/100
weasel.p2.sd <- 20/100 * weasel.p2.mean # No incertitude on value so add a 20% CV

weasel.a.mean <- 445.6515
weasel.a.sd <- 20/100 * weasel.a.mean # No incertitude so add a 20% CV on the value

weasel.aw.mean <- 367.8232
weasel.aw.sd <- 20/100 * weasel.aw.mean # No incertitude so add a 20% CV on the value

# Create list object for R.coordinates function
weasel.param <- list(R1 = list(p = list(s = list(mean = weasel.p1.mean, sd = weasel.p1.sd),
                                        w = list(mean = weasel.p1.mean, sd = weasel.p1.sd)),
                               a = list(s = list(mean = weasel.a.mean, sd = weasel.a.sd),
                                        w = list(mean = weasel.aw.mean, sd = weasel.aw.sd))),
                     R2 = list(p = list(s = list(mean = weasel.p2.mean, sd = weasel.p2.sd),
                                        w = list(mean = weasel.p2.mean, sd = weasel.p2.sd)),
                               a = list(s = list(mean = weasel.a.mean, sd = weasel.a.sd),
                                        w = list(mean = weasel.aw.mean, sd = weasel.aw.sd))))


#----------------------------------------------------#
#   Function to generate coordinates of R1 and R2    #
#----------------------------------------------------#

R.coordinates <- function(lemming, pred, n, avian = FALSE){
  
  # R2 - High predation year
  r.w <- rnorm(n, lemming$w$mean, lemming$w$sd)
  r.s <- rnorm(n, lemming$s$mean, lemming$s$sd)
  
  p.w <- rnorm(n, pred$R2$p$w$mean, pred$R2$p$w$sd)
  p.s <- rnorm(n, pred$R2$p$s$mean, pred$R2$p$s$sd)
  
  if(avian == TRUE) p.s <- 2*p.s
  
  a.w <- rnorm(n, pred$R2$a$w$mean, pred$R2$a$w$sd)
  a.s <- rnorm(n, pred$R2$a$s$mean, pred$R2$a$s$sd)
  
  R2 <- exp((r.w - a.w * p.w) * q) * exp((r.s - a.s * p.s) * (1 - q))
  
  
  # R1 - Low predation yeat
  r.w <- rnorm(n, lemming$w$mean, lemming$w$sd)
  r.s <- rnorm(n, lemming$s$mean, lemming$s$sd)
  
  p.w <- rnorm(n, pred$R1$p$w$mean, pred$R2$p$w$sd)
  p.s <- rnorm(n, pred$R1$p$s$mean, pred$R2$p$s$sd)
  
  a.w <- rnorm(n, pred$R1$a$w$mean, pred$R2$a$w$sd)
  a.s <- rnorm(n, pred$R1$a$s$mean, pred$R2$a$s$sd)
  
  R1 <- exp((r.w - a.w * p.w) * q) * exp((r.s - a.s * p.s) * (1 - q))
  
  return(as.data.frame(cbind(R1 = R1, R2 = R2)))
}


#------------------------------------------------------#
#   Create 1000 replicate of R1 and R2 combinations    #
#------------------------------------------------------#

set.seed(20)

owl.points <- R.coordinates(lemming.param, owl.param, n = 1000, avian = TRUE)
z.owl <- ks::kde(log(owl.points), compute.cont=TRUE) %>% # Compute the density kernel
  with(., contourLines(x = eval.points[[1]],
                       y = eval.points[[2]], 
                       z = estimate,
                       levels = cont["5%"])[[1]]) %>% # Compute contour line of 95% kernel
  data.frame(.) %>% 
  exp(.)


jaeger.points <- R.coordinates(lemming.param, jaeger.param, n = 1000, avian = TRUE)
z.jaeger <- ks::kde(log(jaeger.points), compute.cont=TRUE) %>% # Compute the density kernel
  with(., contourLines(x = eval.points[[1]],
                       y = eval.points[[2]],
                       z = estimate,
                       levels = cont["5%"])[[1]]) %>% # Compute contour line of 95% kernel
  data.frame(.) %>% 
  exp()


fox.points <- R.coordinates(lemming.param, fox.param, n = 1000)
z.fox <- ks::kde(log(fox.points), compute.cont=TRUE) %>% # Compute the density kernel
  with(., contourLines(x = eval.points[[1]],
                       y = eval.points[[2]],
                       z = estimate,
                       levels = cont["5%"])[[1]]) %>% # Compute contour line of 95% kernel
  data.frame(.) %>% 
  exp(.)


weasel.points <- R.coordinates(lemming.param, weasel.param, n = 1000)
z.weasel <- ks::kde(log(weasel.points), compute.cont=TRUE) %>% # Compute the density kernel
  with(., contourLines(x = eval.points[[1]],
                       y = eval.points[[2]],
                       z = estimate,
                       levels = cont["5%"])[[1]]) %>% # Compute contour line of 95% kernel
  data.frame(.) %>% 
  exp()


#---------------------------------------#
# R2 vs R1 plot for different K value   #
#---------------------------------------#

pdf("figS4_mechanistic_stochastic_parameter_plane_plot.pdf", height = 12, width = 8)

par(mar = c(6.1, 5.1, 4.1, 2.1))

# To remove scientific notation on the exponential y axis
options(scipen=5)

# R1, R2 and k values for the first plot
R1 <- seq(1, 1000, length.out = 10000)
k <- c(15:2, 1/c(1:15))
R2 <- 1 / R1^k[1]

# This plot the R1 and R2 values for K = 1
plot(R2 ~ R1, type = "l", log = "xy",
     ylim = c(0.1, 1000), xlim = c(1, 1000),
     yaxs = "i", xaxs = "i",
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "")

axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5)

mtext(expression("R"[1]), side = 2, line = 3, cex = 2)
mtext(expression("R"[0]), side = 1, line = 3, cex = 2)

# Find other R1-R2 coordinates for other k
for(i in 2:length(k)){
  R2 <- 1 / R1^k[i]
  
  if(k[i] == 1){ ltype <- 2
  } else {ltype <- 1}
  
  if(k[i] < 1){lcolor <- color[2]
  } else {lcolor <- "black"}
  
  lines(R2 ~ R1, type = "l", lwd = 1.5, 
        lty = ltype, col = lcolor)
}

abline(h = 1, col = color[2], lty = 2)


#-------------------------#
#   Add points to plot    #
#-------------------------#

points(owl.points$R2 ~ owl.points$R1, col = color[4])
points(jaeger.points$R2 ~ jaeger.points$R1, col = color[6])
points(fox.points$R2 ~ fox.points$R1, col = color[5])
points(weasel.points$R2 ~ weasel.points$R1, col = color[7])


lines(z.owl$y ~ z.owl$x, lwd = 2, col = color[4])
lines(z.jaeger$y ~ z.jaeger$x, lwd = 2, col = color[6])
lines(z.fox$y ~ z.fox$x, lwd = 2, col = color[5])
lines(z.weasel$y ~ z.weasel$x, lwd = 2, col = color[7])

legend(1.01, 800, legend = c("Owl", "Fox", "Jaeger", "Ermine"),
       col = color[4:7], lwd = 2, bty = "n", cex = 2)

dev.off()


##############################################
#                                            #
#   Figure S5 predator time series density   #
#                                            #
##############################################

color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)[c(4:7),]
color <- rgb(color[, 1], color[, 2], color[,3], max = 255)

#-------------------#
#   Predator data   #
#-------------------#

predator <- readRDS("data_clean/2_PredatorAbundance.rds")

#------------------#
#   Lemming data   #
#------------------#

lemmingDensity <- readRDS("data_clean/1_lemmingMeanDensity.rds")[, c(1,2)]

predator <- left_join(predator, lemmingDensity, by = "year")
predator[which(predator$period == "P3"), "density"] <- NA

rm(lemmingDensity)

# Presence absence threshold 
threshold <- readRDS("data_clean/3_predator_threshold.rds")

#---------------#
#   Snowy owl   #
#---------------#

tikz("figS5_predator_time_series_density.tex", height = 6, width = 10, standAlone = TRUE) 

par(mfrow = c(2, 4), mar = c(7.1, 6.1, 7.1, 1), mgp = c(3.6, 1, 0))

# Plot nest density vs years
barplot(owl ~ year, data = predator[which(predator$Season == "S"),], 
        col = color[1], border = NA, las = 2,
        xlab = "", ylab = "", ylim = c(0, 0.0025), cex.axis = 1.2, cex.names = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 4.5, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Snowy owl", adj = 3.5, line = 4, cex = 2)

mtext("A)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = 0.00015, lty = 2)



# Plot nest density vs lemming density
plot(owl ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[1],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("B)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$owl$mu, lty = 2)
abline(h = 0.00015, lty = 2)
text(threshold$owl$mu + 2, 0.002, paste("L* =", signif(threshold$owl$mu, 2)), font = 2, cex = 1.5)


#---------------#
#   Arctic fox  #
#---------------#
fox_threshold <- 0.1

# Plot active den proportion vs years
barplot(fox_repro ~ year, data = predator[which(predator$Season == "S"),],
        col = color[2], border = NA, las = 2,
        xlab = "", ylab = "", ylim = c(0, 0.35), cex.axis = 1.2, cex.names = 1.4)

mtext("Reproductive den\nproportion", side = 2, line = 3, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Arctic fox", adj = 3, line = 4, cex = 2)

mtext("C)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = fox_threshold, lty = 2)

text(9, 0.12, "Discretisation threshold", font = 2, cex = 1.2)


# Plot active den proportion vs mean june lemming density
plot(fox_repro ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[2],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Reproductive den\nproportion", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("D)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$fox$mu, lty = 2)
abline(h = fox_threshold, lty = 2)
text(threshold$fox$mu + 2, 0.28, paste("L* =", signif(threshold$fox$mu, 2)), font = 2, cex = 1.5)

text(5, 0.25, "True presence", cex = 1.2)
text(5, 0.05, "False presence", cex = 1.2)
text(0.8, 0.05, "True \nabsence", cex = 1.2)
text(0.8, 0.25, "False \nabsence", cex = 1.2)


#----------------------------#
#   Long-tailed jaeger       #
#----------------------------#
jaeger_threshold <- 0.001

# Plot nest density vs years
barplot(jaeger ~ year, data = predator[which(predator$Season == "S"),], col = color[3],
        xlab = "", ylab = "", border = NA, las = 2, ylim = c(0, 0.012), cex.axis = 1.2, cex.names = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 4, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Long-tailed jaeger", adj = -2, line = 4, cex = 2)

mtext("E)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = jaeger_threshold, lty = 2)


# Plot nest density vs lemming density
plot(jaeger ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[3],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Nest density (Nest/ha)", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("F)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$jaeger$mu, lty = 2)
abline(h = jaeger_threshold, lty = 2)
text(threshold$jaeger$mu + 2, 0.010, paste("L* =", signif(threshold$jaeger$mu, 2)), font = 2, cex = 1.5)


#----------------#
#   Weasel       #  
#----------------#

# Plot weasel index vs years
barplot(weasel ~ year, data = predator[which(predator$Season == "S"),], col = color[4], 
        xlab = "", ylab = "", border = NA, las = 2, ylim = c(0, 3), cex.axis = 1.2, cex.names = 1.4)

mtext("Abundance index", side = 2, line = 4, cex = 1.2)
mtext("Years", side = 1, line = 4.5, cex = 1.2)

mtext("Ermine", adj = 1.9, line = 4, cex = 2)

mtext("G)", adj = -0.2, line = 1.5, cex = 1.2)

abline(h = 0.5, lty = 2)


# Plot weasel index vs lemming density
plot(weasel ~ density, data = predator[which(predator$Season == "S"),], pch = 19, col = color[4],
     xlab = "", ylab = "", cex.axis = 1.4)

mtext("Abundance index", side = 2, line = 3, cex = 1.2)
mtext("Lemming density\n(n/ha)", side = 1, line = 5, cex = 1.2)

mtext("H)", adj = -0.2, line = 1.5, cex = 1.2)

abline(v = threshold$weasel$mu, lty = 2)
abline(h = 0.5, lty = 2)

text(4.5, 2.7, "Binomial threshold not conclusive", cex = 1.2)


dev.off()


#################################
#                               #
#   Figure S6 growth by group   #
#                               #
#################################

# Color scale
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
    predator[i, "meca_fit"] <- meca$Lem_alone
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


#---------------------------------------------------------#
#   Difference between obs and pred by predation group    #
#---------------------------------------------------------#

for (i in 1:nrow(predator)) {
  
  pred <- NULL
  
  if(predator[i, "owl"] == 1) pred <- paste0(pred, "O")
  
  if(predator[i, "fox_repro"] == 1) pred <- paste0(pred, "F")
  
  if(predator[i, "jaeger"] == 1) pred <- paste0(pred, "J")
  
  if(predator[i, "weasel"] == 1) pred <- paste0(pred, "E")
  
  if(is.null(pred)) pred <- "none"
  
  predator[i, "pred_class"] <- pred
  
}

rm(pred)

predator$pred_class <- factor(predator$pred_class, levels = c("none", "E", "JE", "OJE", "FJ", "OFJ", "FJE", "OFJE"))

pred_var <- predator |> 
  distinct(pred_class, .keep_all = TRUE) |> 
  arrange(pred_class)

tikz("FigS6_Growth_by_group.tex", width = 7, height = 7, standAlone = TRUE)

par(mar = c(7.1, 6.1, 4.1, 2.1), lend=1)

boxplot(meca_fit ~ pred_class, data = predator, border = NA, log = 'y', yaxt = 'n',
        ylab = NA, xlab = NA, ylim = exp(c(-3.5, 5.5)), lwd = 2, cex.axis = 1.5)

yticks <- c(0.01, 0.1, 1, 10, 100)

axis(2, at = yticks, labels = sub("\\.?0+$", "", format(yticks, scientific = FALSE)), cex.axis = 1.5)

mtext("Annual growth rate", 2, cex = 2, line = 3.5)
mtext("Predation assemblages", 1, cex = 2, line = 4)

for (i in 1:nrow(pred_var)) {
  
  polygon(x = c(i, i+0.4, i+0.4, i), y = exp(c(rep(pred_var[i, "pheno_lwr"], 2),  rep(pred_var[i, "pheno_upr"], 2))),
          col = adjustcolor(color[4], alpha = 0.5), border = NA)
  segments(x0 = i, x1 = i+0.4, y0 = exp(pred_var[i, "pheno_fit_link"]),
           col = adjustcolor(color[4]), lwd = 6)
  
  polygon(x = c(i-0.4, i, i, i-0.4), y = exp(c(rep(pred_var[i, "meca_lwr"], 2),  rep(pred_var[i, "meca_upr"], 2))),
          col = adjustcolor(color[3], alpha = 0.5), border = NA)
  segments(x0 = i, x1 = i-0.4, y0 = exp(pred_var[i, "meca_fit"]),
           col = adjustcolor(color[3]), lwd = 6)
  
  
} 


legend(5.8, exp(5.5), legend = c("Mechanistic", "Phenomenological", "Observed values"), pch = c(NA, NA, 19), 
       lwd = c(2, 2, NA), box.lty = 0, col = c(color[c(3,4)], "black"), cex = 1.2)

abline(h = 1, lty = 2, col = "grey")

points(exp(Growth) ~ pred_class, data = predator, pch = 19)

dev.off()
