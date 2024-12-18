library(tidyverse)

# Run the mechanistic parameterisation of the model

#-------------------#
#   Predator data   #
#-------------------#
predator <- readRDS("data_clean/2_PredatorPresence.rds")

threshold <- readRDS("data_clean/3_predator_threshold.rds")


#------------------#
#   Lemming data   #
#------------------#
lemmingDensity <- readRDS("data_clean/1_lemmingMeanDensity.rds")[, c(1,2)]

predator <- predator[which(predator$Season == "S"),]

predator <- left_join(predator, lemmingDensity, by = "year")

rm(lemmingDensity)


#----------------------------------#
#   Diet and energetic need data   #
#----------------------------------#

pred <- read.csv2("data_raw/Sauvé_Legagneux_alpha2.csv", sep = ",")[, c(1:6)]

pred[, "Q_g"] <- pred$Q_B * pred$mass_g # Get Q
pred[, "Q_d_g"] <- pred$Q_g * pred$d # Pondaration for pred diet
pred[, "Q_d_ind"] <- pred$Q_d_g / pred$Lemming_g # Transform mass into lemming number

# For attack rate: divide energetic need by threshold for high state
# For low state or winter, use cross-multiplication to find attack rate slope
# This assume that the reduction in attack rate is proportional to the energetic need
# This also keep the relationship between the attack rate for one species intact

pred[, "R"] <- NA

# For weasel, use the mean density of predator during high state
pred[which(pred$specie == "weasel" & pred$season == "Hsummer"), "R"] <- mean(predator[which(predator$weasel == 1), "density"])
pred[which(pred$specie == "owl"), "R"] <- mean(predator[which(predator$owl == 1), "density"])
pred[which(pred$specie == "jaeger"), "R"] <- mean(predator[which(predator$jaeger == 1), "density"])

# Compute attack rate
pred[, "alpha"] <- pred$Q_d_ind / pred$R # Divide by prey density (Sauv? 2020)

# Change fox attack rate for Beardsell (2021)
pred[which(pred$specie == "arctic fox" & pred$season == "Hsummer"), "alpha"] <- 0.012 * 100 * 12 * 365

# Cross-multiplication
pred[which(pred$specie == "arctic fox" & pred$season == "Lsummer"), "alpha"] <- pred[6, "Q_d_ind"] * pred[8, "alpha"] / pred[8, "Q_d_ind"]
pred[which(pred$specie == "arctic fox" & pred$season == "winter"), "alpha"] <- pred[7, "Q_d_ind"] * pred[8, "alpha"] / pred[8, "Q_d_ind"]

pred[which(pred$specie == "weasel" & pred$season == "Lsummer"), "alpha"] <- pred[4, "Q_d_ind"] * pred[3, "alpha"] / pred[3, "Q_d_ind"]
pred[which(pred$specie == "weasel" & pred$season == "winter"), "alpha"] <- pred[5, "Q_d_ind"] * pred[3, "alpha"] / pred[3, "Q_d_ind"]

# Save 
saveRDS(pred, "data_clean/5_meca_attack_rate.RDS")

#--------------------------------------------#
#  Measure the Ri value for each assemblage  #
#--------------------------------------------#

# Lemming growth rate = 0.5
r <- 4.62

Density <- readRDS("data_clean/5_PredMeanDensity.rds")

q <- 0.75


#-------------------------------------#
#   Growth rate for each assemblage   #
#-------------------------------------#

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

saveRDS(R2_meca, "data_clean/5_R2_meca.RDS")


#---------------------------------------------------#
#   Measure the variance around each growth value   #
#---------------------------------------------------#
# Function to calculate variance of the product of two independent variables
variance_product <- function(mean_x, sd_x, mean_y, sd_y) {
  # Calculate variances from standard deviations
  var_x <- sd_x^2
  var_y <- sd_y^2
  
  # Calculate the variance of the product
  var_product <- (mean_x^2 * var_y) + (mean_y^2 * var_x) + (var_x * var_y)
  
  return(var_product)
}

# Function to calculate variance of the sum of two independent variables
variance_sum <- function(sd_x, sd_y) {
  # Calculate variances from standard deviations
  var_x <- sd_x^2
  var_y <- sd_y^2
  
  # Calculate the variance of the product
  var_sum <- sqrt(var_x + var_y)
  
  return(var_sum)
}

# Function to calculate variance of the product of an independent variable to a constant
variance_prod_c <- function(sd_x, c) {
  
  var_c <- sd_x^2 * c^2
  
  return(var_c)
}

## R1_FW
# winter
var.f.w <- variance_product(mean_x = fox.aw.mean, sd_x = fox.aw.sd,
                            mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.w <- variance_product(mean_x = weasel.aw.mean, sd_x = weasel.aw.sd,
                            mean_y = weasel.p1.mean, sd_y = weasel.p1.sd)

var.fw.w <- variance_sum(sd_x = var.f.w, sd_y = var.w.w)

var.fwr.w <- variance_sum(sd_x = r.sd, sd_y = var.fw.w)
var.fwrq.w <- variance_prod_c(sd_x = var.fwr.w, c = q)

# Low summer
var.f.a1 <- variance_product(mean_x = fox.a1.mean, sd_x = fox.a1.sd,
                             mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.a1 <- variance_product(mean_x = weasel.a.mean, sd_x = weasel.a.sd,
                             mean_y = weasel.p1.mean, sd_y = weasel.p1.sd)

var.fw.a1 <- variance_sum(sd_x = var.f.a1, sd_y = var.w.a1)

var.fwr.a1 <- variance_sum(sd_x = r.sd, sd_y = var.fw.a1)
var.fwrq.a1 <- variance_prod_c(sd_x = var.fwr.a1, c = (1-q))

var.R1_FW <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fwrq.a1)

## Weasel
# winter
var.f.w <- variance_product(mean_x = fox.aw.mean, sd_x = fox.aw.sd,
                            mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.w <- variance_product(mean_x = weasel.aw.mean, sd_x = weasel.aw.sd,
                            mean_y = weasel.p2.mean, sd_y = weasel.p2.sd)

var.fw.w <- variance_sum(sd_x = var.f.w, sd_y = var.w.w)

var.fwr.w <- variance_sum(sd_x = r.sd, sd_y = var.fw.w)
var.fwrq.w <- variance_prod_c(sd_x = var.fwr.w, c = q)

# High summer for weasel
var.f.a1 <- variance_product(mean_x = fox.a1.mean, sd_x = fox.a1.sd,
                             mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.a2 <- variance_product(mean_x = weasel.a.mean, sd_x = weasel.a.sd,
                             mean_y = weasel.p2.mean, sd_y = weasel.p2.sd)

var.fw.a2 <- variance_sum(sd_x = var.f.a1, sd_y = var.w.a2)

var.fwr.a2 <- variance_sum(sd_x = r.sd, sd_y = var.fw.a2)
var.fwrq.a2 <- variance_prod_c(sd_x = var.fwr.a2, c = (1-q))

var.R2_W <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fwrq.a2)

## Weasel + Jaeger
# High summer for weasel + Jaeger
var.j.a2 <- variance_prod_c(sd_x = jaeger.p.sd, c = 2)
var.j.a2 <- variance_product(mean_x = jaeger.a.mean, sd_x = jaeger.a.sd,
                             mean_y = jaeger.p.mean*2, sd_y = var.j.a2)

var.fwj.a2 <- variance_sum(sd_x = var.fw.a2, sd_y = var.j.a2)

var.fwjr.a2 <- variance_sum(sd_x = r.sd, sd_y = var.fwj.a2)

var.fwrjq.a2 <- variance_prod_c(sd_x = var.fwjr.a2, c = (1-q))

var.R2_JW <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fwrjq.a2)

## Weasel + Jaeger + Owl
# High summer for weasel + Jaeger
var.o.a2 <- variance_prod_c(sd_x = owl.p.sd, c = 2)
var.o.a2 <- variance_product(mean_x = owl.a.mean, sd_x = owl.a.sd,
                             mean_y = owl.p.mean*2, sd_y = var.o.a2)

var.fwjo.a2 <- variance_sum(sd_x = var.fwj.a2, sd_y = var.o.a2)

var.fwjor.a2 <- variance_sum(sd_x = r.sd, sd_y = var.fwjo.a2)

var.fwrjoq.a2 <- variance_prod_c(sd_x = var.fwjor.a2, c = (1-q))

var.R2_JWO <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fwrjoq.a2)

## Fox + Jaeger
# winter
var.f.w <- variance_product(mean_x = fox.aw.mean, sd_x = fox.aw.sd,
                            mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.w <- variance_product(mean_x = weasel.aw.mean, sd_x = weasel.aw.sd,
                            mean_y = weasel.p1.mean, sd_y = weasel.p1.sd)

var.fw.w <- variance_sum(sd_x = var.f.w, sd_y = var.w.w)

var.fwr.w <- variance_sum(sd_x = r.sd, sd_y = var.fw.w)
var.fwrq.w <- variance_prod_c(sd_x = var.fwr.w, c = q)

# High summer for Fox + Jaeger
var.f.a2 <- variance_product(mean_x = fox.a2.mean, sd_x = fox.a2.sd,
                             mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.a1 <- variance_product(mean_x = weasel.a.mean, sd_x = weasel.a.sd,
                             mean_y = weasel.p1.mean, sd_y = weasel.p1.sd)

var.fj.a2 <- variance_sum(sd_x = var.f.a2, sd_y = var.j.a2)
var.fjw.a2 <- variance_sum(sd_x = var.fj.a2, var.fw.a1)

var.fjwr.a2 <- variance_sum(sd_x = r.sd, sd_y = var.fjw.a2)

var.fjwrq.a2 <- variance_prod_c(sd_x = var.fjwr.a2, c = (1-q))

var.R2_FJ <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fjwrq.a2)

## Fox + Jaeger + Weasel
# winter
var.f.w <- variance_product(mean_x = fox.aw.mean, sd_x = fox.aw.sd,
                            mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.w <- variance_product(mean_x = weasel.aw.mean, sd_x = weasel.aw.sd,
                            mean_y = weasel.p2.mean, sd_y = weasel.p2.sd)

var.fw.w <- variance_sum(sd_x = var.f.w, sd_y = var.w.w)

var.fwr.w <- variance_sum(sd_x = r.sd, sd_y = var.fw.w)
var.fwrq.w <- variance_prod_c(sd_x = var.fwr.w, c = q)

# High summer for Fox + Jaeger + Weasel
var.fjw.a2 <- variance_sum(sd_x = var.fj.a2, sd_y = var.w.a2)

var.fjwr.a2 <- variance_sum(sd_x = r.sd, sd_y = var.fjw.a2)

var.fjwrq.a2 <- variance_prod_c(sd_x = var.fjwr.a2, c = (1-q))

var.R2_FJW <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fjwrq.a2)

## Fox + Jaeger + Weasel + Owl
# High summer for Fox + Jaeger + Weasel + Owl
var.fjwo.a2 <- variance_sum(sd_x = var.fjw.a2, sd_y = var.o.a2)

var.fjwor.a2 <- variance_sum(sd_x = r.sd, sd_y = var.fjwo.a2)

var.fjworq.a2 <- variance_prod_c(sd_x = var.fjwor.a2, c = (1-q))

var.R2_FJWO <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fjworq.a2)

## Fox + Jaeger + Owl
var.f.w <- variance_product(mean_x = fox.aw.mean, sd_x = fox.aw.sd,
                            mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.w <- variance_product(mean_x = weasel.aw.mean, sd_x = weasel.aw.sd,
                            mean_y = weasel.p1.mean, sd_y = weasel.p1.sd)

var.fw.w <- variance_sum(sd_x = var.f.w, sd_y = var.w.w)

var.fwr.w <- variance_sum(sd_x = r.sd, sd_y = var.fw.w)
var.fwrq.w <- variance_prod_c(sd_x = var.fwr.w, c = q)

# High summer for Fox + Jaeger + Owl
var.f.a2 <- variance_product(mean_x = fox.a2.mean, sd_x = fox.a2.sd,
                             mean_y = fox.p.mean, sd_y = fox.p.sd)
var.w.a1 <- variance_product(mean_x = weasel.a.mean, sd_x = weasel.a.sd,
                             mean_y = weasel.p1.mean, sd_y = weasel.p1.sd)

var.fj.a2 <- variance_sum(sd_x = var.f.a2, sd_y = var.j.a2)
var.fjo.a2 <- variance_sum(sd_x = var.fj.a2, sd_y = var.o.a2)
var.fjow.a2 <- variance_sum(sd_x = var.fjo.a2, var.w.a1)

var.fjowr.a2 <- variance_sum(sd_x = r.sd, sd_y = var.fjow.a2)

var.fjowrq.a2 <- variance_prod_c(sd_x = var.fjowr.a2, c = (1-q))

var.R2_FJO <- variance_sum(sd_x = var.fwrq.w, sd_y = var.fjowrq.a2)

### Regroup together
R2_var_meca <- list(Lem_alone = var.R1_FW, 
                    Weasel = var.R2_W,
                    JW = var.R2_JW,
                    OJW = var.R2_JWO,
                    FJ = var.R2_FJ,
                    OFJ = var.R2_FJO,
                    FJW = var.R2_FJW,
                    OFJW = var.R2_FJWO)

saveRDS(R2_var_meca, "data_clean/5_R2_var_meca.RDS")

#--------------------------------                                             -----#
#   Measure what is the density of predator needed to bring the growth rate to 0   #
#-------------------------                                             ------------#
# Fox
p.fox = (r * (1 - q) + r * q) / (pred[8, "alpha"] * (1 - q) + pred[7, "alpha"] * q)

# Owl
p.owl = (r * (1 - q) + r * q) / (pred[1, "alpha"] * (1 - q))

# Jaeger
p.jaeger = (r * (1 - q) + r * q) / (pred[2, "alpha"] * (1 - q))

# Weasel
p.weasel = (r * (1 - q) + r * q) / (pred[3, "alpha"] * (1 - q) + pred[5, "alpha"] * q)

