library(jsonlite)
library(MASS)
library(magrittr)

## Couleurs
color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)
color <- rgb(color[,1], color[, 2], color[, 3], maxColorValue = 255)


### Test Mechanistic phase plot with stochasticity

# Winter length
q <- 0.75

#-------------------------------#
#   Lemming growth parameter    #
#-------------------------------#

# Montly instatenaous growth rate
r.mean <- 4.62


r.high <- 6.15
r.low <- 3.08

r.sd <- (r.high - r.low)/4 # Have high and low approximatly at 2 standard deviation from the mean

lemming.param <- list(s = list(mean = r.mean, sd = r.sd),
                      w = list(mean = r.mean, sd = r.sd))


#---------------------#
#   Owl parameters    #
#---------------------#

owl.p.mean <- 0.001375 # Get value from predator dataframe built in 5 + get sd
owl.p.sd <- 0.000611649

owl.a.mean <- 709.1973 # Get a values from 7 Sauve-Legangeux
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

pdf("results/=figS4_mechanistic_stochastic_parameter_plane_plot.pdf", height = 12, width = 8)

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

mtext(expression("R"[2]), side = 2, line = 3, cex = 2)
mtext(expression("R"[1]), side = 1, line = 3, cex = 2)

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
