library(jsonlite)
library(tikzDevice)

library(grImport2)
library(rsvg)
library(plotrix)

library(deSolve)


# Colors
color <- fromJSON(txt = "data_clean/colorJSON.json")
color <- matrix(unlist(color$rgb), ncol = 3, byrow = T)
color <- rgb(color[,1], color[, 2], color[, 3], maxColorValue = 255)[4:7]

# Plot size : 30 * 15

tikz("fig1_schema.tex", height = 18, width = 28, standAlone = TRUE)

layout(matrix(c(1,1,2,2,7,7,7,
                3,3,4,4,7,7,7,
                5,5,6,6,7,7,7), ncol = 7, byrow = TRUE))

par(mar = c(8.1, 12.1, 15, 5.1), mgp = c(3, 2, 0))


#-------------------------------#
#    Continuous migrant plot    #
#-------------------------------#

x <- seq(-5, 5, length.out = 50)

L <- 1
xo <- 0
k <- 3
  
migrant_summer <- c(L / (1 + exp(-k * (x - xo))), L / (1 + exp(-k * (-x - xo)))) 

migrant_continuous <- c(rep(0, 100), migrant_summer, rep(0, 100), migrant_summer)


plot(migrant_continuous, type = "l", bty = "L",
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "",
     lwd = 6, axes = FALSE)

axis(1, lwd.ticks = 0, labels = NA, at = c(-15:450), lwd = 3)
axis(2, lwd.ticks = 0, labels = NA, at = c(-1:4), lwd = 3)

axis(1, lwd = 7, labels = NA, lwd.ticks = 0, at = c(0:110), col = color[3], line = 1)
axis(1, lwd = 7, labels = NA, lwd.ticks = 0, at = c(190:310), col = color[3], line = 1)

axis(1, lwd = 7, labels = NA, lwd.ticks = 0, at = c(120:180), col = color[2], line = 1)
axis(1, lwd = 7, labels = NA, lwd.ticks = 0, at = c(320:380), col = color[2], line = 1)


mtext("Migrant density", side = 2, line = 3, cex = 3, at = 0.5)
mtext("Time", side = 1, line = 7, cex = 3)

mtext(c("W", "S", "W", "S"), side = 1, line = 3, cex = 2, at = c(55, 150, 250, 350))

mtext("A)", adj = -0.2, line = 3, cex = 3)

mtext("Continuous dynamics", adj = 4.5, line = 8, cex = 4)


#--------------------------------#
#    Continuous predator plot    #
#--------------------------------#

x <- seq(-20, 20, length.out = 50)

L <- 1
xo <- 0
k <- 0.5


pred_continuous <- L / (1 + exp(-k * (x - xo)))


plot(pred_continuous, type = "l", bty = "L",
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "",
     lwd = 6, axes = FALSE)

abline(v = 25, lty = 2, lwd = 6)
text("$L^*$", x = 21, y = 0.9, cex = 5)

axis(1, lwd.ticks = 0, labels = NA, at = c(-1:50), lwd = 3)
axis(2, lwd.ticks = 0, labels = NA, at = c(-1:4), lwd = 3, cex.axis = 3)
axis(2, lwd.ticks = 4, labels = c(0, 1), at = c(0, 1), las = 1, cex.axis = 4)

mtext("Predator\nreproduction", side = 2, line = 5, cex = 2.8, at = 0.5)
mtext("Prey density", side = 1, line = 3, cex = 3)

mtext("D)", adj = -0.3, line = 3, cex = 3)


#-----------------------------#
#    Discrete migrant plot    #
#-----------------------------#

mig1 <- round(migrant_continuous)
mig1[which(mig1 == 1)] <- NA

mig2 <- round(migrant_continuous)
mig2[which(mig2 == 0)] <- NA

plot(mig1, xlim = c(0, 400), ylim = c(0, 1),
     type = "l", bty = "L", col = color[3],
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "",
     lwd = 7, axes = FALSE)

lines(mig2, lwd = 7, col = color[2])

segments(x0 = 126, y0 = 0, y1 = 1, lty = 2, lwd = 6)
segments(x0 = 175, y0 = 0, y1 = 1, lty = 2, lwd = 6)
segments(x0 = 326, y0 = 0, y1 = 1, lty = 2, lwd = 6)
segments(x0 = 376, y0 = 0, y1 = 1, lty = 2, lwd = 6)

points(x = c(126, 175, 326, 376), y = c(0, 1, 0, 1), cex = 3, pch = 19)
points(x = c(126, 175, 326, 376), y = c(1, 0, 1, 0), cex = 2.5, pch = 19, col = "white")
points(x = c(126, 175, 326, 376), y = c(1, 0, 1, 0), cex = 3, pch = 1)

axis(1, lwd.ticks = 0, labels = NA, at = c(-15:450), lwd = 3)
axis(2, lwd.ticks = 0, labels = NA, at = c(-1:4), lwd = 3)

mtext("Migrant density", side = 2, line = 3, cex = 3, at = 0.5)
mtext("Time", side = 1, line = 4, cex = 3)

mtext("B)", adj = -0.2, line = 3, cex = 3)

mtext("Discretized dynamics", adj = 4.5, line = 8, cex = 4)


#------------------------------#
#    Discrete predator plot    #
#------------------------------#

pred0 <- round(pred_continuous)
pred0[which(pred0 == 1)] <- NA

pred1 <- round(pred_continuous)
pred1[which(pred1 == 0)] <- NA


plot(pred0, type = "l", bty = "L",
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "",
     lwd = 7, axes = FALSE, 
     col = color[3], ylim = c(0,1))

lines(pred1, lwd = 7, col = color[2])

segments(25, y0 = 0, y1 = 1, lty = 2, lwd = 6)
text("$L^*$", x = 29, y = 0.5, cex = 5)

points(x = 25, y = 1, cex = 3, pch = 19)
points(x = 25, y = 0, cex = 2.5, pch = 19, col = "white")
points(x = 25, y = 0, cex = 3, pch = 1)

axis(1, lwd.ticks = 0, labels = NA, at = c(-1:50), lwd = 3)
axis(2, lwd.ticks = 0, labels = NA, at = c(-1:4), lwd = 3, cex.axis = 3)
axis(2, lwd.ticks = 4, labels = c(0, 1), at = c(0, 1), las = 1, cex.axis = 4)

mtext("Predator\nreproduction", side = 2, line = 5, cex = 2.8, at = 0.5)
mtext("Prey density", side = 1, line = 3, cex = 3)

mtext("E)", adj = -0.3, line = 3, cex = 3)


#---------------------------------------#
#    Seasonal schema of state switch    #
#---------------------------------------#
par(mar = c(8.1, 8.1, 4.1, 3.1), mgp = c(3, 2, 0))

# Convert svg file to cairo svg file format
rsvg_svg(svg = "svg/lemming_black.svg", file = "svg/lemming_black_cairo.svg")
rsvg_svg(svg = "svg/snowy_owl_2.svg", file = "svg/snowy_owl_2_cairo.svg")
rsvg_svg(svg = "svg/snowy_owl_2_dashed.svg", file = "svg/snowy_owl_2_dashed_cairo.svg")

rsvg_svg(svg = "svg/arctic_fox_family_dashed.svg", file = "svg/arctic_fox_family_dashed_cairo.svg")
rsvg_svg(svg = "svg/arctic_fox_family_black.svg", file = "svg/arctic_fox_family_black_cairo.svg")

# Read svg
SVGlemming <- readPicture("svg/lemming_black_cairo.svg")
SVGowl_black <- readPicture("svg/snowy_owl_2_cairo.svg")
SVGowl_dashed <- readPicture("svg/snowy_owl_2_dashed_cairo.svg")

SVGfox <- readPicture("svg/arctic_fox_family_dashed_cairo.svg")
SVGfox_family <- readPicture("svg/arctic_fox_family_black_cairo.svg")


par(mar = c(1.1, 2.1, 8.1, 2.1))

# Create left plot
plot.new()


plotrix::draw.circle(x = 0.1875, y = 0.5, radius = 0.2, border = color[3], lwd = 8)
plotrix::draw.circle(x = 0.8125, y = 0.5, radius = 0.2, border = color[2], lwd = 8)


iArrows <- igraph:::igraph.Arrows


iArrows(x1 = 0.3, y1 = 0.85, x2 = 0.7, y2 = 0.85,
        h.lwd = 4, sh.lwd = 4, curve = 0.5,
        width = 4, size = 1)


iArrows(x1 = 0.7, y1 = 0.15, x2 = 0.3, y2 = 0.15,
        h.lwd = 4, sh.lwd = 4, curve = 0.5,
        width = 4, size = 1)


text("Summer", x = 0.82, y = 0.95, cex = 4)
text("Winter", x = 0.18, y = 0.95, cex = 4)


# Plot svg
grid.picture(SVGlemming,   x = 0.24, y = 0.11,
             width = grid::unit(1.21, "in"), height = grid::unit(1.1, "in"))

grid.picture(SVGlemming,  x = 0.085, y = 0.11,
             width = grid::unit(1.21, "in"), height = grid::unit(1.1, "in"))


grid.picture(SVGowl_dashed,  x = 0.055, y = 0.17,
             width = grid::unit(2.2, "in"), height = grid::unit(2.2, "in"))

grid.picture(SVGowl_black,  x = 0.21, y = 0.17,
             width = grid::unit(2.2, "in"), height = grid::unit(2.2, "in"))

mtext("C)", adj = -0.01, line = -4, cex = 3)

mtext("States and switches", adj = 2.1, line = 4, cex = 4)


# Create right plot
plot.new()


plotrix::draw.circle(x = 0.1875, y = 0.5, radius = 0.2, border = color[3], lwd = 8)
plotrix::draw.circle(x = 0.8125, y = 0.5, radius = 0.2, border = color[2], lwd = 8)


iArrows(x1 = 0.3, y1 = 0.85, x2 = 0.7, y2 = 0.85,
        h.lwd = 4, sh.lwd = 4, curve = 0.5,
        width = 4, size = 1)


iArrows(x1 = 0.7, y1 = 0.15, x2 = 0.3, y2 = 0.15,
        h.lwd = 4, sh.lwd = 4, curve = 0.5,
        width = 4, size = 1)


# Plot svg
text("$\\geq L^*$", x = 0.82, y = 0.95, cex = 5)
text("$< L^*$", x = 0.18, y = 0.95, cex = 5)


grid.picture(SVGlemming,   x = 0.37, y = 0.11,
             width = grid::unit(1.21, "in"), height = grid::unit(1.1, "in"))

grid.picture(SVGlemming,  x = 0.525, y = 0.11,
             width = grid::unit(1.21, "in"), height = grid::unit(1.1, "in"))

grid.picture(SVGfox,  x = 0.345, y = 0.16,
             width = grid::unit(4, "in"), height = grid::unit(1.25, "in"))

grid.picture(SVGfox_family,  x = 0.5, y = 0.16,
             width = grid::unit(4, "in"), height = grid::unit(1.25, "in"))

mtext("F)", adj = -0.08, line = -4, cex = 3)


#------------------------#
#    Time series plot    #
#------------------------#



#------------------------------------------#
#    Create discrete lemming time serie    #
#------------------------------------------#

# lenght (years) of the simulation
L <- numeric(7)
# Starting lemming density
L[1] <- 0.1


# Realize growth during winter month
r1 <- 1.46839
# Realize growth during summer month (low predation pressure)
r2 <- 0.2
# Realize growth during summer month (high predation pressure)
r3 <- -18.22068

# Winter season length
q <- 0.75


# Realize growth for a year of low predation pressure
R1 <- exp(r1*q)*exp(r2*(1-q))
# Realize growth for a year of high predation pressure
R2 <- exp(r1*q)*exp(r3*(1-q))


# Lemming threshold density for high predation pressure
Lstar <- 2


for(i in 2:length(L)){
  if (L[i-1] < Lstar) {L[i] <- L[i-1]*R1
  
  } else if (L[i-1] >= Lstar) {L[i] <- L[i-1]*R2} 
}



#------------------------------------------------------#
#    Create continuous seasonal lemming time series    #
#------------------------------------------------------#

model1 <- function (time, y, parms) {
  
  with(as.list(c(y, parms)), {
    dN <- N * r1
    list(dN)
  })
}

model2 <- function (time, y, parms) {
  
  with(as.list(c(y, parms)), {
    dN <- N * r2
    list(dN)
  })
}

model3 <- function (time, y, parms) {
  
  with(as.list(c(y, parms)), {
    dN <- N * r3
    list(dN)
  })
}


parms <- c(r1 = r1, r2 = r2, r3 = r3)
times <- seq(1, 7, 0.01)

out <- data.frame(time = 1, N = 0.1)

winter <- data.frame(time = NA, N = NA)
summer.low <- data.frame(time = NA, N = NA)
summer.high <- data.frame(time = NA, N = NA)

break.pt <- c(1, 1.25, 2, 2.25, 3, 3.25, 4, 4.25, 5, 5.25, 6, 6.25)



for (i in break.pt) {
  if(i == 4){
    temp <- ode(y = c(N = out[nrow(out), 2]), 
                times[which(times == i): which(times == (i + 0.25))],
                model3,
                parms)
    
    summer.high <- rbind(summer.high, temp, rep(NA, 3))
  }
  
  if(i %in% c(1,2,3,5,6)){
    temp <- ode(y = c(N = out[nrow(out), 2]),
                times[which(times == i): which(times == (i + 0.25))],
                model2,
                parms)
    
    summer.low <- rbind(summer.low, temp, rep(NA, 3))
  }
  
  if(i %in% c(1.25:6.25)){
    temp <- ode(y = c(N = out[nrow(out), 2]), 
                times[which(times == i): which(times == (i + 0.75))],
                model1,
                parms)
    
    winter <- rbind(winter, temp, rep(NA, 3))
  }
  
  out <- rbind(out, temp[-1,])
}



#----------------------#
#    Load svg image    #
#----------------------#

# Convert svg file to cairo svg file format
# rsvg_svg(svg = "svg/lemming_grey.svg", file = "svg/lemming_grey_cairo.svg")
rsvg_svg(svg = "svg/lt_jaeger_grey_2.svg", file = "svg/lt_jaeger_grey_2_cairo.svg")
rsvg_svg(svg = "svg/lt_jaeger_dashed_2.svg", file = "svg/lt_jaeger_dashed_2_cairo.svg")

# Read svg
SVGlemming_grey <- readPicture("svg/lemming_grey_cairo.svg")
SVGjaeger_grey <- readPicture("svg/lt_jaeger_grey_2_cairo.svg")
SVGjaeger_dashed <- readPicture("svg/lt_jaeger_dashed_2_cairo.svg")


#---------------#
# Create plot   #
#---------------#

par(mar = c(37.3, 7.1, 37.3, 5.1), mgp = c(3, 3, 0))

plot(L, type = "l", bty = "L",
     yaxt = "n", xaxt = "n",
     ylab =""  , xlab = "",
     ylim = c(0, 3.8), lwd = 6,
     axes = FALSE)


axis(1, cex.axis = 4, at = c(0:7), labels = c(NA, 0:6), lwd = 3, lwd.ticks = 3)
axis(2, lwd.ticks = 0, labels = NA, at = c(-1:4), lwd = 3)


mtext("Lemming\ndensity", side = 2, line = 2, cex = 3, at = 3.3)
mtext("Years", side = 1, line = 7, cex = 3, at = 7)

mtext("$L^*$", side = 2, line = 1, cex = 4, las = 1, at = 2.3)


abline(h = 2.3, lty = 2, lwd = 5)


lines(winter, col = color[3], lwd = 4)
lines(summer.low, col = color[2], lwd = 4)
lines(summer.high, col = color[1], lwd = 4)


transition <- out[which(out$time %in% break.pt), ]

arrows(transition[4, 1], 0.1, transition[5, 1], 0.1, lwd = 4, angle = 90, code = 3, length = 0.1)

points(transition, pch = 19, cex = 3, 
       col = c(rep(color[2:3], 3),
               color[c(1,3)],
               rep(color[2:3], 2)))


lines(x = c(1.5, 2.5), y = c(1.1, 0.6581202), lwd = 5, col = "grey")
lines(x = c(5.5, 4.5), y = c(2.5, 1.631166), lwd = 5, col = "grey")


plotrix::draw.circle(1.5, 1.1, 0.6, lwd = 7, col = "white", border = "grey")
plotrix::draw.circle(5.5, 2.5, 0.6, lwd = 7, col = "white", border = "grey")


text(labels = c("$r_1$ = winter", "$r_2$ = low summer", "$r_3$ = high summer"),
     x = rep(1, 3), y = c(3.8, 3.5, 3.2), col = color[3:1], cex = 5, pos = 4)

text("$q$", x = 2.625, y = 0.25, cex = 4)


# # Don't delete lines under this
# text("$R_1 = e^{r_2 (1-q)} e^{r_1 q}$", x = 1.9, y = 1.8, cex = 3)
# text("$R_2 = e^{r_3 (1-q)} e^{r_1 q}$", x = 5.7, y = 3.2, cex = 3)
# # Used the above line to get the latex code written below!


tikzAnnotate(c('\\definecolor{winter}{RGB}{33,176,192}', # Define color for the color insertion
               '\\definecolor{low_summer}{RGB}{218,53,47}',
               '\\definecolor{high_summer}{RGB}{224,150,0}',

               # Fist formula in tex synthax for color insertion
               '\\node[text=drawColor,anchor=base,inner sep=0pt, outer sep=0pt, scale=  3.5] at (1348.12,628.18)',
               '{$R_1 = e^{\\textcolor{low_summer}{r_2} (1-q)} e^{\\textcolor{winter}{r_1}q}$};',
               # \textcolor allow to color part of the text. Double \ for escape

               # Second formula
               '\\node[text=drawColor,anchor=base,inner sep=0pt, outer sep=0pt, scale=  3.5] at (1800.03,870.39)',
               '{$R_2 = e^{\\textcolor{high_summer}{r_3} (1-q)} e^{\\textcolor{winter}{r_1}q}$};')
)


# Add icon image in circles
grid.picture(SVGlemming_grey,  x = 0.66, y = 0.375,
             width = grid::unit(1.098, "in"), height = grid::unit(0.99, "in"))

grid.picture(SVGjaeger_dashed,  x = 0.64, y = 0.40,
             width = grid::unit(2.34, "in"), height = grid::unit(1.62, "in"))

grid.picture(SVGlemming_grey,  x = 0.89, y = 0.56,
             width = grid::unit(1.089, "in"), height = grid::unit(0.99, "in"))

grid.picture(SVGjaeger_grey,  x = 0.87, y = 0.585,
             width = grid::unit(2.34, "in"), height = grid::unit(1.62, "in"))

mtext("G)", adj = -0.1, line = 5, cex = 3)


dev.off()
