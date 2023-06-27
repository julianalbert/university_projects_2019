##
#
# Author: Julian Albert
# Date: 09 September 2019
#
# Description:
# Machine Learning Assignment 1 consists of two problems, 
# the first is to do target functions and coefficient approximation using order
# polynomials. The second considers candidate hypothesis and explores problems
# with overfitting.
#
#------------------------------------------------------------------------------#

# 0. Clean Workspace, Directory and Library ----

## Clean Workspace
rm(list=ls())
dev.off() # Close Plots
setwd("~") # Clear Path to User

## Locations
project_folder <- "/Documents/UCT/Coursework/MachineLearning"
loc_script <- "/Assignment_1/UCT_Assignment/Code/MachineLearning_Ass1"
loc_figs <- "/Assignment_1/UCT_Assignment/Figs"

## Directories
dir_script <- paste("~", project_folder, loc_script, sep = '')
dir_figs <- paste("~", project_folder, loc_figs, sep = '')

## Set Working Directory to Script Location
setwd(dir_script)

## Libraries - Lazyload
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, data.table, Cairo, ggplot2, viridis)

# 1. Problem 1 ----

func.tmp_of_k <- function(k, x, q) # k in q, x is data, q is order of polynomial
{
  x^k * choose(q, k) * choose( (q+k-1)/2, q)
}

func.legendre_p1 <- function(q, x)
{
  
  k <- as.matrix(0:q, ncol = 1) # to have dimension qx1

  tmp.rhs <- apply(k, 1, func.tmp_of_k, x, q) # to each k
  Lq <- (2^q) * rowSums(tmp.rhs) # Lq(x)
  
  return(Lq)
}

nx <- 1000 # smooth polynomials require lots of x's
nk <- 5 # assignment wants q = 0, 1, ..., 5

x <- seq(-1, 1, length.out = nx)
q <- as.matrix(seq(0, nk, by = 1), ncol = 1)

res.Lq <- apply(q, 1, func.legendre_p1, x) # to each q get Lq(x)
# round(res.Lq, 3)
colour_vec <- c("dodgerblue3", "firebrick2", "forestgreen", "gold", 
                "darkorange", "darkorchid3") # colour for pretty plot

# i) plot Lq over -1, 1 for different value of q

setwd(dir_figs)
cairo_pdf("ML_Ass1_fig_Lq_for_diff_q.pdf", width = 5, height = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(res.Lq[, 1], ylim = c(-1, 1), xlab = "x", ylab = expression(L[q](x)),
     main = expression(paste(L[q](x), " for Different Values of q")),
     type = "l", col = colour_vec[1], lwd = 2) # titles
for(i in 2:dim(res.Lq)[2]){lines(res.Lq[, i], col = colour_vec[i], lwd = 2)}
legend("topright", title = expression(paste(bold("Value of q"))), 
       inset = c(-0.4, 0), legend = c("0", "1", "2", "3", "4", "5"), 
       col = colour_vec, lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       y.intersp = 1, x.intersp = 0.5, seg.len = 0.5) # legend stuff
dev.off() # turn-off plot

# ii) plot using two equations. Polynomial vs Legendre, 3 targets
x <- seq(-1, 1, length.out = 1000)

## creates random polynomial target functions > Using equation 2
func.poly_target <- function(x, nk)
{
  alpha_vec <- runif(nk + 1, -1, 1) # represent q = 0, 1, ..., nk
  target_f <- 0
  
  for(i in 1:(nk + 1)){
    target_f <- target_f + alpha_vec[i] * x^(i-1)
  }
  
  return(target_f)
  
}

cairo_pdf("ML_Ass1_fig_Polynomials.pdf", width = 12, height = 4)
layout(matrix(1:3, 1, 3, byrow = TRUE), respect = TRUE)
plot(x, 1.5*x, type = "n", ylab = expression(f(x)), main = "Polynomial of Order 2")
for(i in 1:3) {lines(x, func.poly_target(x, 2), col = colour_vec[1])}
plot(x, 1.5*x, type = "n", ylab = expression(f(x)), main = "Polynomial of Order 4")
for(i in 1:3) {lines(x, func.poly_target(x, 4), col = colour_vec[2])}
plot(x, 1.5*x, type = "n", ylab = expression(f(x)), main = "Polynomial of Order 10")
for(i in 1:3) {lines(x, func.poly_target(x, 10), col = colour_vec[3])}
dev.off()

cairo_pdf("ML_Ass1_fig_Poly_for_diff_q_wx.pdf", width = 5, height = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(x, 1.5*x, type = "n", ylab = expression(f(x)),
     main = "Polynomials of Order q")
for(i in 1:length(colour_vec)){lines(x, func.poly_target(x, i), col = colour_vec[i])}
legend("topright", title = expression(paste(bold("Value of q"))), 
       inset = c(-0.4, 0), legend = c("1", "2", "3", "4", "5", "6"), 
       col = colour_vec, lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       y.intersp = 1, x.intersp = 0.5, seg.len = 0.5) # legend stuff
dev.off() # turn-off plot

# creates random Legendre target functions > Using equation 3
func.legendre_target <- function(x, nk)
{
  beta_vec <- runif(nk + 1, -1, 1) # represent q = 0, 1, ..., nk
  target_f <- 0
  
  for(i in 1:(nk + 1)){
    target_f <- target_f + beta_vec[i] * func.legendre_p1((i-1), x)
  }
  
  return(target_f)
  
}

cairo_pdf("ML_Ass1_fig_LegendrePolynomials.pdf", width = 12, height = 4)
layout(matrix(1:3, 1, 3, byrow = TRUE), respect = TRUE)
plot(x, 1.5*x, type = "n", ylab = expression(f(x)), main = "Legendre Polynomial of Order 2")
for(i in 1:3) {lines(x, func.legendre_target(x, 2), col = colour_vec[1])}
plot(x, 1.5*x, type = "n", ylab = expression(f(x)), main = "Legendre Polynomial of Order 4")
for(i in 1:3) {lines(x, func.legendre_target(x, 4), col = colour_vec[2])}
plot(x, 1.5*x, type = "n", ylab = expression(f(x)), main = "Legendre Polynomial of Order 10")
for(i in 1:3) {lines(x, func.legendre_target(x, 10), col = colour_vec[3])}
dev.off() # turn-off plot

cairo_pdf("ML_Ass1_fig_LegPoly_for_diff_q_wx.pdf", width = 5, height = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(x, 1.5*x, type = "n", ylab = expression(f(x)),
     main = "Legendre Polynomials of Order q")
for(i in 1:length(colour_vec)){lines(x, func.legendre_target(x, i), col = colour_vec[i])}
legend("topright", title = expression(paste(bold("Value of q"))), 
       inset = c(-0.4, 0), legend = c("1", "2", "3", "4", "5", "6"), 
       col = colour_vec, lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       y.intersp = 1, x.intersp = 0.5, seg.len = 0.5) # legend stuff
dev.off() # turn-off plot

setwd(dir_script)

# 2. Problem 2 ----

## Generate 10-th order target using legendre over -1, 1
generator <- function(n, # Dataset Size
                      x, # X values for the Data
                      targetfunction,
                      sigma # the sd of noise
                      )
{
  
  l <- length(targetfunction) # how many data points there are in the target
  dat <- matrix(rep(NA, 2*n), ncol = n) # N columns of data, 2 rows for X and Y
  xdat_indices <- sample(1:l, n) # get n indices for data
  ydat <- targetfunction[xdat_indices] + rnorm(n, 0, sigma) # y =fx + e
  xdat <- x[xdat_indices] # x = x[indexed]
  Data <- data.frame(xdat, ydat)
  
  return(Data)
  
}

#fitted model of the data, in this case lm give B0 + B1X1 + B2X^2
func.fitted_model <- function(x, model)
{
  
  fitted_model <- 0
  coeff_vec <- as.numeric(model$coefficient)
  
  for(i in 1:length(model$coefficient)){
    fitted_model <- fitted_model + (coeff_vec[i]*(x^(i-1)))
  }
  
  return(fitted_model)
}

# gives the bias for a given fitted model >> (g-f)^2 / N
func.fit_target_bias <- function(x, target, model)
{
  
  fitted_model <- func.fitted_model(x, model)
  bias <- (t(fitted_model - target) %*% (fitted_model - target))/(length(x))
  
  return(bias)
}

# Test Case
x_dat <- seq(-1, 1, 0.01)
sigma <- 0.5
n <- 15
targetfunction <- func.legendre_target(x_dat, 10)

setwd(dir_figs)

cairo_pdf("ML_Ass1_fig_Prob2i_fits.pdf", width = 12, height = 4)
layout(matrix(1:3, 1, 3, byrow = TRUE), respect = TRUE)
for(i in 1:3){
plot(c(-1, 1), c(-4, 4), main = "",
     type = "n", xlab = "x", ylab=expression(f(x)))
lines(x_dat, targetfunction, type = "l", lwd = 2) # target function
data_gen <- generator(n, x_dat, targetfunction, sigma) # generated data
points(data_gen$xdat, data_gen$ydat) # plot points

model_2 <- lm(data_gen$ydat ~ data_gen$xdat + I(data_gen$xdat^2)) #quadratic fit
# IS and OOS erros
err.in_2 <- (t(model_2$residuals)%*%model_2$residuals)/n
err.out_2 <- func.fit_target_bias(x_dat, targetfunction, model_2) + (sigma^2)
lines(x_dat, func.fitted_model(x_dat, model_2), 
      lty = 2, col = "blue", lwd = 1) # plot lines for fitted quadratic
# Labels for IS, OOS
text1 = bquote(italic(E)["in"](x^2) == .(format(err.in_2, digits = 3)))
text2 = bquote(italic(E)["out"](x^2) == .(format(err.out_2, digits = 3)))
text(x=-0.7,y=3.7,labels=text1)
text(x=-0.7,y=3.2,labels=text2)

model_10 <- lm(data_gen$ydat ~ data_gen$xdat + I(data_gen$xdat^2) + 
                 I(data_gen$xdat^3) + I(data_gen$xdat^4) +I(data_gen$xdat^5) + 
                 I(data_gen$xdat^6) + I(data_gen$xdat^7) + I(data_gen$xdat^8) +
                   I(data_gen$xdat^9) + I(data_gen$xdat^10)) # 10-th order
# IS and OOS erros
err.in_10 <- (t(model_10$residuals)%*%model_10$residuals)/n 
err.out_10 <- func.fit_target_bias(x_dat, targetfunction, model_10) + (sigma^2)
lines(x_dat, func.fitted_model(x_dat, model_10), 
      lty = 2, col = "red", lwd = 1) # plot lines for fitted 10-th order
# Labels for IS, OOS
text3 = bquote(italic(E)["in"](x^10) == .(format(err.in_10, digits = 3)))
text4 = bquote(italic(E)["out"](x^10) == .(format(err.out_10, digits = 3)))
text(x=0,y=3.7,labels=text3)
text(x=0,y=3,labels=text4)
# Overfit Measure and Labels for it
overfit_measure <- err.out_10 - err.out_2
text5 = bquote("["~italic(E)["out"](x^10) - italic(E)["in"](x^2)~"]" == .(format(overfit_measure, digits = 3)))
text(x=-0.35,y=2.3,labels=text5)
}
dev.off()

setwd(dir_script)

## Need a function that takes in sigma and n to make colour map
func.overfit_measure <- function(sigma, n, k, order_q, delta)
{
  
  overfit <- numeric() # initialise
  targetfunction <- func.legendre_target(x_dat, order_q) # same target
  
  # Generate k times to take average
  for(i in 1:k){

  data_gen <- generator(n, x_dat, targetfunction, sigma) # generate data
  
  # fit 2nd order >> get IS, OOS
  model_2 <- lm(data_gen$ydat ~ data_gen$xdat + I(data_gen$xdat^2))
  err.in_2 <- (t(model_2$residuals)%*%model_2$residuals)/n 
  err.out_2 <- func.fit_target_bias(x_dat, targetfunction, model_2) + (sigma^2)
  
  # fit 10-th order >> get IS, OOS
  model_10 <- lm(data_gen$ydat ~ data_gen$xdat + I(data_gen$xdat^2) + 
                   I(data_gen$xdat^3) + I(data_gen$xdat^4) +I(data_gen$xdat^5) + 
                   I(data_gen$xdat^6) + I(data_gen$xdat^7) + I(data_gen$xdat^8) +
                   I(data_gen$xdat^9) + I(data_gen$xdat^10))
  err.in_10 <- (t(model_10$residuals)%*%model_10$residuals)/n # In sample error for the quadratic model
  err.out_10 <- func.fit_target_bias(x_dat, targetfunction, model_10) + (sigma^2) # Out of sample error
  
  overfit[i] <- err.out_10 - err.out_2   # store overit measure
  }
  
  overfit <- ifelse(overfit > delta, delta, overfit) # error threshold
  overfit <- ifelse(overfit < -delta, -delta, overfit) # error threshold
  overfit_measure <- mean(overfit)
  
  return(list(E2in = err.in_2, E2out = err.out_2,
              E10in = err.in_10, E10out = err.out_10,
              Overfit = overfit_measure))
}

x_dat <- seq(-1, 1, 0.01)
sigma_vector <- seq(0.2, 1.1, length.out = 21)[-21]
N_vector <- seq(20, 110, by = 1)
grid <- expand.grid(N_vector, sigma_vector)

dat_prob2i <- apply(grid, 1, function(x){
  func.overfit_measure(sigma = x[2], n = x[1], 
                       k = 50, order_q = 10, delta = 0.2)
})

dat_prob2i <- bind_rows(dat_prob2i)
grid$Overfit <- dat_prob2i$Overfit

setwd(dir_figs)
cairo_pdf("ML_Ass1_fig_Prob2i_colourmap.pdf", width = 5, height = 5)
ggplot(data = grid, aes(x = Var1, y = Var2, fill = Overfit)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = bquote("\n Dataset Size ( N )"), 
       y = bquote("Noise Level ("~sigma~")"~"\n"),
       title = "Colour Map of Overfit Measure",
       subtitle = bquote("Relative Performances of" ~ 2^"nd" ~ "and" ~ 10^"th" ~ "Order Polynomials")) +
  theme(aspect.ratio = 0.75)
dev.off()

## Problem 2ii

x_dat <- seq(-1, 1, 0.01)
Qf_vector <- seq(1, 40, by = 1)
N_vector <- seq(20, 60, by = 1)
grid_ii <- expand.grid(N_vector, Qf_vector)

dat_prob2ii <- apply(grid_ii, 1, function(x){
  func.overfit_measure(sigma = 0.2, n = x[1], 
                       k = 50, order_q = x[2], delta = 0.2)
})

dat_prob2ii <- bind_rows(dat_prob2ii)
grid_ii$Overfit <- dat_prob2ii$Overfit

cairo_pdf("ML_Ass1_fig_Prob2ii_colourmap.pdf", width = 5, height = 5)
ggplot(data = grid_ii, aes(x = Var1, y = Var2, fill = Overfit)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = bquote("\n Dataset Size ( N )"), 
       y = bquote("Order of Target Functions ("~"Q"[f]~")"~"\n"), title = "") +
  theme(aspect.ratio = 0.75)
dev.off()

setwd(dir_script)
