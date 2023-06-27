##
#
# Author: Julian Albert
# Date: 22 September 2019
#
# Description:
# ML Assignment 2 - Regularisation, validation set size and image eigenfaces.
#
#------------------------------------------------------------------------------#

# 0. Clean Workspace, Directory and Library ----

## Clean Workspace
rm(list=ls())
dev.off() # Close Plots
setwd("~") # Clear Path to User

## Locations
project_folder <- "/Documents/UCT/Coursework/MachineLearning"
loc_script <- "/Assignment_2/UCT_Assignment/Code/MachineLearning_Ass2"
loc_figs <- "/Assignment_2/UCT_Assignment/Figs"
loc_data <- "/Assignment_2/UCT_Assignment/Code/MachineLearning_Ass2/Faces"

## Data File name
dat_faces_filen <- numeric()
for(i in 1:400) dat_faces_filen[i] <- paste(i, ".pgm", sep = "")

## Directories
dir_script <- paste("~", project_folder, loc_script, sep = '')
dir_figs <- paste("~", project_folder, loc_figs, sep = '')
dir_data <- paste("~", project_folder, loc_data, sep = '')

## Set Working Directory to Script Location
setwd(dir_script)

## Libraries - Lazyload
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, data.table, Cairo, parallel, doParallel, pixmap, rARPACK)

colour_vec <- c("dodgerblue3", "firebrick2", "forestgreen", "gold", 
                "darkorange", "darkorchid3") # colour for pretty plot

# Problem 1 ----

# Simulate a Dataset

set.seed(123)
N <- 30
x_seq <- seq(-1, 1, length.out = N)
epsilon <- rnorm(N, 0, 1)
x_dat <- runif(N, -1, 1)
y_dat <- 0.8*x_dat + epsilon
data <- as.data.frame(cbind(x_dat, y_dat))

# fit models g1 and g2
g1_betas <- lm(y_dat ~ 0 + x_dat, offset = rep(+0.5, length(x_dat)))$coefficients
g2_betas <- lm(y_dat ~ 0 + x_dat, offset = rep(-0.5, length(x_dat)))$coefficients
g1_pred <- +0.5 + g1_betas*x_seq
g2_pred <- -0.5 + g2_betas*x_seq

## i) plot models and fits
setwd(dir_figs)
cairo_pdf("ML_Ass2_fig_P1_modelfits.pdf", height = 5, width = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(data$y_dat~data$x_dat, xlab = "x", ylab = "f(x)")
lines(x_seq, 0.8*x_seq, col = 'black', lwd = 2)
lines(g1_pred ~ x_seq, col = colour_vec[1], lwd = 1.5)
lines(g2_pred ~ x_seq, col = colour_vec[2], lwd = 1.5)  
legend("topright", title = expression(paste(bold("Model"))), 
       inset = c(-0.35, 0), lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       legend = c(expression(y), expression(g[1](x)), expression(g[2](x))), 
       col = c('black', colour_vec[1:2]), y.intersp = 1.5, x.intersp = 0.4, 
       seg.len = 0.8)
dev.off()

# from the plot data seems to have upper and lower band, both models same?

## i) What if we fitted model 1000 times?
N_dat <- 30
N_sims <- 1000
x_seq <- seq(-1, 1, length.out = N_dat)
epsilon <- rnorm(N_dat, 0, 1)
x_dat <- runif(N_dat, -1, 1)
y_dat <- 0.8*x_dat + epsilon
data <- as.data.frame(cbind(x_dat, y_dat))
model <- 0.8*x_seq

g1_mat_pred <- matrix(NA, N_sims, N_dat)
g2_mat_pred <- matrix(NA, N_sims, N_dat)

for(i in 1:N_sims){
  
  x_dat <- runif(N_dat, -1, 1)
  epsilon <- rnorm(N_dat, 0, 1)
  y_dat <- 0.8*x_dat + epsilon

  # fit models g1 and g2
  g1_betas <- lm(y_dat ~ 0 + x_dat, offset = rep(+0.5, length(x_dat)))$coefficients
  g2_betas <- lm(y_dat ~ 0 + x_dat, offset = rep(-0.5, length(x_dat)))$coefficients
  g1_mat_pred[i, ] <- +0.5 + g1_betas*x_seq
  g2_mat_pred[i, ] <- -0.5 + g2_betas*x_seq
  
}

g1_pred_means <- colMeans(g1_mat_pred)
g2_pred_means <- colMeans(g2_mat_pred)

cairo_pdf("ML_Ass2_fig_P1_mean_modelfits.pdf", height = 5, width = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(x_seq, model, lwd = 2, type = 'l', xlab = "x", ylab = "f(x)",
     ylim = c(min(g2_pred_means), max(g1_pred_means)))
lines(x_seq, g1_pred_means, lty = "dashed", col = colour_vec[1])
lines(x_seq, g2_pred_means, lty = "dashed", col = colour_vec[2])
legend("topright", title = expression(paste(bold("Model"))), 
       inset = c(-0.35, 0), lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       legend = c(expression(y), bquote(bar(g[1](x))), bquote(bar(g[2](x)))), 
       col = c('black', colour_vec[1:2]), y.intersp = 1.5, x.intersp = 0.4, 
       seg.len = 0.8)
dev.off()

## ii)
  
# Simulate 10'000 datasets of y size = 30
n_datasets <- 10000
x_seq <- seq(-1, 1, length.out = 1000)
true_model <- 0.8*x_seq

func.sim_dataset <- function(n_datasets, N)
{
  # initialise data storage
  data_df <- list()
  # gen x, y
  for(i in 1:n_datasets){
    x_dat <- runif(N, -1, 1)
    epsilon <- rnorm(N, 0, 1)
    y_dat <- 0.8*x_dat + epsilon
    data_df[[i]] <- data.frame(X = x_dat, Y = y_dat)
  }
  return(data_df)
}

dat_datasets <- func.sim_dataset(n_datasets, N)
## Calc Eval and Eout
func.Eval_Eout <- function(data, i_start, i_end, x_seq, true_model)
{
  
  ## Initialise Storage... 
  ## Need a matrix of Eval and Eout for each i
  mat_of_errors <- matrix(0, nrow = length(i_start:i_end), ncol = 2)
  
  # For Dval of size 5, 6, ..., 25 and Dtrain of size 25, 24, ..., 5
  for(i in i_start:i_end)
  {
    # Subset data into training and validation
    idx <- sample(1:NROW(data), i, replace = FALSE)
    data_validation <- data[idx, ]
    data_train <- data[-idx, ]
    # fit models g1 and g2
    g1_betas <- lm(Y ~ 0 + X, offset = rep(+0.5, length(X)), data_train)$coefficients
    g2_betas <- lm(Y ~ 0 + X, offset = rep(-0.5, length(X)), data_train)$coefficients
    g1_pred <- +0.5 + g1_betas*data_validation$X
    g2_pred <- -0.5 + g2_betas*data_validation$X
    # Calculate 1/n * (Y - g(Y)_val)^2
    MSE_g1 <- mean((data_validation$Y - g1_pred)^2)
    MSE_g2 <- mean((data_validation$Y - g2_pred)^2)
    
    E_val <- min(MSE_g1, MSE_g2)
    
    # select model with lower MSE
    if(MSE_g1 < MSE_g2){
      gstar_betas <- g1_betas
      gstar_pred <- +0.5 + gstar_betas*(x_seq)
    } else {
      gstar_betas <- g2_betas
      gstar_pred <- -0.5 + gstar_betas*(x_seq)
    }
    
    # Calculate OOS error and store
    E_out <- mean( (gstar_pred - true_model)^2 ) + 1# ?
    mat_of_errors[(i-i_start+1), ] <- c(E_val, E_out)
  }
  
  # Name columns to not get confused by which measure is which
  dat_erros_val_and_oos <- as.data.frame(mat_of_errors)
  names(dat_erros_val_and_oos) <- c("E_val", "E_out")
  
  return(dat_erros_val_and_oos)
}

## Parallel, 21 linear models over 10'000 datasets = 210'000 linear models
system.time(
dat_Errors <- mclapply(dat_datasets, # list
                       func.Eval_Eout, i_start = 5, i_end = 25,
                       x_seq = x_seq, true_model = true_model, # function & pars
                       mc.cores = detectCores()) # how many cores
)

# Select Eval and Eout to plot
Evals <- lapply(dat_Errors, function(x) x$E_val)
Eouts <- lapply(dat_Errors, function(x) x$E_out)
Evals_Mean <- bind_cols(Evals) %>% rowMeans()
Eouts_Mean <- bind_cols(Eouts) %>% rowMeans()

cairo_pdf("ML_Ass2_fig_P1_EvalEout.pdf", height = 5, width = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(smooth.spline(y = Evals_Mean, x = 5:25), type = 'l', col = colour_vec[5],
     ylim = c(min(Evals_Mean), max(Eouts_Mean)), lwd = 1.5,
     xlab = "Validation Set Size", ylab = 'Expected Error')
lines(smooth.spline(y = Eouts_Mean, x = 5:25), col = colour_vec[6], lwd = 1.5)
legend("topright", title = expression(paste(bold("Error"))), 
       inset = c(-0.35, 0), lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       legend = c(expression(E[val]), expression(E[out])), 
       col = colour_vec[5:6], y.intersp = 1.5, x.intersp = 0.4, 
       seg.len = 0.8)
dev.off()

# Why between 5:25? what happens over full range cant have val of size 30 in lm()
system.time(
  dat_Errors <- mclapply(dat_datasets, # list
                         func.Eval_Eout, i_start = 1, i_end = 29,
                         x_seq = x_seq, true_model = true_model, # function & pars
                         mc.cores = detectCores()) # how many cores
)

## What happens to uncertainty around Eval?
Evals <- lapply(dat_Errors, function(x) x$E_val)
Evals_Mean <- bind_cols(Evals) %>% rowMeans()
Evals_Mean <- Evals_Mean[-29]
Evals_sd <- bind_cols(Evals) %>% apply(1, function(x) sd(x))
Evals_sd <- Evals_sd[-29]
cairo_pdf("ML_Ass2_fig_P1_EvalwSigma_128.pdf", height = 5, width = 5)
plot(x = c(1, 28), y = c(min(Evals_Mean-Evals_sd), max(Evals_Mean+Evals_sd)), type = 'n',
     xlab = "Validation Set Size", ylab = 'Expected Error', yaxs = 'i', xaxs = 'i')
polygon(c(1:28, rev(1:28)), c( (Evals_Mean-Evals_sd), rev(Evals_Mean+Evals_sd) ),
        col = "grey85")
lines(smooth.spline(y = Evals_Mean, x = 1:28), col = colour_vec[5], lwd = 2)
lines(smooth.spline(y = Evals_Mean+Evals_sd, x = 1:28), lwd = 2)
lines(smooth.spline(y = Evals_Mean-Evals_sd, x = 1:28), lwd = 2)
dev.off()

Evals <- lapply(dat_Errors, function(x) x$E_val)
Evals_Mean <- bind_cols(Evals) %>% rowMeans()
Evals_Mean <- Evals_Mean[-c(27, 28, 29)]
Evals_sd <- bind_cols(Evals) %>% apply(1, function(x) sd(x))
Evals_sd <- Evals_sd[-c(27, 28, 29)]
cairo_pdf("ML_Ass2_fig_P1_EvalwSigma_126.pdf", height = 5, width = 5)
plot(x = c(1, 26), y = c(min(Evals_Mean-Evals_sd), max(Evals_Mean+Evals_sd)), type = 'n',
     xlab = "Validation Set Size", ylab = 'Expected Error', yaxs = 'i', xaxs = 'i')
polygon(c(1:26, rev(1:26)), c( (Evals_Mean-Evals_sd), rev(Evals_Mean+Evals_sd) ),
        col = "grey85")
lines(smooth.spline(y = Evals_Mean, x = 1:26), col = colour_vec[5], lwd = 2)
lines(smooth.spline(y = Evals_Mean+Evals_sd, x = 1:26), lwd = 2)
lines(smooth.spline(y = Evals_Mean-Evals_sd, x = 1:26), lwd = 2)
dev.off()

Evals <- lapply(dat_Errors, function(x) x$E_val)
Evals_Mean <- bind_cols(Evals) %>% rowMeans()
Evals_Mean <- Evals_Mean[-c(1:4, 26:29)]
Evals_sd <- bind_cols(Evals) %>% apply(1, function(x) sd(x))
Evals_sd <- Evals_sd[-c(1:4, 26:29)]

cairo_pdf("ML_Ass2_fig_P1_EvalwSigma_525.pdf", height = 5, width = 5)
plot(x = c(5, 25), y = c(min(Evals_Mean-Evals_sd), max(Evals_Mean+Evals_sd)), type = 'n',
     xlab = "Validation Set Size", ylab = 'Expected Error', yaxs = 'i', xaxs = 'i')
polygon(c(5:25, rev(5:25)), c( (Evals_Mean-Evals_sd), rev(Evals_Mean+Evals_sd) ),
        col = "grey85")
lines(smooth.spline(y = Evals_Mean, x = 5:25), col = colour_vec[5], lwd = 2)
lines(smooth.spline(y = Evals_Mean+Evals_sd, x = 5:25), lwd = 2)
lines(smooth.spline(y = Evals_Mean-Evals_sd, x = 5:25), lwd = 2)
dev.off()

# Problem 2 ----

## Simulate dataset
set.seed(123)
N = 50
x_seq <- seq(-1, 1, 0.01)
x_dat <- runif(N, -1, 1)
epsilon <- rnorm(N, 0, 1)
y_dat <- sin(pi*x_dat) + epsilon
data <- as.data.frame(cbind(x_dat, y_dat))
model <- sin(pi*x_seq)
Qf <- 10

setwd(dir_figs)
cairo_pdf("ML_Ass2_fig_P2_sim_model_init.pdf", height = 5, width = 5)
plot(x_seq, model, type = 'l', lwd = 2, xlab = "x", ylab = "f(x)",
     ylim = c(min(y_dat), max(y_dat)))
points(data)
dev.off()

## Legendre polynomial function
func.Legendre <- function(x, q)
{
  Lq = 0
  for(i in 0:q){
    Lq = Lq + ((x^i)*choose(q, i)*choose((q+i-1)/2, q))
  }
  return((2^q)*Lq)
}

## Target function with q-th order Legendre Polynomial
func.rand_Legfunc <- function(x, q)
{
  fx = 0
  beta_vec = runif(q+1, -1, 1)
  for(i in 0:q){
    fx = fx + (func.Legendre(x, i)*beta_vec[(i+1)])
  }
  return(fx)
}

### Takes X -> Z maps to Z... z = [1 L1 L2 ... LQ]'
### Z = [z1 z2 ... zN]' where z1 = [1 1 ... 1]', z2 = [L1(x1) L1(x2) ... L1(x3)]'

## Fit model
func.fit_model <- function(X, Y, Qf, lambda = 0)
{
  # initialise Z
  Z <- matrix(0, nrow = NROW(X), ncol = (Qf+1))
  # Lq for each column
  for(i in 0:Qf){
    Z[, (i+1)] <- func.Legendre(X, i)
  }
  # calc w* and hence predict
  I <- diag(NCOL(Z))
  Hat_mat <- Z %*% solve(crossprod(Z) + lambda*I) %*% t(Z)
  Beta_Hat <- solve(crossprod(Z) + lambda*I) %*% t(Z) %*% Y
  Y_hat <- Z %*% Beta_Hat
  
  return(list(Hat_mat = Hat_mat, Beta_Hat = Beta_Hat, Y_hat = Y_hat))
}

test0 <- func.fit_model(x_dat, y_dat, Qf, lambda = 0)
test5 <- func.fit_model(x_dat, y_dat, Qf, lambda = 5)

func.predict <- function(x_seq, betas, Qf)
{
  Z <- matrix(0, nrow = NROW(x_seq), ncol = (Qf+1))
  for(i in 0:Qf){
    Z[, (i+1)] <- func.Legendre(x_seq, i)
  }
  pred <- Z %*% betas
  return(pred)
}

pred0 <- func.predict(x_seq, test0$Beta_Hat, Qf)
pred10 <- func.predict(x_seq, test5$Beta_Hat, Qf)

setwd(dir_figs)
cairo_pdf("ML_Ass2_fig_P2_modelfits.pdf", height = 5, width = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(x_seq, model, type = 'l', lwd = 2, xlab = "x", ylab = "f(x)",
     ylim = c(min(pred0, pred10), max(pred0, pred10)))
points(data)
lines(x_seq, pred0, col = colour_vec[1], lwd = 1.5)
lines(x_seq, pred10, col = colour_vec[2], lwd = 1.5)
legend("topright", title = expression(paste(bold("Model"))), 
       inset = c(-0.35, 0), lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       legend = c(expression(y), expression(lambda==0), expression(lambda==5)), 
       col = c('black', colour_vec[1:2]), y.intersp = 1.5, x.intersp = 0.4, 
       seg.len = 0.8)
dev.off()

## Cross validation - K-fold
func.cv <- function(lambda, data, k.folds)
{
  ## is data a subset of the fold number?
  stopifnot(NROW(data) %% k.folds == 0)
  ## data shuffle all the rows 
  data <- as_tibble(data)
  data_shuffled <- sample_frac(data, 1L)
  MSE <- numeric()
  ## Split data into k folds
  folds <- rep(1:k.folds, nrow(data_shuffled)/k.folds)
  data_in_folds <- split(data_shuffled, folds)

  for(fold in 1:k.folds)
  {
    ## Use fold as validation, all else as testing
    dat_train <- bind_rows(data_in_folds[-fold])
    dat_validate <- bind_rows(data_in_folds[fold])
    # fit model with specified lambda
    beta_hat <- func.fit_model(dat_train$x_dat, dat_train$y_dat, 10, lambda)$Beta_Hat
    yhat <- func.predict(dat_validate$x_dat, beta_hat, 10)
    MSE[fold] <- mean((yhat - dat_validate$y_dat)^2)
  }
  # MSE for the 10 folds and specified lambda value
  return(MSE)
}

# For different values of lambda
lambda_vec <- seq(0.1, 10, length.out = 500)
testcv <- mclapply(lambda_vec, func.cv, data = data, k.folds = 10,
                   mc.cores = detectCores())

cairo_pdf("ML_Ass2_fig_P2_CVerrors.pdf", height = 5, width = 10)
par(mfrow = c(1, 2))
lapply(testcv, mean) %>% # mean over folds...
  unlist %>%
  smooth.spline(x = lambda_vec) %>%
  plot(type = 'l', xlab = expression(lambda), 
       ylab = expression(E[val]))
lapply(testcv, median) %>% # median over folds...
  unlist %>%
  smooth.spline(x = lambda_vec) %>%
  plot(type = 'l', xlab = expression(lambda), 
       ylab = expression(E[val]))
dev.off()

# approximate lambda based on plots 
lambdaOpt <- 1.70
testOpt <- func.fit_model(x_dat, y_dat, Qf = 10, lambda = lambdaOpt)
predOpt <- func.predict(x_seq, testOpt$Beta_Hat, Qf)

setwd(dir_figs)
cairo_pdf("ML_Ass2_fig_P2_opt_fit.pdf", height = 5, width = 5)
par(mar = par()$mar + c(0,0,0,5), pty = 's') # larger margins so legend on side
plot(x_seq, model, xlab = bquote(lambda), ylab = bquote(E[val]), type = 'l',
     ylim = c(min(predOpt), max(predOpt)))
points(data)
lines(x_seq, predOpt, col = colour_vec[5], lwd = 1.5)
legend("topright", title = expression(paste(bold("Model"))), 
       inset = c(-0.35, 0), lwd = 3, cex = 1, xpd = TRUE, bty = "n",
       legend = c(expression(y), bquote(lambda%~~%.(lambdaOpt))), 
       col = c('black', colour_vec[5]), y.intersp = 1.5, x.intersp = 0.4, 
       seg.len = 0.8)
dev.off()

# Problem 3 ----

## Initial pars, 400 92x112 images
N <- 400; Ht <- 112; Wdth <- 92
## Read in images
images <- mclapply(1:N, function(i){
  read.pnm(paste(dir_data, "/", dat_faces_filen[i], sep = ""), cellres = 1)},
  mc.cores = (detectCores() - 1))

# image(images[[1]]@grey, col = grey(seq(0, 1, length = 256)), axes = FALSE)

## Rotate images to correct orientation
func.rotate <- function(x) t(apply(x, 2, rev))
image_oriented <- lapply(images, function(image){ func.rotate(image@grey) })
## Oriented Images need to be in vector format
image_oriented_vec <- lapply(image_oriented, as.vector) %>% do.call("rbind", .)
## Rows are image no. indices... column mean/sd gives pixel mean across all images
image_mean <- colMeans(image_oriented_vec) %>% matrix(., nrow = Wdth, ncol = Ht)
image_sd <- apply(image_oriented_vec, 2, sd) %>% matrix(., nrow = Wdth, ncol = Ht)

## Plot mean and standard deviation image
setwd(dir_figs)
cairo_pdf("ML_Ass2_fig_P3_mean_image.pdf", height = 5, width = 5)
image(image_mean, col = grey(seq(0, 1, length = 256)), axes = FALSE)
dev.off()

cairo_pdf("ML_Ass2_fig_P3_sd_image.pdf", height = 5, width = 5)
image(image_sd, col = grey(seq(0, 1, length = 256)), axes = FALSE)
dev.off()

## Scale images
image_scaled <- lapply(image_oriented, 
                       function(image){ (image - image_mean) / image_sd })

## Image 168... plot original (oriented) and scaled
n <- 168

cairo_pdf("ML_Ass2_fig_P3_orient_image168.pdf", height = 5, width = 5)
image(image_oriented[[n]], col = grey(seq(0, 1, length = 256)), axes = FALSE)
dev.off()

cairo_pdf("ML_Ass2_fig_P3_scaled_image168.pdf", height = 5, width = 5)
image(image_scaled[[n]], col = grey(seq(0, 1, length = 256)), axes = FALSE)
dev.off()

## Want each row to be a wdth*ht vector
images_scaled_vec <- lapply(image_scaled, 
                            function(image){ as.vector(t(image)) }) %>%
  do.call("rbind", .) 

## Get Eigenvalues/Vectors using prcomp >> unit eigenvectors
### N << p (much smaller) do eigen decomp on smaller matix
### rather use XXt
## new eigen vector is Xt psi
A <- 1/N * images_scaled_vec %*% t(images_scaled_vec) 
PCA <- prcomp(A)
image_eigen_vectors <- t(images_scaled_vec) %*% PCA$rotation
image_eigen_vectors_norm <- apply(image_eigen_vectors, 2, function(x){ x/sqrt(sum(x^2)) })

check_scree <- eigen(A)
check_eigs <- t(images_scaled_vec) %*% check_scree$vectors
setwd(dir_figs)
cairo_pdf("ML_Ass2_fig_P3_eigenvalues.pdf", height = 5, width = 5)
plot(cumsum(check_scree$values)/sum(check_scree$values), type = 'l',
xlab = "N", ylab = "Variance Explained")
abline(h = c(0.5, 0.8, 0.95), lty = 'dashed', col = colour_vec[2])
abline(v = c(5, 50, 200), lty = 'dashed', col = colour_vec[1])
dev.off()

## Plot first ten eigenfaces

cairo_pdf("ML_Ass2_fig_P3_eigenfaces.pdf", height = 7.5, width = 10)
par(mfrow = c(2, 5))
par(mar = c(0.2, 0.2, 0.2, 0.2))
for (i in 1:10){
  eigen_vec_mat <- matrix(image_eigen_vectors_norm[, i], nrow = 92, byrow = TRUE)
  image(eigen_vec_mat, col = grey(seq(0, 1, length = 256)), axes = FALSE)
}
dev.off()

## reconstruct an image based on eigenfaces used
func.reconstruct <- function(images_scaled_vec, image_eigen_vectors_norm)
{
  ## Project onto the Eigen Space
  project.eig <- t(data.matrix(images_scaled_vec)) %*% image_eigen_vectors_norm
  ## Project back
  reconstruct.img <- project.eig %*% t(image_eigen_vectors_norm)
  ## Plot the image... Vector needs to be matrix
  image(matrix(reconstruct.img, nrow = 92, byrow = TRUE), 
        col = grey(seq(0, 1, length = 256)), axes = FALSE)
}

## Image 115, c(x, y) corresponds to number of eigenfaces used
n = 115
cairo_pdf("ML_Ass2_fig_P3_recon5_image115.pdf", height = 5, width = 5)
func.reconstruct(images_scaled_vec[n, ], image_eigen_vectors_norm[, c(1:5)])
dev.off()

cairo_pdf("ML_Ass2_fig_P3_recon50_image115.pdf", height = 5, width = 5)
func.reconstruct(images_scaled_vec[n, ], image_eigen_vectors_norm[, c(1:50)])
dev.off()

cairo_pdf("ML_Ass2_fig_P3_recon200_image115.pdf", height = 5, width = 5)
func.reconstruct(images_scaled_vec[n, ], image_eigen_vectors_norm[, c(1:200)])
dev.off()

cairo_pdf("ML_Ass2_fig_P3_scaled_image115.pdf", height = 5, width = 5)
image(image_scaled[[n]], col = grey(seq(0, 1, length = 256)), axes = FALSE)
dev.off()
