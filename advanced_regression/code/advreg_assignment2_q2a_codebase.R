
# UCT Assignment
# Author: Julian Albert
# Date:   04/04/2019

rm(list = ls()); dev.off()
# Assignment prelim script
source("../../../../PrelimScript/assignment_prelim_script.R")
## 0. Load required packages
p_load(mvtnorm, MASS, psych, parallel)

# Question 2 ----

## 2.1 Read in the Data ----
set.seed(123)

### All the predictor and response data
data <- read.csv('Gametocyte_Data.csv', 
                 header = T, sep =';') %>% as_tibble()
### time data
dat_times <- c(0, 3, 7, 14, 21, 28, 42)
### knot data
dat_knot1 <- c(0.5, 10, 25)

### seperate groups
Group1 <- data %>%
  dplyr::filter(grp == 1) %>%
  dplyr::select(-c(profile_num, grp))

plot(as.numeric(Group1[1, ])~dat_times, ylim = c(0, 12), col ='red',
     type = 'o', main = 'Group 1', ylab = 'Response', xlab = 'Time')
for(i in 1:dim(Group1)[1]) lines(as.numeric(Group1[i, ])~dat_times, 
                                 type = 'o', col = 'red')

Group2 <- data %>%
  dplyr::filter(grp != 1) %>%
  dplyr::select(-c(profile_num, grp))

plot(as.numeric(Group2[1, ])~dat_times, ylim = c(0, 12), col = 'blue',
     type = 'o', main = 'Group 2', ylab = 'Response', xlab = 'Time')
for(i in 1:dim(Group2)[1]) lines(as.numeric(Group2[i, ])~dat_times, 
                                 type = 'o', col = 'blue')

### both groups with Group variable 
dat_all <- cbind(bind_rows(Group1, Group2), factor(data$grp)) %>%
  as_tibble() %>% rename(Group = `factor(data$grp)`)

### PARAMETERS
N <- NROW(dat_all)
n1 <- NROW(Group1)
n2 <- NROW(Group2)

J <- length(dat_times)
Lstar <- length(dat_knot1)
L <- length(dat_knot1) + 2 # add two for betas
knot.function <- function(x) return(x*log(x))

I <- diag(Lstar)
D <- matrix(0, L, L)
D[(Lstar:L), (Lstar:L)] <- I

basis_matrix <- matrix(0, J, Lstar)
for(j in 1:J){
  basis_matrix[j, ] <- abs(dat_times[j] - dat_knot1) %>%
    knot.function()
}

Design_matrix <- cbind(1, dat_times, basis_matrix)

## 2.2 Gibbs sampler ----
### paramaters
i1 <- 0.001; i2 <- 0.001
i3 <- 0.001; i4 <- 0.001
nsamps <-  15000
### Inpute Data
predictors <- Design_matrix
response <- dat_all %>% dplyr::filter(Group == 1) %>%
  dplyr::select(-Group) %>% as.matrix()

func.gibbs <- function(predictors, # the predictor variables
           response, # the response variable
           D,
           nsamps = 15000, # the no. of iterations used in the Gibbs sampler
           i1 = 0.001, i2 = 0.001, # pars for prior for error variance
           i3 = 0.001, i4 = 0.001 # pars for prior for phi variance
           )
{
  ### Data Matrices
  X <- predictors
  XtX <- crossprod(X)
  Y <- response
  
  ### Initial Parameter Specifications
  var_e <- 1
  var_phi <- 1
  icounter <- 0
  Sigma_e <- numeric()
  Sigma_Phi <- numeric()
  Phi <- matrix(0, L, floor(nsamps/2))
  
  ### Posterior Sampling
  for (i in 1:nsamps){
  
    # Phi Given All Else
    sigma_0 <- solve(1/var_e * XtX + 1/var_phi * D)
    mu_0 <- 1/var_e * (sigma_0 %*% t(X) %*% Y)
    phi_posterior <- mvrnorm(1, mu_0, sigma_0)
    
    # Error Variance Given All Else
    a_e <- (i1 + 7/2)
    b_e <- (i2 + 1/2 * crossprod(Y - X %*% phi_posterior))
    var_e <- 1/rgamma(1, a_e, b_e)
    
    # Error Phi Given All Else
    a_phi <- (i3 + Lstar/2)
    b_phi <- (i4 + 1/2 *  t(phi_posterior) %*% D %*% phi_posterior)
    var_phi <- 1/rgamma(1, a_phi, b_phi)
    
    if (i > floor(nsamps/2)){
      icounter <- icounter + 1
      Sigma_e[icounter] <- var_e
      Sigma_Phi[icounter] <- var_phi
      Phi[, icounter] <- phi_posterior
    }
  } #end of simulations
  
  return(list(Phi = Phi,
              Sigma_Phi = Sigma_Phi,
              Sigma_e = Sigma_e))
}

func.gibbs_statcheck <- function(predictors, # the predictor variables
                       response, # the response variable
                       D,
                       nsamps = 15000, # the no. of iterations used in the Gibbs sampler
                       i1 = 0.9, i2 = 0.1, # pars for prior for error variance
                       i3 = 5, i4 = 0.03 # pars for prior for phi variance
                       )
{
  ### Data Matrices
  X <- predictors
  XtX <- crossprod(X)
  Y <- response
  
  ### Initial Parameter Specifications
  var_e <- 0.1
  var_phi <- 0.1
  icounter <- 0
  Sigma_e <- numeric()
  Sigma_Phi <- numeric()
  Phi <- matrix(0, L, floor(nsamps/2))
  
  ### Posterior Sampling
  for (i in 1:nsamps){
    
    # Phi Given All Else
    sigma_0 <- solve(1/var_e * XtX + 1/var_phi * D)
    mu_0 <- 1/var_e * (sigma_0 %*% t(X) %*% Y)
    phi_posterior <- mvrnorm(1, mu_0, sigma_0)
    
    # Error Variance Given All Else
    a_e <- (i1 + 7/2)
    b_e <- (i2 + 1/2 * crossprod(Y - X %*% phi_posterior))
    var_e <- 1/rgamma(1, a_e, b_e)
    
    # Error Phi Given All Else
    a_phi <- (i3 + Lstar/2)
    b_phi <- (i4 + 1/2 * t(phi_posterior) %*% D %*% phi_posterior)
    var_phi <- 1/rgamma(1, a_phi, b_phi)
    
    if (i > floor(nsamps/2)){
      icounter <- icounter + 1
      Sigma_e[icounter] <- var_e
      Sigma_Phi[icounter] <- var_phi
      Phi[, icounter] <- phi_posterior
    }
  } #end of simulations
  
  return(list(Phi = Phi,
              Sigma_Phi = Sigma_Phi,
              Sigma_e = Sigma_e))
}

test1 <- func.gibbs_statcheck(predictors, response[1,], D = D)
setwd('../Figs')
pdf('q2a_phi1.pdf')
plot(test1$Phi[1, ], type = 'l',
     xlab = 'Sample No.', ylab = bquote(tilde(phi[1])))
pdf('q2a_phi2.pdf')
plot(test1$Phi[2, ], type = 'l',
     xlab = 'Sample No.', ylab = bquote(tilde(phi[2])))
pdf('q2a_phi3.pdf')
plot(test1$Phi[3, ], type = 'l',
     xlab = 'Sample No.', ylab = bquote(tilde(phi[3])))

## 2.3 Let's GO! ----

### Run Gibbs in parallel
no.cores <- detectCores()
cl <- makeCluster(no.cores)
clusterExport(cl, c("L", 'J', 'Lstar'))
clusterEvalQ(cl, library(MASS))
q2a_gibbs.out <- parApply(cl = cl, X = response, MARGIN = 1, 
                          FUN =func.gibbs, predictors = predictors, 
                          D = D, nsamps = nsamps, 
                          i1 = i1, i2 = i2, i3 = i3, i4 = i4)
stopCluster(cl)

### should get a 25x7 prediction matrix, Credibility Intervals
names(q2a_gibbs.out) <- seq(1, 25) %>% as.character()
yhats <- lapply(q2a_gibbs.out, function(x) predictors %*% x$Phi)
y.lowers <- lapply(yhats, apply, 1, function(x) quantile(x, 0.025))
y.means <- lapply(yhats, apply, 1, function(x) mean(x))
y.uppers <- lapply(yhats, apply, 1, function(x) quantile(x, 0.975))
y.fitted <- lapply(q2a_gibbs.out, 
                    function(x) predictors %*% rowMeans(x$Phi))

### get the MSE
yhat_long <- lapply(q2a_gibbs.out, 
                    function(x) predictors %*% rowMeans(x$Phi)) %>%
  bind_rows() %>% as.matrix() %>% as.vector()

response_long <- as.vector(t(response)) # 175 x 1
q2a.MSE <- mean((response_long - yhat_long)^2)

set.seed(420)
tmp.sample <- c(1, sample(NROW(Group1), 3))

setwd('../Figs')
pdf('credibility_intervals.pdf')
par(mfrow=c(2,2), family="serif", mai=c(0.4,0.4,0.4,0.4))
for(i in tmp.sample){
  plot(as.numeric(Group1[i, ])~dat_times, ylim = c(0, 13), 
       col ='darkgreen', type = 'o', ylab = 'Response', xlab = 'Time',
       main = bquote('Profile' ~ .(i)))
  lines(y.lowers[[i]]~dat_times, type = 'l', col = 'red', lty = 2)
  lines(y.means[[i]]~dat_times, type = 'o', col = 'blue')
  lines(y.uppers[[i]]~dat_times, type = 'l', col = 'red', lty = 2)
  lines(y.fitted[[i]]~dat_times, type = 'l', col = 'blue')
  lines(as.numeric(Group1[i, ])~dat_times, 
        type = 'o', col = 'darkgreen')
  legend(x= 26, y=13, bty="n", 
         col=c('red', 'blue', 'darkgreen'), 
         cex=0.8, c("Interval" ,"Predicted", 'True'), 
         lty = c(2, 1, 1), y.intersp=1, lwd = c(1.5, 1.5, 1.5))
}
dev.off()

## 2.4 Tune priors ----

#sample from the prior distribution of var_e and var_phi
num_tuning_samps <- 10000
tmp.shapes_e <- round(seq(0.5, 1.5, length.out = 10), 2)
tmp.rates_e <- round(seq(0.05, 0.15, length.out = 10), 2)

tmp.shapes_phi <- round(seq(4.5, 5.5, length.out = 10), 2)
tmp.rates_phi <- round(seq(0.01, 0.05, length.out = 10), 2)

set.seed(123)
for(i in 1:length(tmp.shapes_e)){
### cycle through rates and shapes
tmp.var_e <- 1/rgamma(num_tuning_samps, tmp.shapes_e[i], tmp.rates_e[i])
tmp.var_phi <- 1/rgamma(num_tuning_samps, tmp.shapes_phi[i], tmp.rates_phi[i])
edf <- numeric(num_tuning_samps)
X <- predictors
XtX <- crossprod(X)
### get different edf values
  for(j in 1:num_tuning_samps){
    tmp.sigma_0 <- solve(1/tmp.var_e[j] * XtX + 1/tmp.var_phi[j] * D)
    H <- X %*% tmp.sigma_0 %*% t(X) * 1/tmp.var_e[j]
    edf[j] <- tr(H)
  }
### plot this bad boi
  if( i == 3){
  setwd('../Figs')
  pdf('q2a_tuning_is.pdf')
  hist(edf, breaks = 20,
       xlab = expression(tr(H)), prob=T,
       xlim = c(0, 5),
       main = bquote(atop(i[1] == .(tmp.shapes_e[i]), 
                          i[2] == .(tmp.rates_e[i])) ~ 
                       atop(i[3] == .(tmp.shapes_phi[i]),
                            i[4] == .(tmp.rates_phi[i])) ~~~~~ 
                       'Iteration' == .(i) ))
  dev.off()
  }

} # iteration 10

### Nice Left Skew... Not too skew
tuned_shape_e <- tmp.shapes_e[3]
tuned_rate_e <- tmp.rates_e[3]
tuned_shape_phi <- tmp.shapes_phi[3]
tuned_rate_phi <- tmp.rates_phi[3]

### rerun with sensical priors
no.cores <- detectCores()
cl <- makeCluster(no.cores)
clusterExport(cl, c("L", 'J', 'Lstar'))
clusterEvalQ(cl, library(MASS))
q2a_gibbs.out2 <- parApply(cl = cl, X = response, MARGIN = 1, 
                          FUN =func.gibbs, predictors = predictors, 
                          D = D, nsamps = nsamps, 
                          i1 = tuned_shape_e, i2 = tuned_rate_e, 
                          i3 = tuned_shape_phi, i4 = tuned_rate_phi)
stopCluster(cl)

### should get a 25x7 prediction matrix
names(q2a_gibbs.out2) <- seq(1, 25) %>% as.character()
### get the MSE
yhat2_long <- lapply(q2a_gibbs.out2, 
                    function(x) predictors %*% rowMeans(x$Phi)) %>%
  bind_rows() %>% as.matrix() %>% as.vector()

### get the MSE
q2a.MSE2 <- mean((response_long-yhat2_long)^2)


# --- END --- #
