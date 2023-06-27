
# UCT Assignment
# Author: Julian Albert
# Date:   12/04/2019

rm(list = ls()); dev.off()
# Assignment prelim script
source("../../../../PrelimScript/assignment_prelim_script.R")
## 0. Load required packages
p_load(mvtnorm, MASS, psych, Matrix)

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
dat_knot2 <- c(0.5, 5, 25)

### seperate groups
Group1 <- data %>%
  dplyr::filter(grp == 1) %>%
  dplyr::select(-c(profile_num, grp))

Group2 <- data %>%
  dplyr::filter(grp != 1) %>%
  dplyr::select(-c(profile_num, grp))

### both groups with Group variable 
dat_all <- cbind(bind_rows(Group1, Group2), factor(data$grp)) %>%
  as_tibble() %>% rename(Group = `factor(data$grp)`)

### PARAMETERS
N <- NROW(dat_all)
n1 <- NROW(Group1)
n2 <- NROW(Group2)
J <- length(dat_times)
Lstar <- length(dat_knot1)

no.rows <- N*J
no.cols <- 3 + Lstar*N

knot.function <- function(x) return(x*log(x))

I <- diag(Lstar*N)
D <- matrix(0, no.cols, no.cols)
D[((Lstar+1):no.cols), ((Lstar+1):no.cols)] <- I

basis_matrix <- matrix(0, no.rows, no.cols)
basis_matrix[, 1] <- 1
basis_matrix[, 2] <- dat_times
basis_matrix[1:no.rows/2, 3] <- 1

Z1 <- t(apply(as.matrix(dat_times), 1, 
        function(x) abs(x - dat_knot1) %>%
          knot.function()))

Z2 <- t(apply(as.matrix(dat_times), 1, 
              function(x) abs(x - dat_knot2) %>%
                knot.function()))

Z1_mat <- diag(25) %x% Z1
Z2_mat <- diag(25) %x% Z2

basis_matrix[1:dim(Z1_mat)[1], 
             4:(dim(Z1_mat)[2]+3)] <- Z1_mat

basis_matrix[(dim(Z1_mat)[1]+1):dim(basis_matrix)[1], 
             (dim(Z1_mat)[2]+4):dim(basis_matrix)[2]] <- Z2_mat

## 2.3 Gibbs sampler ----

func.gibbs <- function(predictors, # the predictor variables
                       response, # the response variable
                       D,
                       nsamps = 15000, # the no. of iterations used in the Gibbs sampler
                       i1 = 0.001, i2 = 0.001, # pars for prior for error variance
                       i3 = 0.001, i4 = 0.001 # pars for prior for phi variance
)
{
  ### Data Matrices
  X <- predictors # 350 153
  XtX <- crossprod(X) # 153 153
  Y <- response # 50 7
  Y_long <- as.vector(t(Y)) %>% as.matrix() # 350   1
  
  ### Initial Parameter Specifications
  var_e <- 1
  var_phi <- 1
  icounter <- 0
  Sigma_e <- numeric()
  Sigma_Phi <- numeric()
  Phi <- matrix(0, no.cols, floor(nsamps/2)) # 153 7500
  
  ### Posterior Sampling
  for (i in 1:15000){

    # Phi Given All Else
    sigma_0 <- solve(1/var_e * XtX + 1/var_phi * D)
    mu_0 <- 1/var_e * (sigma_0 %*% t(X) %*% Y_long)
    phi_posterior <- mvrnorm(1, mu_0, sigma_0)
    
    # Error Variance Given All Else
    a_e <- (i1 + no.rows/2)
    b_e <- (i2 + 1/2 * crossprod(Y_long - X %*% phi_posterior))
    var_e <- 1/rgamma(1, a_e, b_e)
    
    # Error Phi Given All Else
    a_phi <- (i3 + no.cols/2)
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

## 2.3 Let's GO!

### paramaters
i1 <- tuned_shape_e; i2 <- tuned_rate_e
i3 <- tuned_shape_phi; i4 <- tuned_rate_phi

predictors <- basis_matrix
response <- dat_all %>% dplyr::select(-Group) %>% as.matrix()

set.seed(123)
q2b_gibbs.out <- func.gibbs(predictors, response, D, 15000,
                            i1, i2, i3, i4)
q2b_params <- rowMeans(q2b_gibbs.out$Phi)
q2b_yhat <- predictors %*% q2b_params # mse = 0.1106431
q2b_mse <- mean((q2b_yhat-Y_long)^2)

q2b_y_plot <- matrix(Y_long, nrow = 50, byrow = TRUE)
q2b_yhat_plot <- matrix(q2b_yhat, nrow = 50, byrow = TRUE)

set.seed(123)
tmp.sample <- c(1, sample(NROW(Group1), 3))

setwd('../Figs')
pdf('q2b_fits.pdf')
par(mfrow=c(2,2), family="serif", mai=c(0.4,0.4,0.4,0.4))
for(i in tmp.sample){
  plot(as.numeric(Group1[i, ])~dat_times, ylim = c(0, 13), 
       col ='darkgreen', type = 'o', ylab = 'Response', xlab = 'Time',
       main = bquote('Profile' ~ .(i)))
  lines(q2b_yhat_plot[i, ]~dat_times, type = 'o', col = 'blue')
  lines(as.numeric(Group1[i, ])~dat_times, 
        type = 'o', col = 'darkgreen')
  legend(x= 26, y=13, bty="n", 
         col=c('blue', 'darkgreen'), 
         cex=0.8, c("Predicted", 'True'), 
         lty = c(1, 1), y.intersp=1, lwd = c(1.5, 1.5))
}
dev.off()



