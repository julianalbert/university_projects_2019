
# UCT Assignment
# Author: Julian Albert
# Date:   26/03/2019

rm(list = ls()); dev.off()
# Assignment prelim script
source("../../../../PrelimScript/assignment_prelim_script.R")
## 0. Load required packages
p_load(ElemStatLearn, corrplot, MASS)

# Question 1 ----

## 1.1 Read in the Data and OLS ----
set.seed(123)
dat_prostate <- get('prostate', asNamespace('ElemStatLearn'))
dat_train <- dat_prostate %>% dplyr::filter(train == TRUE) %>% 
  dplyr::select(train)
dat_prostate_wo_train <- dat_prostate %>% dplyr::filter(train == TRUE) %>% 
   dplyr::select(-c(train, svi, gleason))
dat_std_prostate <- apply(dat_prostate_wo_train, 2, 
                          function(x) (x - mean(x))/(sd(x))) %>%
  cbind(., dat_train)

## 1.1.1 Standardise the Data then split
train_dat <- dat_std_prostate # training predictors

X <- train_dat %>%  dplyr::select(-c(lpsa, train)) %>% as.matrix()
Y <- train_dat %>%  dplyr::select(lpsa) %>% as.matrix()
n <- NROW(X) ; p <- NCOL(X)

### some quick EDA
dat_corr <- cor(train_dat %>%  dplyr::select(-train))
round(dat_corr*lower.tri(dat_corr), 3)

## 1.1.2 OLS
ols.beta_hat <- solve(crossprod(X)) %*% t(X) %*% Y
ols.fhat <- X %*% ols.beta_hat %>% as.numeric()

## 1.2 Ridge Regresion ----

func.cv_general <- function(data, lambda = NULL, method = 'Ridge', 
                            lasso.obj = NULL, lasso.pars = NULL)
{
  
  # Define Parameters and Matrices/Vectors
  data_shuffled <- sample_frac(data, 1L) %>%
     dplyr::select(-train)
  n <- NROW(data_shuffled); p <- NCOL(data_shuffled) - 1
  I_mat <- diag(p); fhat <- numeric(n)
  
  ### PCR Stuff
  if(method != 'PCR') cv.error <- numeric(n) else{
    cv.error <-  matrix(0, n, p) # X.svd <- svd(tmp.X_train)
    U <- X.svd$u
    D <- diag(X.svd$d) # D_{i} >= D_{i+1} forall i
    V <- X.svd$v # principal components of X
  }
  
  # Cross-Validation
  for(i in 1:n){
    idx <- i
    ### Training Data
    tmp.X_train <- data_shuffled[-idx, ] %>% 
       dplyr::select(-lpsa) %>% as.matrix()
    tmp.Y_train <- data_shuffled[-idx, ] %>% 
       dplyr::select(lpsa) %>% as.matrix()
    ### Testing Data
    tmp.X_test <- data_shuffled[idx, ] %>% 
       dplyr::select(-lpsa) %>% as.matrix()
    tmp.Y_test <- data_shuffled[idx, ] %>% 
       dplyr::select(lpsa) %>% as.matrix()
    
    if(method == 'Ridge'){
      ### Get Beta Coefficients 
      beta_hat <- solve(crossprod(tmp.X_train) + lambda*I_mat) %*% t(tmp.X_train) %*% tmp.Y_train
      ### Predictions
      fhat[i] <- tmp.X_test %*% beta_hat
      cv.error[i] <- (fhat[i] - tmp.Y_test)^2
    }else if(method == 'Lasso'){
      lasso.obj_func <- match.fun(lasso.obj)
      init_par <- lasso.pars
      ### Get Beta Coefficients 
      beta_hat <- optim(init_par, lasso.obj_func, lambda = lambda)$par
      ### Predictions
      fhat[i] <- tmp.X_test %*% beta_hat
      cv.error[i] <- (fhat[i] - tmp.Y_test)^2
    }else if(method == 'PCR'){
      for(j in 1:p){
        # wikipedia
        tmp.V <- V[, 1:j] %>% as.matrix()
        Z <- apply(tmp.V, 2, function(x) tmp.X_train %*% x)
        pcr.gamma <- solve(crossprod(Z)) %*% t(Z) %*% tmp.Y_train # ?
        beta_hat <- tmp.V %*% pcr.gamma 
        fhat[j] <- tmp.X_test %*% beta_hat %>% as.numeric()
        cv.error[i, j] <- (fhat[j] - tmp.Y_test)^2
      }
    }
    
  }
  
  CV <- cv.error %>% as.matrix()
  return(colMeans(CV))
}

### Range of Lambda Values, calculate CVs, Plot
lambda_tries <- seq(0, 6, length.out = 12) %>% as.matrix()
ridge_cvs <- apply(lambda_tries, 1, function(x) 
  func.cv_general(train_dat, x, 'Ridge'))
df_ridge <- tibble(lambda = lambda_tries, CV = ridge_cvs)

### Optimal Values
ridge.optimal_pair <- df_ridge %>% dplyr::filter(CV == min(CV))
ridge.optimal_lambda <- ridge.optimal_pair %>%  dplyr::select(lambda) %>% as.numeric()
ridge.optimal_CV <- ridge.optimal_pair %>%  dplyr::select(CV) %>% as.numeric()

setwd('../Figs')
pdf('CV_Ridge.pdf')
ggplot(df_ridge, aes(x = lambda, y = CV)) +
  geom_line(col = 'dodgerblue3') + 
  geom_point(col = 'dodgerblue3') +
  geom_point(aes(x = ridge.optimal_lambda, y = ridge.optimal_CV), 
             col = 'darkorange2', shape = 13, size = 4) +
  labs(title = bquote('CV Error for Different' ~ lambda ~ 'Values'),
       x = bquote(lambda)) +
  theme_university()
dev.off()

### Get Predictions for Ridge
I_mat <- diag(p)
ridge.beta_hat <- solve(crossprod(X) + ridge.optimal_lambda*I_mat) %*% t(X) %*% Y
ridge.fhat <- X %*% ridge.beta_hat %>% as.numeric()

## 1.3 Principal Component Regression ----

X.svd <- svd(X)
U <- X.svd$u
D <- diag(X.svd$d) # D_{i} >= D_{i+1} forall i
V <- X.svd$v # principal components of X

# plot(X.svd$d)
# cumsum((((X.svd$d)^2)/n / sum(((X.svd$d)^2)/n))*100)
# plot(cumsum((((X.svd$d)^2)/n / sum(((X.svd$d)^2)/n))*100))

# Var. Explained
cumsum((((X.svd$d)^2)/n / sum(((X.svd$d)^2)/n))*100) # 5

df_plot_pc <- data.frame(x = 1:6, y = X.svd$d)

setwd('../Figs')
pdf('PCR_components.pdf')
ggplot(df_plot_pc, aes(x = x, y = y)) +
  geom_line(col = 'dodgerblue3') +
  geom_point(col = 'dodgerblue3') +
  geom_hline(yintercept = 5.476453, col = 'darkorange2', lty = 'dashed') +
  geom_vline(xintercept = 5, col = 'darkorange2', lty = 'dashed') +
  labs(title = 'Singular Values for PCR',
       x = 'Component', y = 'Singular Value') +
  theme_university()
dev.off()

### var explained
PCR_cvs <- func.cv_general(train_dat, method = 'PCR') # 7
df_plot_pc_cv <- data.frame(x = 1:6, y = PCR_cvs)

setwd('../Figs')
pdf('PCR_components_CV.pdf')
ggplot(df_plot_pc_cv, aes(x = x, y = y)) +
  geom_line(col = 'dodgerblue3') +
  geom_point(col = 'dodgerblue3') +
  labs(title = 'Cross-Validation for PCR',
       x = 'Component', y = 'CV Error') +
  theme_university()
dev.off()

# cross-validation
Z <- apply(V[, 1:6], 2, function(x) X %*% x)
pcr.gamma <- solve(crossprod(Z)) %*% t(Z) %*% Y # ?
pcr.beta_hat <- V[, 1:6] %*% pcr.gamma 
pcr7.fhat <- X %*% pcr.beta_hat %>% as.numeric()

## 1.6 Lasso ----
lasso.obj_func <- function(pars, lambda)
{
  lambda <- lambda
  beta <- pars
  q <- 1
  
  tmp <- sum((Y - X %*% beta)^2)
  penalty <- lambda * sum(abs(beta)^q)
  lasso.obj <- tmp + penalty
  return(lasso.obj)
}

set.seed(123)
init_par <- runif(p, -5, 5)
lasso_cvs <- apply(lambda_tries, 1, function(x) 
  func.cv_general(train_dat, x, 'Lasso', lasso.obj_func, init_par))
df_lasso <- tibble(lambda = lambda_tries, CV = lasso_cvs)

### Optimal Values
lasso.optimal_pair <- df_lasso %>% dplyr::filter(CV == min(CV))
lasso.optimal_lambda <- lasso.optimal_pair %>%  dplyr::select(lambda) %>% as.numeric()
lasso.optimal_CV <- lasso.optimal_pair %>%  dplyr::select(CV) %>% as.numeric()

lasso.beta_hat <- optim(init_par, lasso.obj_func, lambda = lasso.optimal_lambda)$par
lasso.fhat <- X %*% lasso.beta_hat %>% as.numeric()

## 1.7 Compare Results of OLS, Ridge, Lasso and PCR ----


# # bayes.optimal_lambda <- mean(rr2$sigma2/rr2$tau2)
# ### Get Predictions for Ridge
# I_mat <- diag(p)
# bayes.beta_hat <- solve(crossprod(X) + bayes.optimal_lambda*I_mat) %*% t(X) %*% Y
# bayes.fhat <- X %*% bayes.beta_hat %>% as.numeric()


df_calc_MSE <- tibble(OLS = ols.fhat,
                      Ridge = ridge.fhat,
                      PCR = pcr7.fhat,
                      Lasso = lasso.fhat,
                      Bayes = bayes.fhat) 

df_plot_pred <- df_calc_MSE %>%
  gather(Method, value) %>% 
  mutate(x = rep(1:67, 5),
         y = rep(Y, 5), 
         Method = factor(Method))

setwd('../Figs')
pdf('Regression_fits.pdf')
ggplot(df_plot_pred, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = value, col = Method)) +
  labs(title = 'Regression Fits', x = '', y ='') +
  theme_university() +
  theme(legend.direction = 'vertical',
        legend.position = 'right', 
        legend.key.width = unit(1.5, 'line'))
dev.off()

MSEs <- lapply(df_calc_MSE, function(x) mean((x-Y)^2)) %>%
  bind_cols()

## 1.7 Shrinkage ----

tmp.djs <- X.svd$d
tmp.djs_plot <- seq(0, max(tmp.djs), length.out = 1000)
lambda_tries_plot <- seq(0, 10, length.out = 50) %>% as.matrix()
tmp.shrunk <- apply(lambda_tries_plot, 1, 
                    function(x) tmp.djs_plot/((tmp.djs_plot)^2 + x))

df_plot_shrink <- data.frame(lambda0 = tmp.shrunk[, 1],
                             lambda2.86 = tmp.shrunk[, 15],
                             lambda5.92 = tmp.shrunk[, 30],
                             lambda10 = tmp.shrunk[, 50])

df_plot_shrink <- df_plot_shrink %>% gather(Value, Coeff.) %>%
  mutate(x = rep(tmp.djs_plot, 4),
         Value = factor(Value))

setwd('../Figs')
pdf('shrinkage.pdf')
ggplot(df_plot_shrink, aes(x = x)) +
  geom_line(aes(y = Coeff., 
                col = Value)) +
  ylim(c(0, 0.35)) +
  scale_colour_discrete(name = "Penalty", 
                        breaks = c("lambda0", "lambda2.86", 
                                 "lambda5.92", "lambda10"),
                        labels = c(bquote(lambda ~ "= 0.00"), 
                                   bquote(lambda ~ "= 2.86"), 
                                   bquote(lambda ~ "= 5.92"),
                                   bquote(lambda ~ "= 10.0"))) +
  theme_university() +
  theme(legend.direction = 'vertical',
        legend.position = 'right',
        legend.key.width = unit(1, 'line')) +
  labs(title = 'Ridge Regression Shrinkage',
       x = bquote('Singular Value'~d[j]),
       y = bquote(d[j]/(d[j]^2 + lambda)))
dev.off()

## 1.8 Bayesian Ridge Regression ----

ridge.regression.bayes <-function(X, Y, D, s2, t2, 
                                  i1 = 0.001, i2 = 0.001, 
                                  i3 = 0.001, i4 = 0.001,
                                  ndraws = 15000){
  n <- length(Y) #the sample size
  d <- NCOL(X)
  
  betas <- matrix(NA, ncol = floor(ndraws/2), nrow = d)
  yhat <- matrix(NA, n, floor(ndraws/2))
  sigma2 <- NULL
  tau2 <- NULL
  icounter <- 0
  
  #do the sampling here and only retain the second half of the samples
  for (i in 1:ndraws){
    #sample beta
    lambda <- s2/t2
    C <- solve(crossprod(X)/s2 + D/t2 )
    mean_beta <- C %*% crossprod(X, Y)/s2
    b <- mvrnorm(n = 1, mu = mean_beta, Sigma = C)
    
    #sample s2
    shape_s2 <- i1 + n/2
    rate_s2 <- 0.5*crossprod( Y - X%*%b ) + i2
    s2 <- 1/rgamma(1, shape = shape_s2, rate = rate_s2)
    
    #sample tau2
    shape_tau2 <- i3 + d/2
    rate_tau2 <- 0.5*t(b) %*% D %*% b + i4
    t2 <- 1/rgamma(1, shape = shape_tau2, rate = rate_tau2)
    
    if (i > floor(ndraws/2)){
      icounter <- icounter + 1
      betas[,icounter] <- b
      sigma2[icounter] <- s2
      tau2[icounter] <- t2
    }
  } #end of simulations
  
  list(betas = betas, sigma2 = sigma2, 
       tau2 = tau2, yhat = yhat)
}

D <- diag(NCOL(X))

rr1 <- ridge.regression.bayes(X=X, Y= Y, D=D, s2=10, t2=100,
                              i1=0.0001, i2=0.0001, i3=0.0001, i4=0.0001)

rr2 <- ridge.regression.bayes(X=X, Y= Y, D=D, s2=10, t2=100,
                              i1=2, i2=11, i3=3.5, i4=2)

rr3 <- ridge.regression.bayes(X=X, Y= Y, D=D, s2=10, t2=100,
                              i1=2, i2=0.01, i3=6, i4=0.01)

rr4 <- ridge.regression.bayes(X=X, Y= Y, D=D, s2=10, t2=100,
                              i1=2, i2=0.01, i3=6, i4=0.01)

setwd('../Figs')
pdf('q1_prioronpost.pdf')
par(mfrow=c(3,2), family="serif", mai=c(0.4,0.4,0.4,0.4))
hist(rr1$sigma2/rr1$tau2, prob=TRUE, main="prior 1", xlab = expression(lambda), breaks=50)
plot(rr1$sigma2/rr1$tau2, type="l", ylab = expression(lambda), xlab="", main="prior 1")

hist(rr2$sigma2/rr2$tau2, prob=TRUE, main="prior 2", xlab = expression(lambda), breaks=50)
plot(rr2$sigma2/rr2$tau2, type="l", ylab = expression(lambda), xlab="", main="prior 1")
bayes.optimal_lambda <- mean(rr2$sigma2/rr2$tau2)


hist(rr3$sigma2/rr3$tau2, prob=TRUE, main="prior 3", xlab = expression(lambda), breaks=50)
plot(rr3$sigma2/rr3$tau2, type="l", ylab = expression(lambda), xlab="", main="prior 1")
dev.off()

df_lambda <- function(x){
  #the degrees of freedom for penalised spline regression model
  if(is.nan(x)==TRUE){
    NaN
  }else {
    if (x==0){
      ncol(X)
    }else{
      if (x>100000){
        0
      }else{
        sum(diag(X%*%solve(crossprod(X) + x*D, t(X))))    
      }
    }
  }
}

#sample from the prior distribution fo s2 and tau2
s2 <- 1/rgamma(10000, shape=0.1, rate=0.1)
tau2 <- 1/rgamma(10000, shape=0.1, rate=0.1)
lambda_prior <- s2/tau2

pdf('q1_dof_beforetune.pdf')
hist(na.omit(sapply(lambda_prior, FUN=df_lambda)), main="", 
     xlab=expression(H[lambda]), prob=TRUE, breaks = 20)

#sample from the prior distribution fo s2 and tau2
s2 <- 1/rgamma(10000, shape=2, rate=.01)
tau2 <- 1/rgamma(10000, shape=6, rate=.01)
lambda_prior <- s2/tau2

pdf('q1_dof_aftertune.pdf')
hist(na.omit(sapply(lambda_prior, FUN=df_lambda)), main="", 
     xlab=expression(H[lambda]), prob=TRUE, xlim=c(2,6), breaks = 50)

# --- END --- #

