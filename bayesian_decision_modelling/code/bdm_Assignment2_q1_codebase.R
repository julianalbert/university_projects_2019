
# UCT ASSIGNMENT
# Author: Julian Albert
# Date:   22/05/2019

# Clean Environment
rm(list = ls()); dev.off()
# Assignment prelim script, useful packages and custom ggplot theme
source("assignment_prelim_script.R")
## 0. Load all the packages for the assignment question
if (!require("pacman")) install.packages("pacman")
#p_load()

# 1. Define functions ----

## 1.1 Parameters
X <- c(0.98, 3.27, 6.04)
theta_init <- c(1, 1, 1)
npars <- length(theta_init)
nsamps <- 4000

## 1.2 Conditional theta_i from inverse probability transform
func.theta_i <- function(theta_j_vec)
{
  u <- runif(1, 0, 1) # random uniform
  theta_j <- sum(theta_j_vec) # j not i summed
  theta_i <- theta_j/((1 - u)^(1/5)) - theta_j # IPT
  
  return(theta_i)
}

func.accept_reject <- function(pars)
{
  tmp.theta <- pars[1]
  tmp.shape <- pars[2] # 3 in our case
  tmp.rate <- pars[3] # Xi in our case
  tmp.mode <- (tmp.shape - 1)/tmp.rate
  tmp.c <- dgamma(tmp.mode, tmp.shape, tmp.rate)
  tmp.u <- runif(1, 0, 1)
  
  tmp.target <- dgamma(tmp.theta, tmp.shape, tmp.rate) * tmp.c
  accepted_theta <- ifelse(tmp.target >= tmp.u, tmp.theta, NA)
  
  return(accepted_theta)
}

# 2. Run accept/reject to get priors for theta ----

### Initialise vectors
tmp.accepted_theta <- numeric()
accepted_theta <- matrix(0, nsamps, npars)

for(j in 1:nsamps)
{
  
  for(i in 1:npars){
    ### need to specify theta_i | theta_j
    theta_i <- theta_init[i]
    theta_j <- theta_init[-i]
    ### get theta_i | theta_j and run accept reject
    theta_i_conditional <- func.theta_i(theta_j)
    pars <- c(theta_i_conditional, 3, X[i])
    tmp.accepted_theta[i] <- func.accept_reject(pars)
    ### if not accepted, keep trying until accepted
    while( is.na(tmp.accepted_theta[i]) ){
      theta_i_conditional <- func.theta_i(theta_j)
      pars <- c(theta_i_conditional, 3, X[i])
      tmp.accepted_theta[i] <- func.accept_reject(pars)
    }
    theta_init[i] <- tmp.accepted_theta[i] # theta_i is the accepepted theta
  }
  ### store vector of theta's
  accepted_theta[j, ] <- theta_init
}

# 3. Plots ----

burnin_perc <- 0.5
full_samps_init <- 1
burnt_samps_init <- floor(nsamps*burnin_perc)
burin_samps <- burnt_samps_init:nsamps
plot_thetadf <- data.frame(theta1 = accepted_theta[burin_samps, 1],
                           theta2 = accepted_theta[burin_samps, 2],
                           theta3 = accepted_theta[burin_samps, 3])

setwd('../Figs')
pdf('Theta1_acf.pdf')
acf(plot_thetadf$theta1, main = '')
dev.off()

pdf('Theta2_acf.pdf')
acf(plot_thetadf$theta2, main = '')
dev.off()

pdf('Theta3_acf.pdf')
acf(plot_thetadf$theta3, main = '')
dev.off()

pdf('Theta1_pre_trace.pdf')
plot(plot_thetadf$theta1, 
     type = 'l', xlab = 'Sample', 
     ylab = bquote(theta[1]))
dev.off()
pdf('Theta2_pre_trace.pdf')
plot(plot_thetadf$theta2, type = 'l', xlab = 'Sample', 
     ylab = bquote(theta[2]))
dev.off()
pdf('Theta3_pre_trace.pdf')
plot(plot_thetadf$theta3, type = 'l', xlab = 'Sample', 
     ylab = bquote(theta[3]))
dev.off()

pdf('Theta1_post_trace.pdf')
plot(plot_thetadf$theta1, 
     type = 'l', xlab = 'Sample', 
     ylab = bquote(theta[1]))
dev.off()
pdf('Theta2_post_trace.pdf')
plot(plot_thetadf$theta2, type = 'l', xlab = 'Sample', 
     ylab = bquote(theta[2]))
dev.off()
pdf('Theta3_post_trace.pdf')
plot(plot_thetadf$theta3, type = 'l', xlab = 'Sample', 
     ylab = bquote(theta[3]))
dev.off()









max_thetas <- apply(plot_thetadf, 2, max) # save maxs for range

setwd('../Figs')
pdf('Theta1_posterior.pdf')
ggplot(plot_thetadf, aes(x = theta1)) +
  geom_histogram(aes(y = ..density..), col = 'black', fill = 'white') +
  geom_density(alpha = 0.1, fill = 'black') +
  xlim(-0.1, max_thetas[1]) +
  theme_university() + 
  labs(y = 'Density', x = '')
dev.off()

pdf('Theta2_posterior.pdf')
ggplot(plot_thetadf, aes(x = theta2)) +
  geom_histogram(aes(y = ..density..), col = 'black', fill = 'white') +
  geom_density(alpha = 0.1, fill = 'black') +
  xlim(-0.1, max_thetas[2]) +
  theme_university() + 
  labs(y = 'Density', x = '')
dev.off()

pdf('Theta3_posterior.pdf')
ggplot(plot_thetadf, aes(x = theta3)) +
  geom_histogram(aes(y = ..density..), col = 'black', fill = 'white') +
  geom_density(alpha = 0.1, fill = 'black') +
  xlim(-0.1, max_thetas[3]) +
  theme_university() + 
  labs(y = 'Density', x = '')
dev.off()

setwd('../Code')



