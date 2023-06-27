
# UCT ASSIGNMENT
# Author: Julian Albert
# Date:   25/05/2019

# Clean Environment
rm(list = ls()); dev.off()
## 0. Load all the packages for the assignment question
if (!require("pacman")) install.packages("pacman")
p_load(R2OpenBUGS, tidyverse, coda)

# 1. Data ----

X_dat <- c(2, 5, 5, 8, 10)
n_dat <- c(20, 16, 15, 25, 22)
N <- length(X_dat)

data <- list('N', 'X_dat', "n_dat")

# 2. OpenBUGS Model 1 ----

model1 <- function(){
  
  # HyperPriors
  prior_alpha ~ dunif(0, 10)
  prior_beta ~ dunif(0, 10)
  
  # Sampling Dist
  for(i in 1:N){
    theta[i] ~ dbeta(prior_alpha, prior_beta) # Prior
    X_dat[i] ~ dbin(theta[i], n_dat[i]) # Data Distribution
  }

}

write.model(model1, "model1.txt")
model.file1 = paste(getwd(), "model1.txt", sep="/")
file.show("model1.txt")

sim1 <- bugs(data, inits = NULL, model.file = model.file1,
            parameters = c("theta", "prior_alpha", "prior_beta"),
            n.chains = 3, n.iter = 5000, debug = TRUE, codaPkg = TRUE)

sim1$summary
sim1.coda <- read.bugs(sim1)

# 3. OpenBUGS Model 2 ----

n_new <- 18
data <- list('N', 'X_dat', "n_dat", "n_new")

model2 <- function(){
  
  # HyperPriors
  prior_alpha ~ dunif(0, 10)
  prior_beta ~ dunif(0, 10)
  
  # Sampling Dist
  for(i in 1:N){
    theta[i] ~ dbeta(prior_alpha, prior_beta) # Prior
    X_dat[i] ~ dbin(theta[i], n_dat[i]) # Data Distribution
  }
  
  # New dist
  theta_new ~ dbeta(prior_alpha, prior_beta)
  X_new ~ dbin(theta_new, n_new)
  
}

write.model(model2, "model2.txt")
model.file2 = paste(getwd(), "model2.txt", sep="/")
file.show("model2.txt")

sim2 <- bugs(data, inits = NULL, model.file = model.file2,
             parameters = c("theta", "prior_alpha", "prior_beta",
                            "theta_new", "X_new"),
             n.chains = 3, n.iter = 5000, debug = TRUE)

sim2$summary
colnames(sim2$sims.matrix)

# new theta
plot_thetanew <- sim2$sims.matrix[, 8]
plot_thetanew_CI <- c(sim2$summary[8, 3], 
                       sim2$summary[8, 7])

pdf("q2theta_new.pdf")
hist(plot_thetanew, probability = TRUE, 
     xaxs = 'i', yaxs = 'i', breaks = 50,
     main = '',
     #bquote("Distribution of Pr(X > 1.2) including 95% Confidence Intervals"),
     xlab = bquote(theta[new]))
lines(density(plot_thetanew), lwd = 2)
abline(v = plot_thetanew_CI[1], lwd = 2, lty = 2)
abline(v = plot_thetanew_CI[2], lwd = 2, lty = 2)

# new theta
plot_Xnew <- sim2$sims.matrix[, 9]
plot_Xnew_CI <- c(sim2$summary[9, 3], 
                  sim2$summary[9, 7])

pdf("q2X_new.pdf")
hist(plot_Xnew, probability = TRUE, 
     xaxs = 'i', yaxs = 'i', breaks = 20,
     main = '', ylim = c(0, 0.12),
     #bquote("Distribution of Pr(X > 1.2) including 95% Confidence Intervals"),
     xlab = bquote(X[new]))
lines(density(plot_Xnew), lwd = 2)
abline(v = plot_Xnew_CI[1], lwd = 2, lty = 2)
abline(v = plot_Xnew_CI[2], lwd = 2, lty = 2)
