
# UCT ASSIGNMENT
# Author: Julian Albert
# Date:   25/05/2019

# Clean Environment
rm(list = ls()); dev.off()
# Assignment prelim script, useful packages and custom ggplot theme
source("assignment_prelim_script.R")
## 0. Load all the packages for the assignment question
if (!require("pacman")) install.packages("pacman")
p_load(R2OpenBUGS)

# 1. Data ----

X_dat <- c(2, 5, 5, 8, 10)
n_dat <- c(20, 16, 15, 25, 22)
N <- length(X_dat)

data <- list('N', 'X_dat', "n_dat")

# 2. OpenBUGS Model ----

model <- function(){
  
  # Priors
  for(i in 1:N){
    X_dat[i] ~ dbin(theta[i], n_dat[i])
    theta[i] ~ dbeta(prior_alpha, prior_beta)
  }
  
  prior_alpha ~ dunif(0, 10)
  prior_beta ~ dunif(0, 10)

}

write.model(model, "model.txt")
model.file1 = paste(getwd(), "model.txt", sep="/")
file.show("model.txt")

inits <- function()
{
  list(theta = rnorm(N, 0, 100), 
       prior_alpha = runif(1, 0, 10), 
       prior_beta = runif(1, 0, 10))
}

sim <- bugs(data, inits, model.file = model.file1,
            parameters = c("theta", "prior_alpha", "prior_beta"),
            n.chains = 3, n.iter = 1000, OpenBUGS.pgm = OpenBUGS.pgm, 
            WINE = WINE, WINEPATH = WINEPATH, useWINE = TRUE)

