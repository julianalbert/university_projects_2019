
# UCT ASSIGNMENT
# Author: Julian Albert
# Date:   25/05/2019

# Clean Environment
rm(list = ls()); dev.off()
## 0. Load all the packages for the assignment question
if (!require("pacman")) install.packages("pacman")
p_load(R2OpenBUGS)

# 1. Data ----

X_dat <- c(0.32, 0.63, 0.73, 0.38, 0.54, 0.95, 0.60, 1.03, 0.66, 0.41,
           0.48, 0.27, 0.60, 0.21, 0.39, 0.28, 1.03, 0.68, 0.10, 0.69)

N <- length(X_dat)

data <- list('N', 'X_dat')

model3 <- function(){
  
  # Hyper-prior
  v ~ dpar(2, 1)
  a <- log(2)/(pow(1.5, v))
  b <- log(2)/(pow(0.2, v))
  lambda ~ dunif(a, b)

  for(i in 1:N){
    # Data distribution
    X_dat[i] ~ dweib(v, lambda)
  }
  
  # Mean
  logmu <- loggam(1 + 1/v) - (1/v)*log(lambda)
  mu <- exp(logmu)
  x ~ dweib(v, lambda)
  pmorethan <- 1 - step(1.2 - x)
  pmorethanplot <- exp(-lambda * pow(1.2, v))
  
}

write.model(model3, "model3.txt")
model.file3 = paste(getwd(), "model3.txt", sep="/")
file.show("model3.txt")

sim3 <- bugs(data, inits = NULL, model.file = model.file3,
             parameters = c("x", 'mu', 'pmorethan', 'pmorethanplot', 
                            "v", "a", "b", "lambda", 'logmu'),
             n.chains = 3, n.iter = 5000, debug = TRUE)

sim3$summary

# plots
colnames(sim3$sims.matrix)

# mean distribution
plot_mu <- sim3$sims.matrix[, 2]
plot_mu_CI <- c(sim3$summary[2, 3], 
                sim3$summary[2, 7])

pdf("q3mu_posterior.pdf")
hist(plot_mu, probability = TRUE, 
     xaxs = 'i', yaxs = 'i', 
     ylim = c(0, 7), breaks = 50,
     main = '',
     #bquote("Distribution of"~mu~"including 95% Confidence Intervals"),
     xlab = expression(mu))
lines(density(plot_mu), lwd = 2)
abline(v = plot_mu_CI[1], lwd = 2, lty = 2)
abline(v = plot_mu_CI[2], lwd = 2, lty = 2)

# p>1.2 distribution
plot_pmorethan <- sim3$sims.matrix[, 4]
plot_pmorethan_CI <- c(sim3$summary[4, 3], 
                       sim3$summary[4, 7])

pdf("q3pmorethan_posterior.pdf")
hist(plot_pmorethan, probability = TRUE, 
     xaxs = 'i', yaxs = 'i',
     ylim = c(0, 30), breaks = 50,
     main = '',
     #bquote("Distribution of Pr(X > 1.2) including 95% Confidence Intervals"),
     xlab = "Pr(X > 1.2)")
lines(density(plot_pmorethan), lwd = 2)
abline(v = plot_pmorethan_CI[1], lwd = 2, lty = 2)
abline(v = plot_pmorethan_CI[2], lwd = 2, lty = 2)
