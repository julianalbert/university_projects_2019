
# pars
n <- c(20, 16, 15, 25, 22)
x <- c(2, 5, 5, 8, 10)
alpha <- 10
beta <- 10

# function for specifying first term of posterior mean 
func.prior <- function(alpha, beta, ni)
{
  return (list("weight" = (alpha + beta)/(ni + alpha + beta),
               "mean"= (alpha)/(alpha + beta)))
}

# function for specifying second term of posterior mean 
func.data <- function(alpha, beta, ni, xi)
{
  return (list("weight"=(ni)/(ni + alpha + beta),
               "mean"= (xi)/(ni)))
}

prior_weights_means <- sapply(n, function(x) func.prior(alpha, beta, x))
data_weights_means <- sapply(1:5, function(i) func.data(alpha, beta, n[i], x[i]))

means <- seq(0.01, 50, length = 1000)

prior_list <- list()
for(i in 1:length(n))
{
  ni <- n[i]
  prior_list[[i]] <- sapply(means, 
                            function(x) func.prior(x, x, ni))
}

data_list <- list()
for(i in 1:length(n))
{
  data_list[[i]] <- sapply(means, 
                           function(tmp) func.data(tmp, tmp, n[i], x[i]))
}

post_means_list <- lapply(1:5, function(i)
  (as.vector(unlist(prior_list[[i]][1,]))
   * as.vector(unlist(prior_list[[i]][2,])))
  + (as.vector(unlist(data_list[[i]][1,]))
     *as.vector(unlist(data_list[[i]][2,])))
)

for(i in 1:length(n))
{
  test1 <- as.vector(unlist(data_list[[i]][1, ])) - 
    as.vector(unlist(prior_list[[i]][1, ]))
  testminval <- min(abs(test1))
  equal_line <- which(abs(test1) == testminval)
  
pdf(paste('plot_shrinkage', i, '.pdf' ,sep = ''), height = 3, width = 6)
par(mfrow = c(1, 2))
plot(means, prior_list[[i]][1, ],
     type = 'l', col = 'darkorange3', lwd = 2,
     ylab = bquote("Weight"), xlab = '')
lines(means, data_list[[i]][1, ], 
      col = 'dodgerblue3', lwd = 2)
abline(v = means[equal_line])
plot(means, post_means_list[[i]],
     ylab = bquote(theta[.(i)]), 
     xlab = '',
     type = "l", col="darkgreen", lwd = 2,
     ylim=c(min(unlist(data_list[[i]][2,])),
            max(unlist(prior_list[[i]][2,]))))
lines(means, prior_list[[i]][2,], lwd=2, col="darkorange3")
lines(means, data_list[[i]][2,], lwd=2, col="dodgerblue3")
dev.off()

}

