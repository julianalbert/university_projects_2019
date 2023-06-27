##
#
# Author: Julian Albert
# Date: 22 September 2019
#
# Description:
# HFT Assignment 3 - Basically want to work with TAQ data and try simulate the
# order book activity using hawkes processes
#
#------------------------------------------------------------------------------#

# 0. Clean Workspace, Directory and Library ----

## Clean Workspace
rm(list=ls())
dev.off() # Close Plots
setwd("~") # Clear Path to User

## Locations
project_folder <- "/Documents/UCT/Coursework/HFT"
loc_script <- "/Assignment_3/UCT_Assignment/Code/HFT_Ass3"
loc_figs <- "/Assignment_3/UCT_Assignment/Figs"
loc_data <- "/Data"

## Directories
dir_script <- paste("~", project_folder, loc_script, sep = '')
dir_figs <- paste("~", project_folder, loc_figs, sep = '')
dir_data <- paste("~", project_folder, loc_data, sep = '')

## Filenames
filen_dat <- "/dat_TAQ.rds"

## Set Working Directory to Script Location
setwd(dir_script)

## Libraries - Lazyload
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, lubridate, zoo, data.table, Cairo, hawkes, truncdist)

## Import Clean Data
dat_TAQ <- readRDS(file = paste(dir_data, filen_dat, sep = ""))
#setwd(dir_data)
#load(file = "dat_SimHawkes.RData")

## Options
options(digits.secs = 3)

# 0.1 Simulate Accept/Reject for Inhomogenous Poisson Process ---- 

## Plot an Inhomogeneous Poisson process by thinning
set.seed(123)
func.lambda <- function(t){2 + sin(t)}

M = 4; Time = 4*pi
p = numeric(); py = numeric()
r = numeric(); ry = numeric()
t = 0

while(t < Time)
{
  t = t + rexp(1, M)
  if(t < Time){u = runif(1)}
  if(u <= func.lambda(t)/M){
    p = c(p, t)
    py = c(py, M*u)}
  else{
    r = c(r, t)
    ry = c(ry, M*u)}
}

t <- seq(0, Time, by = 0.01)
plot(c(0, Time), c(0, M), type = 'n', xlab = "Time", ylab = "", 
     xaxs = 'i', yaxs ='i')
lines(t, func.lambda(t), type = 'l')
lines(c(0, Time), c(M, M), lwd = 4)
points(p, py, pch = 16, col = 'darkgreen')
points(r, ry, pch = 3, col = 'red')
segments(p, rep(0, length(p)), p, py, lty = 'dashed', col = 'darkgreen')
dev.off()

# 1. Data Classification ---- 

## Let's work with Naspers 
dat_NPN <- dat_TAQ$NPN

# Select one day of trading
random_date <- sample(unique(as.Date(dat_NPN$DateTimeL)), 1)
dat_NPN_oneday <- dat_NPN %>%
  filter(as.Date(DateTimeL) == random_date)

# Classify Market Orders... >> There is a bug here, if sell follows buy?
dat_NPN_oneday_classified <- dat_NPN_oneday %>%
  mutate(Test.Sign = Trade.Sign, After_Bid = lead(L1.Bid), 
         Before_Bid = lag(L1.Bid), After_Ask = lead(L1.Ask),
         Before_Ask = lag(L1.Ask)) %>%
  mutate(MO_Class = 
           ifelse(Trade.Sign == -1 & After_Bid < Before_Bid, "Sell Moves Bid", 
            ifelse(Trade.Sign == -1, "Sell Doesn't Move Bid",
              ifelse(Trade.Sign == 1 & After_Ask > Before_Ask, "Buy Moves Ask",
                ifelse(Trade.Sign == 1, "Buy Doesn't Move Ask", NA)))))

# Classify Limit Orders... >> Bids and Asks need to be seperate

## Bid Classification
tmp_bids_class <- dat_NPN_oneday %>%
  select(DateTimeL, L1.Bid, L1.Ask) %>%
  mutate(Before_Bid = lag(L1.Bid),
         Before_Ask = lag(L1.Ask)) %>%
  mutate(LO_Bid_Class = 
           ifelse(L1.Bid > Before_Bid & L1.Bid < Before_Ask, "Bid Btwn Quotes",
            ifelse(L1.Bid <= Before_Bid, "Bid At/Below Best Bid", NA)))

## Ask Classification
tmp_asks_class <- dat_NPN_oneday %>%
  select(DateTimeL, L1.Bid, L1.Ask) %>%
  mutate(Before_Bid = lag(L1.Bid),
         Before_Ask = lag(L1.Ask)) %>%
  mutate(LO_Ask_Class = 
           ifelse(L1.Ask > Before_Bid & L1.Ask < Before_Ask, "Ask Btwn Quotes",
                  ifelse(L1.Ask >= Before_Ask, "Ask At/Above Best Ask", NA)))

## Final DataFrame with Classified Events 
dat_NPN_oneday_classified <- dat_NPN_oneday_classified %>%
  mutate(LO_Bid_Class = tmp_bids_class$LO_Bid_Class,
         LO_Ask_Class = tmp_asks_class$LO_Ask_Class)

## We need event arrival-times... subset data into events, take arrival times

## All of the events
Event_vector <- c("Sell Moves Bid", "Sell Doesn't Move Bid",
                  "Buy Moves Ask", "Buy Doesn't Move Ask",
                  "Bid Btwn Quotes", "Bid At/Below Best Bid",
                  "Ask Btwn Quotes", "Ask At/Above Best Ask")
## Column names of data
Column_name_vector <- c("MO_Class", "LO_Bid_Class", "LO_Ask_Class")

## function to subset data to get arrival times
func.get_events <- function(data, Event, Column_name, Type)
{
  ## Subset data into event
  dat_filtered <- data %>%
    filter(get(Column_name) == Event)
  
  if(Type == "Quote"){
    ## Get Quote relevant columns
    dat_filtered <- dat_filtered %>%
      select(DateTimeL, Type, L1.Ask, L1.Bid, MidPrice, Volume.Ask, Volume.Bid,
             MicroPrice)
  } else{
    ## Get Trade relevant columns
    dat_filtered <- dat_filtered %>%
      select(DateTimeL, Type, Price, Trade.Sign, Volume.Trade)
  }
  return(dat_filtered)
}

## Initialise lists
MO_Classes = LO_Bid_Classes = LO_Ask_Classes = list()

## Market Order Events are the first 4
for(i in 1:4){
  MO_Classes[[i]] <- func.get_events(dat_NPN_oneday_classified, 
                                     Event_vector[i], Column_name_vector[1],
                                     "Trade") 
}
names(MO_Classes) <- Event_vector[1:4]

## Bid Limit Order Events are 5th and 6th
for(i in 1:2){
  LO_Bid_Classes[[i]] <- func.get_events(dat_NPN_oneday_classified, 
                                         Event_vector[4+i], Column_name_vector[2],
                                         "Quote") 
}
names(LO_Bid_Classes) <- Event_vector[5:6]

## Ask Limit Order Events are 7th and 8th
for(i in 1:2){
  LO_Ask_Classes[[i]] <- func.get_events(dat_NPN_oneday_classified,
                                         Event_vector[6+i], Column_name_vector[3],
                                         "Quote") 
}
names(LO_Ask_Classes) <- Event_vector[7:8]

## Event Arrivals Relative to Day Start
DayStartTime <- as.POSIXct(paste(random_date, "09:10:00", sep = " "), 
                          timezone = "Africa/Johannesburg")
DayEndTime <- dat_NPN_oneday[NROW(dat_NPN_oneday), ]$DateTimeL

## Get Arrival times for each Event Type relative to day starting time
func.calc_arrival <- function(data)
{
  dat_new <- data %>%
    mutate(arrival_times = difftime(data$DateTimeL, DayStartTime, units = "secs"))
  
  return(dat_new)
}

MO_Classes <- lapply(MO_Classes, func.calc_arrival)
LO_Bid_Classes <- lapply(LO_Bid_Classes, func.calc_arrival)
LO_Ask_Classes <- lapply(LO_Ask_Classes, func.calc_arrival)

## Now We Want just arrival times
func.select_arrival <- function(data)
{
  dat_new <- data %>% select(arrival_times)
  return(dat_new)
}

MO_Arrivals <- lapply(MO_Classes, func.select_arrival)
LO_Bid_Classes <- lapply(LO_Bid_Classes, func.select_arrival)
LO_Ask_Classes <- lapply(LO_Ask_Classes, func.select_arrival)

# All arrival times in a single list
All_Arrivals <- list(MO_Arrivals[[1]], MO_Arrivals[[2]], 
                     MO_Arrivals[[3]], MO_Arrivals[[4]], 
                     LO_Bid_Classes[[1]], LO_Bid_Classes[[2]], 
                     LO_Ask_Classes[[1]], LO_Ask_Classes[[2]])

Event_No_Names_vec <- c()
for(i in 1:8) {Event_No_Names_vec[i] =(paste("Event_No", i, sep = ""))}
names(All_Arrivals) <- Event_No_Names_vec
# lapply(All_Arrivals, function(x) any(is.na(x))) # No NA values

## Pull removes tibble
All_Arrivals_list_of_vectors <- lapply(All_Arrivals, function(x) as.vector(pull(x)))

# 2. Estimate Hawkes Process ---- 

k = 2
## Likelihood function with constraints
func_likelihoodHawkes = function(params, k, columns)
{
  lambda = matrix(params[1:k], nrow = k)
  alpha = matrix(params[(k+1):(k^2 + k)], nrow = k, byrow = TRUE)
  beta = matrix(params[(k^2 + k + 1):(k^2 + 2*k)], nrow = k)
  eigenval = Re(eigen(diag(c(beta)) - alpha)$values)
  if(min(eigenval) <= 0 | min(alpha) < 0 | min(lambda) < 0){
    pen <- 1000000
    ll <- likelihoodHawkes(lambda, alpha, beta, 
                           All_Arrivals_list_of_vectors[columns])
    return(ll + pen)
   }else{
     pen <- 0
     ll <- likelihoodHawkes(lambda, alpha, beta, 
                            All_Arrivals_list_of_vectors[columns])
   return(ll + pen)
   }
}

## convert vector of pars into matrix
func.get_pars <- function(params, k)
{ 
  lambda0 = matrix(params[1:k], nrow = k)
  alpha = matrix(params[(k+1):(k^2 + k)], nrow = k, byrow = TRUE)
  beta = matrix(params[(k^2 + k + 1):(k^2 + 2*k)], nrow = k)
  return(list(lambda0, alpha, beta))
}

## Horizon and true number of events
secs_in_day <- as.numeric(difftime(DayEndTime, DayStartTime, units = "secs"))
n_true <- lapply(All_Arrivals, function(x) NROW(x)) %>% unlist()

# lambda0 will be 8x1 | alpha will be 8x8 | beta will be 8x1
## Market Sell and Buy (aggressive)
lambda_init <- lapply(All_Arrivals_list_of_vectors[c(1, 3)], 
                      function(x) NROW(x)/secs_in_day) %>% unlist()
lambda_init_3 <- lapply(All_Arrivals_list_of_vectors[c(5, 7)], 
                        function(x) NROW(x)/secs_in_day) %>% unlist()
lambda_init_4 <- lapply(All_Arrivals_list_of_vectors[c(1, 3, 5, 7)], 
                        function(x) NROW(x)/secs_in_day) %>% unlist()
## Random starting parameters >> satisfy constraints
set.seed(123)
alpha_init <- diag(0.01, k) %>% as.vector()
beta_init <- rep(0.05, k)
params_init <- as.numeric(c(lambda_init, alpha_init, beta_init))

alpha_init_3 <- diag(0.01, k) %>% as.vector()
beta_init_3 <- rep(0.05, k)
params_init_3 <- as.numeric(c(lambda_init_3, alpha_init_3, beta_init_3))

alpha_init_4 <- diag(c(0.01, 0.01, 0.01, 0.01), 4) %>% as.vector()
beta_init_4 <- c(0.05, 0.05, 0.05, 0.05)
params_init_4 <- as.numeric(c(lambda_init_4, alpha_init_4, beta_init_4))
# func_likelihoodHawkes(params_init, k = 2)
## Market Sell and Buy (aggressive)
params_hawkes_optim_NM <- optim(params_init, func_likelihoodHawkes, k = 2, 
                                columns = c(1, 3), method = "Nelder-Mead", 
                                control = list(maxit = 5000))
## Market Sell and Buy (passive)
params_hawkes_optim_NM_3 <- optim(params_init_3, func_likelihoodHawkes, k = 2,
                                  columns = c(5, 7), method = "Nelder-Mead",
                                  control = list(maxit = 5000))
## Market Sell and Buy (passive) and aggressive
params_hawkes_optim_NM_4 <- optim(params_init_4, func_likelihoodHawkes, k = 4,
                                  columns = c(1, 3, 5, 7), method = "Nelder-Mead",
                                  control = list(maxit = 10000))

## Converges
params_hawkes_optim_NM$convergence
params_hawkes_optim_NM_3$convergence
params_hawkes_optim_NM_4$convergence
## Optimal Pars
optim_pars <- params_hawkes_optim_NM$par
optim_pars_3 <- params_hawkes_optim_NM_3$par
optim_pars_4 <- params_hawkes_optim_NM_4$par
## Get parameter matrices
params_mat_hawkes <- func.get_pars(optim_pars, k)
params_mat_hawkes_3 <- func.get_pars(optim_pars_3, k)
params_mat_hawkes_4 <- func.get_pars(optim_pars_4, 4)
## Clean names
names(params_mat_hawkes) <- c("lambda0_opt", "alpha_opt", "beta_opt")
names(params_mat_hawkes_3) <- c("lambda0_opt", "alpha_opt", "beta_opt")
names(params_mat_hawkes_4) <- c("lambda0_opt", "alpha_opt", "beta_opt")

## Simulate(estimate) hawkes processes
res.sim_hawkes <- simulateHawkes(lambda0 = params_mat_hawkes$lambda0_opt, 
                                 alpha = params_mat_hawkes$alpha_opt, 
                                 beta = params_mat_hawkes$beta_opt,
                                 horizon = secs_in_day)

res.sim_hawkes_3 <- simulateHawkes(lambda0 = params_mat_hawkes_3$lambda0_opt, 
                                   alpha = params_mat_hawkes_3$alpha_opt, 
                                   beta = params_mat_hawkes_3$beta_opt,
                                   horizon = secs_in_day)

res.sim_hawkes_4 <- simulateHawkes(lambda0 = params_mat_hawkes_4$lambda0_opt, 
                                   alpha = params_mat_hawkes_4$alpha_opt, 
                                   beta = params_mat_hawkes_4$beta_opt,
                                   horizon = secs_in_day)
## Clean names
names(res.sim_hawkes) <- Event_No_Names_vec[c(1, 3)]
names(res.sim_hawkes_3) <- Event_No_Names_vec[c(5, 7)]
names(res.sim_hawkes_4) <- Event_No_Names_vec[c(1, 3, 5, 7)]

## Check number of simulated vs true
n_sim <- lapply(res.sim_hawkes, function(x) NROW(x)) %>% unlist()
n_sim_3 <- lapply(res.sim_hawkes_3, function(x) NROW(x)) %>% unlist()
n_sim_4 <- lapply(res.sim_hawkes_4, function(x) NROW(x)) %>% unlist()

n_true[c(1, 3)]; n_sim
n_true[c(5, 7)]; n_sim_3
n_true[c(1, 3, 5, 7)]; n_sim_4

## Calculate Intensities of events
func.intensity <- function(lambda_opt, alpha_opt, beta_opt, res, tend)
{
  
  mu <- lambda_opt
  mult_constant <- alpha_opt*beta_opt
  integral <- sum(exp(-beta_opt*(res[tend] - res[c(1:(tend-1))])))
  lambda_t <- mu + mult_constant*integral
  
  return(lambda_t)
}

## Objectives checking... Lots of local minimum in optim()
func.obj_calc <- function(par_mat, k, n_tests, alpha_range, beta_range,
                          lambda_range, columns)
{

  alpha_diags <- lapply(seq(alpha_range[1], alpha_range[2],
                            length.out = n_tests^2), diag, nrow = 2)
  beta_comb <- expand.grid(seq(beta_range[1], beta_range[2], length.out = n_tests),
                           seq(beta_range[1], beta_range[2], length.out = n_tests))
  lambda_comb <- expand.grid(seq(lambda_range[1], lambda_range[2], length.out = n_tests),
                             seq(lambda_range[1], lambda_range[2], length.out = n_tests))

  z_lambda <- numeric(dim(lambda_comb)[1])
  z_alpha <- numeric(dim(lambda_comb)[1])
  z_beta <- numeric(dim(lambda_comb)[1])

  for(i in 1:dim(lambda_comb)[1])
  {
    ## Constant Lambda
    tmp.pars <- c(as.numeric(par_mat$lambda0_opt),
                  as.numeric(alpha_diags[[i]]),
                  as.numeric(beta_comb[i, ]))
    z_lambda[i] <- func_likelihoodHawkes(tmp.pars, k, columns = columns)
    ## Constant Alpha
    tmp.pars <- c(as.numeric(lambda_comb[i, ]),
                  as.numeric(par_mat$alpha_opt),
                  as.numeric(beta_comb[i, ]))
    z_alpha[i] <- func_likelihoodHawkes(tmp.pars, k, columns = columns)
    ## Constant Beta
    tmp.pars <- c(as.numeric(as.numeric(lambda_comb[i, ])),
                  as.numeric(alpha_diags[[i]]),
                  as.numeric(par_mat$beta_opt))
    z_beta[i] <- func_likelihoodHawkes(tmp.pars, k, columns = columns)
  }

  Z_lambdas <- matrix(z_lambda, nrow = n_tests)
  Z_betas <- matrix(z_beta, nrow = n_tests)
  Z_alphas <- matrix(z_alpha, nrow = n_tests)

  return(list(Z_lambdas = Z_lambdas, Z_betas = Z_betas, Z_alphas = Z_alphas))

}

alpha_range_1 <- c(0.01, 0.04)
beta_range_1 <- c(0.041, 0.06)
lambda_range_1 <- c(0.001, 0.01)
test1 <- func.obj_calc(params_mat_hawkes, 2, 100,
                       alpha_range_1, beta_range_1, lambda_range_1, c(1, 3))

alpha_range_3 <- c(0.06, 0.08)
beta_range_3 <- c(0.081, 0.11)
lambda_range_3 <- c(0.02, 0.1)
test3 <- func.obj_calc(params_mat_hawkes_3, 2, 100,
                       alpha_range_3, beta_range_3, lambda_range_3, c(5, 7))

func.plot_obj <- function(n_tests, z, levels, title)
{
  cols = c('green','yellow','blue')

  filled.contour(seq(0, 1, length.out = n_tests), seq(0, 1, length.out = n_tests), z,
                 plot.axes = {contour(seq(0, 1, length.out = n_tests),
                                      seq(0, 1, length.out = n_tests),
                                      z, add = T, nlevels = 30)}, main = title,
                 color.palette = colorRampPalette(cols))

}

setwd(dir_figs)
cairo_pdf("HFT_Ass3_fig_objs_agg_E13_lam.pdf", height = 5, width = 5)
func.plot_obj(100, test1$Z_lambdas, 30, expression("Constant "~lambda))
dev.off()

cairo_pdf("HFT_Ass3_fig_objs_agg_E13_beta.pdf", height = 5, width = 5)
func.plot_obj(100, test1$Z_betas, 30, expression("Constant "~beta))
dev.off()

cairo_pdf("HFT_Ass3_fig_objs_agg_E13_alpha.pdf", height = 5, width = 5)
func.plot_obj(100, test1$Z_alphas, 30, expression("Constant "~alpha))
dev.off()

cairo_pdf("HFT_Ass3_fig_objs_agg_E57_lam.pdf", height = 5, width = 5)
func.plot_obj(100, test3$Z_lambdas, 30, expression("Constant "~lambda))
dev.off()

cairo_pdf("HFT_Ass3_fig_objs_agg_E57_beta.pdf", height = 5, width = 5)
func.plot_obj(100, test3$Z_betas, 30, expression("Constant "~beta))
dev.off()

cairo_pdf("HFT_Ass3_fig_objs_agg_E57_alpha.pdf", height = 5, width = 5)
func.plot_obj(100, test3$Z_alphas, 30, expression("Constant "~alpha))
dev.off()

func.res_sim_hawkes <- function(res, k_events, param_mat, event_no)
{
  
  colour_vec <- c("black", "firebrick2", "forestgreen", "gold", 
                  "darkorange", "darkorchid3", "violetred1", "dodgerblue3")
  
  ## Optimal Parameters per Event
  tmp.mu <- as.numeric(param_mat$lambda0_opt)
  tmp.alpha <- diag(param_mat$alpha_opt)
  tmp.beta <-  as.numeric(param_mat$beta_opt)
  lambda_t <- numeric()
  lambda_t_list <- list()
  
  ## Calculate for each event at each arrival
  for(event in 1:k_events){
    for(i in 2:NROW(res[[event]])){
      lambda_t[i] <- func.intensity(tmp.mu[event], tmp.alpha[event], 
                                    tmp.beta[event], res[[event]], i)
    }
    lambda_t_list[[event]] <- na.omit(lambda_t)
    lambda_t <- numeric()
  }
  
  ## find max/min for plots
  tmp_max_intensity <- max(unlist(lapply(lambda_t_list, max)))
  tmp_min_lambdas <- min(unlist(lapply(lambda_t_list, NROW)))
  tmp_max_lambdas <- max(unlist(lapply(lambda_t_list, NROW)))
  
  if(k_events > 2){
    m <- matrix(c(1,2,3,4,5,5), nrow = 3, ncol = 2, byrow = TRUE)
    layout(mat = m, heights = c(2,2,1.5))
    }else{
  m <- matrix(c(1,2,3,3), nrow = 2, ncol = 2,byrow = TRUE)
  layout(mat = m, heights = c(0.4,0.4,0.2))}
  
  par(mar = c(2, 2, 2, 2))
  #par(mfrow = c(1, 2))
  for(i in 1:k_events){
    plot(y = lambda_t_list[[i]],
         x = seq(DayStartTime, DayEndTime, length.out = NROW(lambda_t_list[[i]])),
         type = 'l', col = colour_vec[i],
         ylab = expression(lambda~"(t)"), xlab = "Time",
         main = paste("Event No.", event_no[i], sep = " "))
  }
  #par(mfrow = c(1, 1))
  plot(y = c(0, tmp_max_intensity), x = c(0, tmp_max_lambdas),
       ylab = expression(lambda~"(t)"), type = 'n',
       xlab = '', yaxs = 'i', xaxs = 'i', xaxt="n")
  for(i in 1:k_events){
    lines(lambda_t_list[[i]], col = colour_vec[i], lwd = 1.5) }
}

setwd(dir_figs)
cairo_pdf("HFT_Ass3_fig_intensities_agg.pdf", height = 7, width = 7)
func.res_sim_hawkes(res.sim_hawkes, 2, params_mat_hawkes, c(1, 3))
dev.off()

cairo_pdf("HFT_Ass3_fig_intensities_pass.pdf", height = 7, width = 7)
func.res_sim_hawkes(res.sim_hawkes_3, 2, params_mat_hawkes_3, c(5, 7))
dev.off()

cairo_pdf("HFT_Ass3_fig_intensities_agg_and_pass.pdf", height = 10, width = 10)
func.res_sim_hawkes(res.sim_hawkes_4, 4, params_mat_hawkes_4, c(1, 3, 5, 7))
dev.off()

colour_vec <- c("black", "firebrick2", "forestgreen", "gold", 
                "darkorange", "darkorchid3", "violetred1", "dodgerblue3")

## Optimal Parameters per Event
tmp.mu <- as.numeric(params_mat_hawkes_4$lambda0_opt)
tmp.alpha <- diag(params_mat_hawkes_4$alpha_opt)
tmp.beta <-  as.numeric(params_mat_hawkes_4$beta_opt)
lambda_t <- numeric()

lambda_t_list <- list()
## Calculate for each event at each arrival
for(event in 1:length(res.sim_hawkes_4)){
  for(i in 2:NROW(res.sim_hawkes_4[[event]])){
    lambda_t[i] <- func.intensity(tmp.mu[event], tmp.alpha[event], 
                                  tmp.beta[event], res.sim_hawkes_4[[event]], i)
  }
  lambda_t_list[[event]] <- na.omit(lambda_t)
  lambda_t <- numeric()
}

list_intensity <- list()
types_vec <- c('AggSell', 'AggBuy', 'PassBuy', 'PassSell')

for(i in 1:length(res.sim_hawkes_4)){
df_intensity <- data.frame(Intensity = lambda_t_list[[i]],
                           Time = DayStartTime + res.sim_hawkes_4[[i]][-1],
                           Count = 1:(NROW(res.sim_hawkes_4[[i]])-1),
                           Event = types_vec[i])

list_intensity[[i]] <- df_intensity
}


setwd(dir_figs)
cairo_pdf("HFT_Ass3_fig_intensities_agg_wcount.pdf", height = 7, width = 7)
dat_plot_intensities <- do.call(rbind, list_intensity[c(1,2)]) %>% arrange(Time)
scaleFactor <- max(dat_plot_intensities$Intensity) / max(dat_plot_intensities$Count)
ggplot(dat_plot_intensities, aes(x=Time, color = Event)) +
  geom_line(aes(y=Intensity)) +
  geom_line(aes(y=Count * scaleFactor)) +
  scale_y_continuous(name="Intensity", 
                     sec.axis=sec_axis(~.*1/scaleFactor, name="Count")) +
  theme_bw()
dev.off()

cairo_pdf("HFT_Ass3_fig_intensities_pass_wcount.pdf", height = 7, width = 7)
dat_plot_intensities <- do.call(rbind, list_intensity[c(3,4)]) %>% arrange(Time)
scaleFactor <- max(dat_plot_intensities$Intensity) / max(dat_plot_intensities$Count)
ggplot(dat_plot_intensities, aes(x=Time, color = Event)) +
  geom_line(aes(y=Intensity)) +
  geom_line(aes(y=Count * scaleFactor)) +
  scale_y_continuous(name="Intensity", 
                     sec.axis=sec_axis(~.*1/scaleFactor, name="Count")) +
  theme_bw()
dev.off()

cairo_pdf("HFT_Ass3_fig_intensities_agg_and_pass_wcount.pdf", height = 7, width = 7)
dat_plot_intensities <- do.call(rbind, list_intensity) %>% arrange(Time)
scaleFactor <- max(dat_plot_intensities$Intensity) / max(dat_plot_intensities$Count)
ggplot(dat_plot_intensities, aes(x=Time, color = Event)) +
  geom_line(aes(y=Intensity)) +
  geom_line(aes(y=Count * scaleFactor)) +
  scale_y_continuous(name="Intensity", 
                     sec.axis=sec_axis(~.*1/scaleFactor, name="Count")) +
  theme_bw()
dev.off()

# 3. Check Exponential results of data ----

# make list of dfs..
test_interarrivals <- lapply(All_Arrivals_list_of_vectors[c(1, 3, 5, 7)],
                            diff, differences = 1)
 
df <- list(); k_events = 4
for(j in 1:k_events){
df[[j]] <- list(inter_arrivals = test_interarrivals[[j]],
               alpha_opt = diag(params_mat_hawkes_4$alpha_opt)[j],
               beta_opt = params_mat_hawkes_4$beta_opt[j],
               lambda_opt = params_mat_hawkes_4$lambda0_opt[j])
}

names(df) <- Event_No_Names_vec[c(1, 3, 5, 7)]

func.exponential_check <-  function(df)
{
   
 A = c()
 E = c()
 A0 = 1 
 E0 = 1
 A[1] = E0*exp(-df$beta_opt * df$inter_arrivals[1])
 E[1] = exp (- df$beta_opt*df$inter_arrivals[1])*A[1]
 
 for( i in 2:(length(df$inter_arrivals)) ){
   E[i] = 1 + exp(-df$beta_opt*df$inter_arrivals[i-1])*A[i -1]
   A[i] = E[i - 1]*exp(-df$beta_opt*df$inter_arrivals[i])
 }
 
 tmp = c()
 tmp[1] = df$inter_arrivals[1]*df$lambda_opt +
   df$alpha_opt*(1- exp(- df$beta_opt*df$inter_arrivals[1]))*E0
 
 for( i in 2:(length(df$inter_arrivals)) ){
   tmp[i] = df$inter_arrivals[i]*df$lambda_opt +
     df$alpha_opt*(1- exp(- df$beta_opt*df$inter_arrivals[i-1]))*E[i -1]
 }
 
 Total_duration = tmp
 return(Total_duration)
 
}
     
check_exps <- lapply(df, func.exponential_check)
tmp_exp_names <- c(1, 3, 5, 7)

setwd(dir_figs)
cairo_pdf("HFT_Ass3_fig_check_exps.pdf", height = 10, width = 10)
par(mfrow = c(2, 2))
for(i in 1:k_events){
  main = paste("Event No.", tmp_exp_names[i], sep = " ")
  qqplot(qexp(ppoints(length(check_exps[[i]])), 
              rate = 1/mean(check_exps[[i]]) ), check_exps[[i]],
         xlab = "Exponential Quantiles", ylab = '', main = main)
  qqline(check_exps[[i]], col = 'red')
}
dev.off()

# 4. Simluate the Order-Book ----

## Save workspace so we can use same data...
setwd(dir_data)
save.image(file = "dat_SimHawkes.RData")
setwd(dir_script)

## Initialise Midprice and spread
MP_init <- 100
spread_init <- 4
## Sample initial prices and volumes using distributions???
sample_init_prices <- rnorm(1, 100, 20)
sample_init_vols <- rtrunc(1, "norm", mean = 200 , sd = 300)

##
simulated_arrivals <- res.sim_hawkes_4

## Update the Order-Book
types_vec <- c('AggSell', 'AggBuy', 'PassBuy', 'PassSell')
class(simulated_arrivals$Event_No1)
sim_dfs <- list()

for(i in 1:length(simulated_arrivals)){
  sim_dfs[[i]] <- data.frame(time = simulated_arrivals[[i]],
                             type = types_vec[i])
}

dat_ALL_sim_arrivals <- do.call(rbind, sim_dfs) %>%
  arrange(time)

DayLength <- NROW(dat_ALL_sim_arrivals)

sim_bid_prices <- c()
sim_ask_prices <- c()
spread_t <- c()
dat_true_init <- dat_NPN_oneday_classified[1, ]
sim_bid_prices[1] <- dat_true_init$L1.Bid
sim_ask_prices[1] <- dat_true_init$L1.Ask

func.sim_LOB <- function(daylength)
{
  for(t in 2:daylength){
    order <- dat_ALL_sim_arrivals[(t-1), ]
    spread <- max(sim_ask_prices[t-1] - sim_bid_prices[t-1], 0.00001)
    spread_t[t] <- spread
    if(order$type == 'AggSell'){
      tmp_neg_move = rexp(1, rate = 1)  # some distribution?
      sim_bid_prices[t] = sim_bid_prices[t-1] - tmp_neg_move
      sim_ask_prices[t] = sim_ask_prices[t-1] 
    }else if(order$type == 'AggBuy'){
      tmp_pos_move = rexp(1, rate = 1) # some distribution?
      sim_bid_prices[t] = sim_bid_prices[t-1]
      sim_ask_prices[t] = sim_ask_prices[t-1] + tmp_pos_move
    }else if(order$type == 'PassBuy'){
      tmp_pos_move_pass_buy = rtrunc(1, "norm", b = spread, mean = 0, sd = 100) # some distribution?
      sim_bid_prices[t] = sim_bid_prices[t-1] + tmp_pos_move_pass_buy
      sim_ask_prices[t] = sim_ask_prices[t-1]
    }else if(order$type == 'PassSell'){
      tmp_neg_move_pass_sell = rtrunc(1, "norm", b = spread, mean = 0, sd = 100) # some distribution?
      sim_bid_prices[t] = sim_bid_prices[t-1] 
      sim_ask_prices[t] = sim_ask_prices[t-1] - tmp_neg_move_pass_sell
    }
  }
  return(data.frame(Sim_L1.Bid = sim_bid_prices, Sim_L1.Ask = sim_ask_prices,
                    spread = spread_t))
}

test_OB <- func.sim_LOB(DayLength)

plot(y=test_OB$Sim_L1.Bid, x = (DayStartTime + dat_ALL_sim_arrivals$time), 
     type = 'l', xlab = "Time", ylab = 'Price')
lines(y=test_OB$Sim_L1.Ask, x = (DayStartTime + dat_ALL_sim_arrivals$time), col ='blue')

setwd(dir_figs)
cairo_pdf('HFT_Ass3_fig_simulatedLOB3.pdf', height = 5, width = 5)
plot(y=test_OB$Sim_L1.Bid, x = (DayStartTime + dat_ALL_sim_arrivals$time), 
     type = 'l', xlab = "Time", ylab = 'Price')
lines(y=test_OB$Sim_L1.Ask, x = (DayStartTime + dat_ALL_sim_arrivals$time), col ='blue')
dev.off()

cairo_pdf('HFT_Ass3_fig_trueLOB.pdf', height = 5, width = 5)
plot(na.omit(dat_NPN_oneday_classified$L1.Bid), type = 'l',
     xlab = "Time", ylab = 'Price', x = seq(DayStartTime, DayEndTime,
             length.out = length(na.omit(dat_NPN_oneday_classified$L1.Bid))))
lines(na.omit(dat_NPN_oneday_classified$L1.Ask), col = 'blue',
      x = seq(DayStartTime, DayEndTime, 
              length.out = length(na.omit(dat_NPN_oneday_classified$L1.Ask))))
dev.off()
