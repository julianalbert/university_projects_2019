##
#
# Author: Julian Albert
# Date: 02 August 2019
#
# Description:
# HFT Assignment 1 - Basically want to work with TAQ data and perform EDA to 
# better understand the data and the trading environment.
#
#------------------------------------------------------------------------------#

# 0. Clean Workspace, Directory and Library ----

## Clean Workspace
rm(list=ls())
# dev.off() # Close Plots
setwd("~") # Clear Path to User

## Locations
project_folder <- "/Documents/UCT/Coursework/HFT"
loc_script <- "/Assignment_1/UCT_Assignment/Code"
loc_figs <- "/Assignment_1/UCT_Assignment/Figs"

## Directories
dir_script <- paste("~", project_folder, loc_script, sep = '')
dir_figs <- paste("~", project_folder, loc_figs, sep = '')

## Set Working Directory to Script Location
setwd(dir_script)

## Libraries - Lazyload
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, lubridate, zoo, data.table, Cairo, viridis)

## Pull Clean Data
dat_TAQ <- readRDS(file = "dat_TAQ.rds")

## Options
options(digits.secs = 3)

# 1. Create Plots >> Trades/Quotes/MicroPrice ----

StartTime1 <- "10:00:00"
EndTime1 <- "11:00:00"
StartTime2 <- "16:00:00"
EndTime2 <- "17:00:00"
timezone <- "Africa/Johannesburg"

Stock1 <- "AGL: Anglo American PLC"
Stock2 <- "NPN: Naspers Ltd"

func.plot_EDA_1hour <- function(Data, StartTime, EndTime, Stock, Legend = FALSE)
{
  
  ## Randomly Select a Trading Day
  tmp_random_date <- Data %>%
    filter(Type == 'Trade') %>%
    slice(runif(1, 1, NROW(.))) %>%
    dplyr::select(DateTimeL)
  
  ## Define the Day
  tmp_ymd <- paste(year(tmp_random_date$DateTimeL), 
                   month(tmp_random_date$DateTimeL),
                   day(tmp_random_date$DateTimeL), sep = "-")
  
  ## Need to subset for the Trading Hours
  date_start <- as.POSIXct(paste(tmp_ymd, StartTime, sep = " "), tz = timezone)
  date_end <- as.POSIXct(paste(tmp_ymd, EndTime, sep = " "), tz = timezone)
  plot_interval <- interval(date_start, date_end)
  
  dat_hourly <- Data[Data$DateTimeL %within% plot_interval, ]
  
  ## Data in nice plotting format >> Long
  testprice <- dat_hourly %>% 
    rename(TradePrice = Price) %>%
    gather(key, price, c("L1.Ask", "L1.Bid","MicroPrice", "TradePrice")) %>%
    select(DateTimeL, key, price)
  
  testvols_quotes <- dat_hourly %>% 
    gather(key, volume, Volume.Ask:Volume.Bid) %>%
    select(DateTimeL, key, volume)
  
  testvols_mp <- data.frame(DateTimeL = dat_hourly$DateTimeL,
                        key = "Volume.MicroPrice",
                        volume = ifelse(!is.na(dat_hourly$L1.Ask), 
                                        0.5*min(testvols_quotes$volume, 
                                                na.rm = TRUE), NA)) %>% 
    mutate(key = as.character(key))
  
  testvols_trades <- dat_hourly %>% 
    gather(key, volume, Volume.Trade) %>%
    select(DateTimeL, key, volume)
  
  testvols <- bind_rows(testvols_quotes, testvols_mp, testvols_trades)
  
  test_comb <- data.frame(DateTimeL = testprice$DateTimeL,
                          Event = as.factor(testprice$key),
                          Value = testprice$price,
                          Volume = testvols$volume)
  
  ## Generic Titles
  time.title <- paste("Trading between ",
                      unlist(strsplit(StartTime, ":"))[1], 
                      "h", unlist(strsplit(StartTime, ":"))[2], " and ",
                      unlist(strsplit(EndTime, ":"))[1], 
                      "h", unlist(strsplit(EndTime, ":"))[2], sep = "")
  
  date.title <- date(date_start)
  
  ## Plot EDA

  plot <- test_comb %>% na.omit() %>%
    ggplot(aes(x = DateTimeL, y = Value/100, size = Volume, color = Event)) +
    geom_point() +
    theme_dark() +
    scale_color_manual(breaks = c("L1.Ask", "L1.Bid",
                                  "MicroPrice", "TradePrice"),
                       values=c("firebrick1", "royalblue1", 
                                "chartreuse3", "yellow")) + 
    labs(x = "\n Time (HH:MM:SS)", y = "Price (R's) \n", title = Stock,
         subtitle = paste(time.title, date.title, sep = " : ")) +
    scale_x_datetime(date_labels = "%H:%M:%S",
                     timezone = "Africa/Johannesburg") +
    theme(panel.background = element_rect(fill = 'grey15')) +
    theme(legend.key=element_blank()) +
    scale_size_continuous(guide = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 3))) 
  
  if(Legend == TRUE) {plot <- plot + theme(aspect.ratio=1)}
  else{plot <- plot + theme(legend.position ="none") + theme(aspect.ratio=1)}

  return(plot)
  
}

setwd(dir_figs)

# cairo_pdf("HFT_Ass1_fig_EDA_AGL_1011.pdf", height = 5, width = 5)
func.plot_EDA_1hour(dat_TAQ$AGL , StartTime1, EndTime1, Stock1, Legend = TRUE)
# dev.off()

# cairo_pdf("HFT_Ass1_fig_EDA_AGL_1617.pdf", height = 5, width = 5)
func.plot_EDA_1hour(dat_TAQ$AGL , StartTime2, EndTime2, Stock1, Legend = TRUE)
# dev.off()

# cairo_pdf("HFT_Ass1_fig_EDA_NPN_1011.pdf", height = 5, width = 5)
func.plot_EDA_1hour(dat_TAQ$NPN , StartTime1, EndTime1, Stock2, Legend = TRUE)
# dev.off()

# cairo_pdf("HFT_Ass1_fig_EDA_NPN_1617.pdf", height = 5, width = 5)
func.plot_EDA_1hour(dat_TAQ$NPN , StartTime2, EndTime2, Stock2, Legend = TRUE)
# dev.off()

setwd(dir_script)

# 2. Create Plots >> ACF ----

## Get TAQ that has +1(-1) Trade Signs only
dat_tradesigns <- lapply(dat_TAQ, function(x) x %>%
                           setDT() %>% .[!is.na(Trade.Sign) & Trade.Sign != 0])

## Total Length for N days
counts <- lapply(dat_tradesigns, function(x)  x %>%
                   setDT() %>% .[, length(Trade.Sign)])

## Get ACFs
acfs <- lapply( dat_tradesigns, function(x) x %>% 
                 .[, acf(Trade.Sign, lag.max = length(Trade.Sign))] )

## Nice dataframes
dat_AGL_ACF <- tibble(ACF = as.numeric(acfs$AGL$acf), 
                      Lag = as.numeric(acfs$AGL$lag))

dat_NPN_ACF <- tibble(ACF = as.numeric(acfs$NPN$acf), 
                      Lag = as.numeric(acfs$NPN$lag))

## Plot the things
setwd(dir_figs)

# cairo_pdf("HFT_Ass1_fig_ACF_Orderflow_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL_ACF, aes(x = Lag, y = ACF)) +
  geom_line() +
  scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(y = "ACF \n", x = "\n Lag", title = Stock1,
       subtitle = "Autocorrelation of Order-Flow on log-log Scale") +
  theme_bw() +
  theme(aspect.ratio = 0.75)
# dev.off()

# cairo_pdf("HFT_Ass1_fig_ACF_Orderflow_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN_ACF, aes(x = Lag, y = ACF)) +
  geom_line() +
  scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(y = "ACF \n", x = "\n Lag", title = Stock2,
       subtitle = "Autocorrelation of Order-Flow on log-log Scale") +
  theme_bw() +
  theme(aspect.ratio = 0.75)
# dev.off()

setwd(dir_script)

# 3. Create Plots >> Inter-Arrival Times ----

## Frequency of Inter-Arrival Times

setwd(dir_figs)

### Anglo
dat_AGL <- dat_TAQ$AGL

# cairo_pdf("HFT_Ass1_fig_Freq10s_InterArrival_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL[Inter.Arrival.times < 10], aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 10) +
  scale_fill_viridis() +
  labs(title = Stock1,
       subtitle = "Times less than 10 seconds",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_Freq60s_InterArrival_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL[Inter.Arrival.times < 60], aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 30) +
  scale_fill_viridis() +
  labs(title = Stock1,
       subtitle = "Times less than 60 seconds",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_Freq10m_InterArrival_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL[Inter.Arrival.times < 600], aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 50) +
  scale_fill_viridis() +
  labs(title = Stock1,
       subtitle = "Times less than 10 minutes",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_FreqAll_InterArrival_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL, aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 100) +
  scale_fill_viridis() +
  labs(title = Stock1,
       subtitle = "Times for all Trading Events - Absolute Scale",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_FreqAllLog_InterArrival_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL, aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 15) +
  scale_fill_viridis() +
  scale_x_log10() +
  labs(title = Stock1,
       subtitle = "Times for all Trading Events - Log Scale",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

### Naspers 
dat_NPN <- dat_TAQ$NPN

# cairo_pdf("HFT_Ass1_fig_Freq10s_InterArrival_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN[Inter.Arrival.times < 10], aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 10) +
  scale_fill_viridis() +
  labs(title = Stock2,
       subtitle = "Times less than 10 seconds",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_Freq60s_InterArrival_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN[Inter.Arrival.times < 60], aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 30) +
  scale_fill_viridis() +
  labs(title = Stock2,
       subtitle = "Times less than 60 seconds",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_Freq10m_InterArrival_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN[Inter.Arrival.times < 600], aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 50) +
  scale_fill_viridis() +
  labs(title = Stock2,
       subtitle = "Times less than 10 minutes",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_FreqAll_InterArrival_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN, aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 100) +
  scale_fill_viridis() +
  labs(title = Stock2,
       subtitle = "Times for all Trading Events - Absolute Scale",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

# cairo_pdf("HFT_Ass1_fig_FreqAllLog_InterArrival_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN, aes(x = Inter.Arrival.times)) +
  geom_histogram(aes(fill = ..count..), colour="black", bins = 14) +
  scale_fill_viridis() +
  scale_x_log10() +
  labs(title = Stock1,
       subtitle = "Times for all Trading Events - Log Scale",
       y = "", x = "\n Inter-Arrival Times (Seconds)") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "none")
# dev.off()

## QQ Plots

# cairo_pdf("HFT_Ass1_fig_QQExp_InterArrival_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL, aes(sample = Inter.Arrival.times)) +
  stat_qq_line(distribution = stats::qexp, colour = "firebrick1") +
  stat_qq(distribution = stats::qexp, geom = "point") + 
  labs(x = "\n Exponential Quantiles", y = "Inter-Arrival Quantiles \n",
       title = Stock1,
       subtitle = "QQ-Plot Relative to Exponential Distribution") +
  theme_bw() +
  theme(aspect.ratio = 1)
# dev.off()

# cairo_pdf("HFT_Ass1_fig_QQExp_InterArrival_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN, aes(sample = Inter.Arrival.times)) +
  stat_qq_line(distribution = stats::qexp, colour = "firebrick1") +
  stat_qq(distribution = stats::qexp, geom = "point") + 
  labs(x = "\n Exponential Quantiles", y = "Inter-Arrival Quantiles \n",
       title = Stock2,
       subtitle = "QQ-Plot Relative to Exponential Distribution") +
  theme_bw() +
  theme(aspect.ratio = 1)
# dev.off()

## ACF Plots of Inter-Arrival Times

acfs2 <- lapply( dat_tradesigns, function(x) x %>% 
                   .[, acf(na.omit(Inter.Arrival.times), 
                           lag.max = length(na.omit(Inter.Arrival.times)))] )

## Nice dataframes
dat_AGL_ACF_times <- tibble(ACF = as.numeric(acfs2$AGL$acf),
                            Lag = as.numeric(acfs2$AGL$lag))

dat_NPN_ACF_times <- tibble(ACF = as.numeric(acfs2$NPN$acf),
                            Lag = as.numeric(acfs2$NPN$lag))

## Plot the things
setwd(dir_figs)

# cairo_pdf("HFT_Ass1_fig_ACF_InterArrival_AGL.pdf", height = 5, width = 5)
ggplot(dat_AGL_ACF_times, aes(x = Lag, y = ACF)) +
  geom_line() +
  scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(y = "ACF \n", x = "\n Lag", title = Stock1,
       subtitle = "Autocorrelation of Inter-Arrival Times on log-log Scale") +
  theme_bw() +
  theme(aspect.ratio = 0.75)
# dev.off()

# cairo_pdf("HFT_Ass1_fig_ACF_InterArrival_NPN.pdf", height = 5, width = 5)
ggplot(dat_NPN_ACF_times, aes(x = Lag, y = ACF)) +
  geom_line() +
  scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(y = "ACF \n", x = "\n Lag", title = Stock2,
       subtitle = "Autocorrelation of Inter-Arrival Times on log-log Scale") +
  theme_bw() +
  theme(aspect.ratio = 0.75)
# dev.off()

setwd(dir_script)

#------------------------------------------------------------------------------#