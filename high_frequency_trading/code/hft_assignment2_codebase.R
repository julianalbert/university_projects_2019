##
#
# Author: Julian Albert
# Date: 20 August 2019
#
# Description:
# HFT Assignment 2 - Basically want to work with TAQ data and perform EDA to 
# better understand the data and the trading environment.
#
#------------------------------------------------------------------------------#

# 0. Clean Workspace, Directory and Library ----

## Clean Workspace
rm(list=ls())
dev.off() # Close Plots
setwd("~") # Clear Path to User

## Locations
project_folder <- "/Documents/UCT/Coursework/HFT"
loc_script <- "/Assignment_2/UCT_Assignment/Code"
loc_figs <- "/Assignment_2/UCT_Assignment/Figs"
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
p_load(tidyverse, lubridate, zoo, data.table, Cairo, viridis, 
       quantmod, grid, scales, pracma)

## Pull Clean Data
dat_TAQ <- readRDS(file = paste(dir_data, filen_dat, sep = ""))

## Options
options(digits.secs = 3)

# 1. Variables Needed ----

DayStartTime <- "09:00:00"
DayEndTime <- "17:00:00"
timezone <- "Africa/Johannesburg"

Stock1 <- "AGL: Anglo American PLC"
Stock2 <- "NPN: Naspers Ltd"

# 2. Functions ----

## Function to draw candlestick plots
func.draw_candles <- function(df, title_param, subtitle_param, alpha_param = 1)
{
  
  df$change <- ifelse(df$Close > df$Open, "up", 
                      ifelse(df$Close < df$Open, "down", "flat"))
  
  # So let us instead find delta (seconds) between 1st and 2nd row and just 
  # use it for all other rows. We check 1st 3 rows to avoid larger "weekend gaps"
  width_candidates <- c(as.numeric(difftime(df$DateTimeL[2], df$DateTimeL[1]), units = "secs"), 
                        as.numeric(difftime(df$DateTimeL[3], df$DateTimeL[2]), units = "secs"), 
                        as.numeric(difftime(df$DateTimeL[4], df$DateTimeL[3]), units = "secs"))
  
  df$width_s = min(width_candidates)  
  # define the vector of candle colours either by name or by rgb()
  #candle_colors = c("down" = "red", "up" = "green", "flat" = "blue")
  candle_colors = c("down" = rgb(192 , 0, 0, alpha = 255, maxColorValue = 255), 
                    "up" = rgb(0, 192, 0, alpha = 255, maxColorValue = 255), 
                    "flat" = rgb(0, 0, 192, alpha = 255, maxColorValue = 255))
  
  # Candle chart:
  g <- ggplot(df, aes(x = DateTimeL)) +
    geom_errorbar(aes(ymin = Low/100, ymax = High/100, colour = change), 
                  alpha = alpha_param, width = (df$width_s * 0.9)) +
    theme_bw() +
    labs(title = title_param, subtitle = subtitle_param,
         x = "\n Time (HH:MM:SS)", y = "Price (R's) \n") +
    scale_x_datetime(date_labels = "%H:%M:%S",
                     timezone = "Africa/Johannesburg") +
    geom_rect(aes(xmin = DateTimeL - width_s/2 * 0.9, 
                  xmax = DateTimeL + width_s/2 * 0.9, 
                  ymin = pmin(Open/100, Close/100), 
                  ymax = pmax(Open/100, Close/100), 
                  fill = change), alpha = alpha_param) + # candle recatngle
    guides(fill = FALSE, colour = FALSE) +
    scale_color_manual(values = candle_colors) + # color for line
    scale_fill_manual(values = candle_colors) # color for candle fill  
  
  # Handle special cases: flat bar and Open == close:
  if (any(df$change == "flat"))
  { 
    g <- g + geom_segment(data = df[df$change == "flat",], 
                          aes(x = DateTimeL - width_s / 2 * 0.9, 
                              y = Close/100, yend = Close/100,
                              xend = DateTimeL + width_s / 2 * 0.9, 
                              colour = change), 
                          alpha = alpha_param)
  }
  
  return(g)
  
}

## Function to subset data for plotting
func.plot_subset <- function(Data, Date_for_plot)
{

  ## Define the Day
  tmp_ymd <- paste(year(Date_for_plot), 
                   month(Date_for_plot),
                   day(Date_for_plot), sep = "-")
  
  ## Need to subset for the Trading Hours
  date_start <- as.POSIXct(paste(tmp_ymd, DayStartTime, sep = " "), tz = timezone)
  date_end <- as.POSIXct(paste(tmp_ymd, DayEndTime, sep = " "), tz = timezone)
  plot_interval <- interval(date_start, date_end)
  
  dat_hourly <- Data[Data$DateTimeL %within% plot_interval, ]
  return(dat_hourly)
}


## Function to calc by groups and return candlestick plots
func.by_interval_calcs <- function(TAQ_data_1_equity, # Take in data for one equity
         interval_numeric = 10, 
         interval_time_unit = "min", # time for intervals i.e 1, "min"
         DayStartTime = "09:00:00", 
         DayEndTime = "17:00:00", 
         Type = "Trade",
         Stock_name,
         Date_for_plot = Date_for_plot)
{
  
  ## Specify our time interval to group data by
  interval <- paste(interval_numeric, interval_time_unit, sep = " ")
  ## Group Data by time interval >> initialise OHLC columns
  dat_interval <- TAQ_data_1_equity %>%
    group_by(Int = cut(DateTimeL, interval)) %>%
    ungroup() %>%
    mutate(Open = NA, High = NA, Low = NA, Close = NA)
  
  ## We are going to want the plots for both transaction data and for 
  ## Quote data, make function that deals with this
  if(Type == "Quote"){
    
  ## If we want quote data: initialise trade data columns for merge
  tmp.interval_OHLCV <- dat_interval %>% 
    filter(Type != "Quote") %>%
    mutate(Tot_Volume_OHLCV = NA, MicroPrice_OHLCV = NA)
  ## Calculate required measures for the Quote data by interval
  tmp.interval_OHLCV_calcs <- dat_interval %>%
    filter(Type == "Quote") %>%
    group_by(Int) %>%
    mutate(Open = first(na.omit(MidPrice)),
           High = max(na.omit(MidPrice)),
           Low = min(na.omit(MidPrice)),
           Close = last(na.omit(MidPrice)),
           Tot_Volume_OHLCV = Volume.Bid + Volume.Ask,
           MicroPrice_OHLCV = weighted.mean(MidPrice, Tot_Volume_OHLCV)) %>%
    ungroup()
  
  ## Bind Trades and Quotes together to give us calcs. by interval
  df <- bind_rows(tmp.interval_OHLCV, tmp.interval_OHLCV_calcs) %>%
    arrange(DateTimeL)
  
  ## For the Plot we can Seperate data into Quotes only >> subset for a day
  dat.plot_candles <- df %>%
    filter(Type == "Quote") %>%
    func.plot_subset(Date_for_plot)
  
  } else{
    
  ## If we want trade data: initialise quote data columns for merge
  tmp.interval_OHLCV <- dat_interval %>% 
    filter(Type != "Trade") %>%
    mutate(Volume_OHLCV = NA, VWAP = NA)
  
  ## Calculate required measures for the Trade data by interval
  tmp.interval_OHLCV_calcs <- dat_interval %>%
    filter(Type == "Trade") %>%
    group_by(Int) %>%
    mutate(Open = first(Price),
           High = max(Price),
           Low = min(Price),
           Close = last(Price),
           Volume_OHLCV = sum(Volume.Trade),
           VWAP = weighted.mean(Price, Volume.Trade)) %>%
    ungroup()
  
  ## Bind Trades and Quotes together to give us calcs. by interval
  df <- bind_rows(tmp.interval_OHLCV, tmp.interval_OHLCV_calcs) %>%
    arrange(DateTimeL)
  
  ## For the Plot we can Seperate data into Trades only >> subset for a day
  dat.plot_candles <- df %>%
    filter(Type == "Trade") %>%
    func.plot_subset(Date_for_plot)
  
  }
  
  ## Generic Titles
  time.title <- paste(Type, " Events between ",
                      unlist(strsplit(DayStartTime, ":"))[1], 
                      "h", unlist(strsplit(DayStartTime, ":"))[2], " and ",
                      unlist(strsplit(DayEndTime, ":"))[1], 
                      "h", unlist(strsplit(DayEndTime, ":"))[2], sep = "")
  
  date.title <- date(dat.plot_candles$DateTimeL[1])
  
  plot_subtitle <- paste(date.title, time.title, sep = " : ")
  
  ## Plot
  candle_plot <- dat.plot_candles %>%
    group_by(Int) %>%
    slice(n()) %>%
    func.draw_candles(title_param = Stock_name, 
                      subtitle_param = plot_subtitle)

  return(list(Data_used = df,
              Plot = candle_plot))
  
}

## Function to get micro-prices and VWAP as points for plot
func.plot_other <- function(Data, Type = "Trade", Date_for_plot)
{
  
  if(Type == "Trade"){
    
    ## For trade
    point_data <- Data %>%
      filter(Type == "Trade",
             date(DateTimeL) == date(Date_for_plot)) %>%
      group_by(Int) %>%
      slice(n())
    
    points <- point_data$VWAP/100
    
    hist_data <- Data %>%
      filter(Type == "Trade",
             date(DateTimeL) == date(Date_for_plot)) %>%
      group_by(Int) %>%
      slice(n())
    
  } else{
    
    ## For trade
    point_data <- Data %>%
      filter(Type == "Quote",
             date(DateTimeL) == date(Date_for_plot)) %>%
      group_by(Int) %>%
      slice(n())
    
    points <- point_data$MicroPrice_OHLCV/100
    
    hist_data <- Data %>%
      filter(Type == "Quote",
             date(DateTimeL) == date(Date_for_plot)) %>%
      group_by(Int) %>%
      mutate(Volume_OHLCV = sum(Tot_Volume_OHLCV)) %>%
      slice(n())
    
  }
  
  return(list(Points = points,
              HistData = hist_data))
  
}

## Final Plot for candlesticks and histogram
func.final_plot <- function(dat_event_by_interval, 
                            points_event,
                            hist_event,
                            point_size = 1)
{
  
  plot_candles <- dat_event_by_interval$Plot +
    geom_point(aes(y = points_event), size = point_size) +
    labs(x = "", y = "Price (R's) \n") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  plot_hist <- ggplot(hist_event, aes(DateTimeL, Volume_OHLCV/1000)) +
    geom_bar(stat = "identity", color = "black")  +
    theme_bw() +
    labs(x = "\n Time (HH:MM:SS)", y = "Volume (1000's) \n") +
    scale_x_datetime(date_labels = "%H:%M:%S",
                     timezone = "Africa/Johannesburg")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(plot_candles), 
                  ggplotGrob(plot_hist), size = "last"))
  
}

## Function to specify scientific notation 10^# for plots
func.fancy_scientific_labels <- function(l) 
{
  index_zero <- which(l == 0)
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  l[index_zero] <- "0"
  parse(text=l)
}

# i.e. ggplot() + scale_y_continuous(labels = func.fancy_scientific_labels) 

# 3. Plot Candlesticks for AGL 10 minutes ----

idx <- ceiling(runif(1, 0, NROW(dat_TAQ$AGL$DateTimeL)))
Date_for_plot <- dat_TAQ$AGL$DateTimeL[idx]

## Plots for the Transactions
dat_AGL_10min_trades <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$AGL,
                                               interval_numeric = 10,
                                               Type = "Trade",
                                               Stock_name = Stock1,
                                               Date_for_plot = Date_for_plot)

dat_AGL_10min_trades_other <- func.plot_other(dat_AGL_10min_trades$Data_used, 
                                              Type = "Trade",
                                              Date_for_plot = Date_for_plot)

dat_AGL_1min_trades <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$AGL,
                                               interval_numeric = 1,
                                               Type = "Trade",
                                               Stock_name = Stock1,
                                               Date_for_plot = Date_for_plot)

dat_AGL_1min_trades_other <- func.plot_other(dat_AGL_1min_trades$Data_used, 
                                              Type = "Trade",
                                              Date_for_plot = Date_for_plot)

setwd(dir_figs)
cairo_pdf("HFT_Ass2_fig_AGL10min_trades.pdf", height = 5, width = 5)
func.final_plot(dat_AGL_10min_trades, 
                dat_AGL_10min_trades_other$Points, 
                dat_AGL_10min_trades_other$HistData)
dev.off()

cairo_pdf("HFT_Ass2_fig_AGL1min_trades.pdf", height = 5, width = 5)
func.final_plot(dat_AGL_1min_trades, 
                dat_AGL_1min_trades_other$Points, 
                dat_AGL_1min_trades_other$HistData,
                point_size = 0.1)
dev.off()
setwd(dir_script)

## Plots for the Quotes
dat_AGL_10min_quotes <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$AGL,
                                               interval_numeric = 10,
                                               Type = "Quote",
                                               Stock_name = Stock1,
                                               Date_for_plot = Date_for_plot)

dat_AGL_10min_quotes_other <- func.plot_other(dat_AGL_10min_quotes$Data_used, 
                                              Type = "Quote",
                                              Date_for_plot = Date_for_plot)

dat_AGL_1min_quotes <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$AGL,
                                               interval_numeric = 1,
                                               Type = "Quote",
                                               Stock_name = Stock1,
                                               Date_for_plot = Date_for_plot)

dat_AGL_1min_quotes_other <- func.plot_other(dat_AGL_1min_quotes$Data_used, 
                                              Type = "Quote",
                                              Date_for_plot = Date_for_plot)

setwd(dir_figs)
cairo_pdf("HFT_Ass2_fig_AGL10min_quotes.pdf", height = 5, width = 5)
func.final_plot(dat_AGL_10min_quotes, 
                dat_AGL_10min_quotes_other$Points, 
                dat_AGL_10min_quotes_other$HistData)
dev.off()

cairo_pdf("HFT_Ass2_fig_AGL1min_quotes.pdf", height = 5, width = 5)
func.final_plot(dat_AGL_1min_quotes, 
                dat_AGL_1min_quotes_other$Points, 
                dat_AGL_1min_quotes_other$HistData,
                point_size = 0.1)
dev.off()
setwd(dir_script)

# 4. Plot Candlesticks for NPN  10 minutes  ----

idx <- ceiling(runif(1, 0, NROW(dat_TAQ$NPN$DateTimeL)))
Date_for_plot <- dat_TAQ$NPN$DateTimeL[idx]

## Plots for the Transactions
dat_NPN_10min_trades <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$NPN,
                                               interval_numeric = 10,
                                               Type = "Trade",
                                               Stock_name = Stock2,
                                               Date_for_plot = Date_for_plot)

dat_NPN_10min_trades_other <- func.plot_other(dat_NPN_10min_trades$Data_used, 
                                              Type = "Trade",
                                              Date_for_plot = Date_for_plot)

dat_NPN_1min_trades <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$NPN,
                                               interval_numeric = 1,
                                               Type = "Trade",
                                               Stock_name = Stock2,
                                               Date_for_plot = Date_for_plot)

dat_NPN_1min_trades_other <- func.plot_other(dat_NPN_1min_trades$Data_used, 
                                              Type = "Trade",
                                              Date_for_plot = Date_for_plot)

setwd(dir_figs)
cairo_pdf("HFT_Ass2_fig_NPN10min_trades.pdf", height = 5, width = 5)
func.final_plot(dat_NPN_10min_trades, 
                dat_NPN_10min_trades_other$Points, 
                dat_NPN_10min_trades_other$HistData)
dev.off()

cairo_pdf("HFT_Ass2_fig_NPN1min_trades.pdf", height = 5, width = 5)
func.final_plot(dat_NPN_1min_trades, 
                dat_NPN_1min_trades_other$Points, 
                dat_NPN_1min_trades_other$HistData,
                point_size = 0.1)
dev.off()
setwd(dir_script)

## Plots for the Quotes
dat_NPN_10min_quotes <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$NPN,
                                               interval_numeric = 10,
                                               Type = "Quote",
                                               Stock_name = Stock2,
                                               Date_for_plot = Date_for_plot)

dat_NPN_10min_quotes_other <- func.plot_other(dat_NPN_10min_quotes$Data_used, 
                                              Type = "Quote",
                                              Date_for_plot = Date_for_plot)

dat_NPN_1min_quotes <- func.by_interval_calcs(TAQ_data_1_equity = dat_TAQ$NPN,
                                               interval_numeric = 1,
                                               Type = "Quote",
                                               Stock_name = Stock2,
                                               Date_for_plot = Date_for_plot)

dat_NPN_1min_quotes_other <- func.plot_other(dat_NPN_1min_quotes$Data_used, 
                                              Type = "Quote",
                                              Date_for_plot = Date_for_plot)

setwd(dir_figs)
cairo_pdf("HFT_Ass2_fig_NPN10min_quotes.pdf", height = 5, width = 5)
func.final_plot(dat_NPN_10min_quotes, 
                dat_NPN_10min_quotes_other$Points,
                dat_NPN_10min_quotes_other$HistData)
dev.off()

cairo_pdf("HFT_Ass2_fig_NPN1min_quotes.pdf", height = 5, width = 5)
func.final_plot(dat_NPN_1min_quotes,
                dat_NPN_1min_quotes_other$Points,
                dat_NPN_1min_quotes_other$HistData,
                point_size = 0.1)
dev.off()
setwd(dir_script)

# 5. Time Series Stylised facts ----

func.freq_plot <- function(data)
{
  
    n <- length(data)
    mean <- mean(data)
    sd <- sd(data)
    bins <- seq(min(data), max(data), length = 50)
    binwidth <- diff(bins)[1]
    
    ggplot(as.data.frame(data), 
           aes(x = data, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
      geom_histogram(binwidth = binwidth, colour = "black", fill = "white") +
      stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd)*n*binwidth,
                    color = "red", size = 0.5) +
      theme_bw()
}

## Frequency plot >> VWAP
AGL_freq_trades <- dat_AGL_1min_trades$Data_used %>%
  filter(Type == "Trade") %>%
  group_by(Int) %>%
  slice(n())

setwd(dir_figs)
cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_Freq.pdf", width = 5, height = 5)
func.freq_plot(AGL_freq_trades$VWAP) +
  labs(x = "\n VWAP", y = "Count \n", title = Stock1,
       subtitle = "1-minute VWAP Frequency")
dev.off()

cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_Freq_lt41000.pdf", width = 5, height = 5)
func.freq_plot(AGL_freq_trades$VWAP[AGL_freq_trades$VWAP < 41000]) +
  labs(x = "\n VWAP", y = "Count \n", title = Stock1,
       subtitle = "1-minute VWAP Frequency : Excl. Outlier > 41000")
dev.off()

## Frequency plot >> Micro-Price
AGL_freq_quotes <- dat_AGL_1min_quotes$Data_used %>%
  filter(Type == "Quote" & !is.na(MicroPrice_OHLCV)) %>%
  group_by(Int) %>%
  slice(n())

cairo_pdf("HFT_Ass2_fig_AGL1min_MicroPrice_Freq.pdf", width = 5, height = 5)
func.freq_plot(AGL_freq_quotes$MicroPrice_OHLCV) +
  labs(x = "\n MicroPrice", y = "Count \n", title = Stock1,
       subtitle = "1-minute MicroPrice Frequency")
dev.off()

## QQ plot does this so we use it for GGplot because different lengths
sx <- sort(AGL_freq_quotes$MicroPrice_OHLCV)
sy <- sort(AGL_freq_trades$VWAP)
lenx <- length(sx)
leny <- length(sy)
if (leny < lenx)sx <- approx(1L:lenx, sx, n = leny)$y
if (leny > lenx)sy <- approx(1L:leny, sy, n = lenx)$y

cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_MicroPrice_QQ.pdf", width = 5, height = 5)
ggplot() + 
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_point(aes(x=sx, y=sy)) +
  theme_bw() +
  labs(y = "VWAP \n", x = "\n MicroPrice", title = Stock1, 
       subtitle = "QQ-Plot for 1-minute VWAP vs MicroPrice Quantiles") ## Highly correlated
dev.off()

## Detrend
sx <- sort(diff(log(AGL_freq_quotes$MicroPrice_OHLCV)))
sy <- sort(diff(log(AGL_freq_trades$VWAP)))
lenx <- length(sx)
leny <- length(sy)
if (leny < lenx)sx <- approx(1L:lenx, sx, n = leny)$y
if (leny > lenx)sy <- approx(1L:leny, sy, n = lenx)$y

cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_MicroPrice_QQ_DiffLog.pdf", width = 5, height = 5)
ggplot() + 
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_point(aes(x=sx, y=sy)) +
  theme_bw() +
  labs(y = "VWAP \n", x = "\n MicroPrice", title = Stock1, 
       subtitle = "QQ-Plot for 1-minute Differenced VWAP vs MicroPrice Quantiles") ## MP fatter tails
dev.off()

## Tails >> VWAP

### Lower
qlower <- quantile((AGL_freq_trades$VWAP), 0.05)
sample_qlower <- (AGL_freq_trades$VWAP)[(AGL_freq_trades$VWAP) <= qlower]

cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_Freq_TailLower.pdf", width = 5, height = 5)
func.freq_plot(sample_qlower)  +
  labs(x = "\n VWAP", y = "Count \n", title = Stock1,
       subtitle = "1-minute VWAP Frequency - Lower Tail (5%)")
dev.off()

### Upper
qupper <- quantile((AGL_freq_trades$VWAP), 0.95)
sample_qupper <- (AGL_freq_trades$VWAP)[(AGL_freq_trades$VWAP) >= qu]

cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_Freq_TailUpper.pdf", width = 5, height = 5)
func.freq_plot(sample_qupper) +
  labs(x = "\n VWAP", y = "Count \n", title = Stock1,
       subtitle = "1-minute VWAP Frequency - Upper Tail (95%)")
dev.off()

length(which(sample_qupper > 41000))/length(sample_qupper)
cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_Freq_TailUpper_lt41000.pdf", width = 5, height = 5)
func.freq_plot(sample_qupper[sample_qupper < 41000])   +
  labs(x = "\n VWAP", y = "Count \n", title = Stock1,
       subtitle = "1-minute VWAP Frequency - Upper Tail (95%) : Excl. Outlier > 41000 ")
dev.off()

## Tails >> MicroPrice

### Lower
qlower <- quantile((AGL_freq_quotes$MicroPrice_OHLCV), 0.05)
sample_qlower <- (AGL_freq_quotes$MicroPrice_OHLCV)[
  (AGL_freq_quotes$MicroPrice_OHLCV) <= qlower]

cairo_pdf("HFT_Ass2_fig_AGL1min_MicroPrice_Freq_LowerTail.pdf", width = 5, height = 5)
func.freq_plot(sample_qlower) +
  labs(x = "\n MicroPrice", y = "Count \n", title = Stock1,
       subtitle = "1-minute MicroPrice Frequency - Lower Tail (5%)")
dev.off()

### Upper
qupper <- quantile((AGL_freq_quotes$MicroPrice_OHLCV), 0.95)
sample_qupper <- (AGL_freq_quotes$MicroPrice_OHLCV)[
  (AGL_freq_quotes$MicroPrice_OHLCV) >= qupper]

cairo_pdf("HFT_Ass2_fig_AGL1min_MicroPrice_Freq_UpperTail.pdf", width = 5, height = 5)
func.freq_plot(sample_qupper)  +
  labs(x = "\n MicroPrice", y = "Count \n", title = Stock1,
       subtitle = "1-minute MicroPrice Frequency - Upper Tail (95%)")
dev.off()

## ACFs
func.acf_plot <- function(data)
{
  
  acfres <- acf(data, main = "", lag.max = length(data), plot = F)
  
  afcs <- tibble(ACF = as.numeric(acfres$acf),
                 Lag = as.numeric(acfres$lag))
  
  ggplot(afcs, aes(x = Lag, y = ACF)) +
    geom_line() +
    labs(y = "ACF \n", x = "\n Lag") +
    theme_bw() +
    theme(aspect.ratio = 0.75)
  
}

cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_ACF.pdf", width = 5, height = 5)
func.acf_plot(AGL_freq_trades$VWAP) +
  labs(title = Stock1,
       subtitle = "Autocorrelation of 1-minute VWAP on Absolute Scale")
dev.off()

cairo_pdf("HFT_Ass2_fig_AGL1min_VWAP_ACF_DiffLog.pdf", width = 5, height = 5)
func.acf_plot(diff(log(AGL_freq_trades$VWAP))) +
  labs(title = Stock1,
       subtitle = "Autocorrelation of Differenced 1-minute VWAP on Absolute Scale")
dev.off()

cairo_pdf("HFT_Ass2_fig_AGL1min_MicroPrice_ACF.pdf", width = 5, height = 5)
func.acf_plot(AGL_freq_quotes$MicroPrice_OHLCV) +
  labs(title = Stock1,
       subtitle = "Autocorrelation of 1-minute MicroPrice on Absolute Scale")
dev.off()

cairo_pdf("HFT_Ass2_fig_AGL1min_MicroPrice_ACF_DiffLog.pdf", width = 5, height = 5)
func.acf_plot(diff(AGL_freq_quotes$MicroPrice_OHLCV)) +
  labs(title = Stock1,
       subtitle = "Autocorrelation of Differenced 1-minute MicroPrice on Absolute Scale")
dev.off()

# Part II ----

# Price Impact ----

## Price Impact and seasonality
dat_AGL_all <- dat_TAQ$AGL
dat_NPN_all <- dat_TAQ$NPN
dev.off()

# 20 log normal bins
bins <- logspace(-3, 0.5, n = 21)

## for each bin we want the mean(delta price) and sum(volume)/TotalVolume

func.price_impact_curve <- function(data, stock, bins)
{
  
dat_Price_impact <- data %>%
  mutate(BuyerSellerInitiated = lag(data$Trade.Sign, 1),
         VolumeTraded = lag(data$Volume.Trade, 1)) %>%
  select(DateTimeL, Mid.Price.Change,
         BuyerSellerInitiated, VolumeTraded) %>%
  filter(BuyerSellerInitiated != "0") %>%
  mutate(BuyerSellerInitiated = as.factor(BuyerSellerInitiated),
         NormalisedVolume = VolumeTraded/mean(VolumeTraded, na.rm = TRUE))
  
dat_Price_impact$bins <- cut(dat_Price_impact$NormalisedVolume, bins) 
dat_Price_impact <- na.omit(dat_Price_impact)

dat_Price_impact <- dat_Price_impact %>%
  filter(BuyerSellerInitiated != "0") %>%
  mutate(BuyerSellerInitiated = as.factor(BuyerSellerInitiated)) %>%
  group_by(bins, BuyerSellerInitiated) %>%
  mutate(AvgMidPriceChange = mean(Mid.Price.Change),
         AvgNormalisedVolume = mean(NormalisedVolume)) %>%
  ungroup()

Price_impact_plot <- dat_Price_impact %>%
  group_by(bins, BuyerSellerInitiated) %>%
  slice(n())

ggplot(Price_impact_plot, aes(y = AvgMidPriceChange,
                              x = AvgNormalisedVolume,
                              col = BuyerSellerInitiated)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_color_manual(labels = c("Buyer Initiated", "Seller Initiated"), 
                     values = c("cornflowerblue", "darkorange2")) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "\n Average Normalised Volume",
       y = "Average Price Impact \n",
       title = stock, 
       subtitle = "Average Price Impact Curves",
       color = "")  +
  scale_y_continuous(label = math_format()) +
  annotation_logticks(sides  = "b") +
  theme(aspect.ratio = 0.75,
        legend.direction = 'horizontal',
        legend.position = "bottom")
}

setwd(dir_figs)
cairo_pdf("HFT_Ass2_fig_AGL_PriceImpact.pdf", height = 5, width = 5)
func.price_impact_curve(dat_AGL_all, Stock1, bins)
dev.off()

cairo_pdf("HFT_Ass2_fig_NPN_PriceImpact.pdf", height = 5, width = 5)
func.price_impact_curve(dat_NPN_all, Stock2, bins)
dev.off()

 # Order Book Sesonality ----

# Average the volumes traded across all the days in your data and 
# plot the aggregate volume as a function of time across the day 
# (intraday volume curves) as normalised by the daily volume.

func.normalise <- function(data)
{
  
  num <- data - min(data, na.rm = TRUE)
  denom <- max(data, na.rm = TRUE) - min(data, na.rm = TRUE)
  
  dat_norm <- num/denom
  return(dat_norm)
  
}

func.norm_trade_vol_plot <- function(data, stock)
{
  
  ## Do we want to get a daily average and take the timevol/daily average
  dat_NormTradeVol_by_DailyVol <- data %>%
    filter(Type == "Trade") %>% # trade data
    mutate(date = as.Date(DateTimeL),
           time = format(DateTimeL,"%H:%M")) %>% # seperate date and time
    group_by(date) %>%
    mutate(NormVol = func.normalise(Volume.Trade)) %>% # daily volume
    ungroup() %>%
    group_by(time) %>%
    mutate(AvgNormVol = mean(NormVol, na.rm = TRUE)) %>% # avg of vol at t/dayvol
    slice(n()) %>% # only need one row from each time
    ungroup() %>%
    mutate(time = as.POSIXct(strptime(time, format = "%H:%M"), na.rm = TRUE))
  
  # as.POSIXct(strptime(test1$time, format = "%H:%M:%S"))
  
  ggplot(dat_NormTradeVol_by_DailyVol, aes(x = time, y = AvgNormVol)) +
    geom_line() + 
    scale_x_datetime(date_labels = "%H:%M:%S",
                     timezone = "Africa/Johannesburg") +
    labs(x = "\n Time (HH:MM:SS)", 
         y = "Average Normalised Trading Volume per Unit Time \n", title = stock,
         subtitle = "Normalised by the Daily Volume") +
    theme_bw()
  
}

cairo_pdf("HFT_Ass2_fig_AGL_NormTradeVol.pdf", height = 5, width = 5)
func.norm_trade_vol_plot(dat_AGL_all, Stock1)
dev.off()

cairo_pdf("HFT_Ass2_fig_NPN_NormTradeVol.pdf", height = 5, width = 5)
func.norm_trade_vol_plot(dat_NPN_all, Stock2)
dev.off()

func.norm_abs_ret_plot <- function(data, stock)
{

  ## Do we want to get a daily average and take the timevol/daily average
  dat_NormIntradayRet_by_DailyAbsRet <- data %>%
    filter(Type == "Trade") %>% # trade data
    mutate(date = as.Date(DateTimeL),
           time = format(DateTimeL,"%H:%M")) %>% # seperate date and time
    mutate(AbsReturns = c(0, abs(diff(log(Price))))) %>% # log returns
    group_by(date) %>%
    mutate(NormRet = func.normalise(AbsReturns)) %>% # normalise
    ungroup() %>%
    group_by(time) %>%
    mutate(AvgNormRet = mean(NormRet, na.rm = TRUE)) %>% # avg
    slice(n()) %>% # only need one row from each time
    ungroup() %>%
    mutate(time = as.POSIXct(strptime(time, format = "%H:%M"), na.rm = TRUE))
  
  # as.POSIXct(strptime(test1$time, format = "%H:%M:%S"))
  
  ggplot(dat_NormIntradayRet_by_DailyAbsRet, aes(x = time, y = AvgNormRet)) +
    geom_line() + 
    scale_x_datetime(date_labels = "%H:%M:%S",
                     timezone = "Africa/Johannesburg") +
    labs(x = "\n Time (HH:MM:SS)", 
         y = "Average Normalised Intraday Return \n", title = stock,
         subtitle = "Normalised by the Average Daily Absolute Intraday Returns") +
    theme_bw()
  
}

cairo_pdf("HFT_Ass2_fig_AGL_NormAbsRet.pdf", height = 5, width = 5)
func.norm_abs_ret_plot(dat_AGL_all, Stock1)
dev.off()

cairo_pdf("HFT_Ass2_fig_NPN_NormAbsRet.pdf", height = 5, width = 5)
func.norm_abs_ret_plot(dat_NPN_all, Stock2)
dev.off()

func.spreads_plot <- function(data, stock)
{
  
  ## Do we want to get a daily average and take the timevol/daily average
  dat_spreads <- data %>%
    filter(Type == "Quote") %>% # trade data
    mutate(date = as.Date(DateTimeL),
           time = format(DateTimeL,"%H:%M")) %>% # seperate date and time
    group_by(date) %>%
    mutate(DailySpread = L1.Ask - L1.Bid,
           NormDailySpread = func.normalise(DailySpread)) %>% # Daily returns
    ungroup() %>%
    group_by(time) %>%
    mutate(AvgNormIntradaySpread = mean(NormDailySpread, na.rm = TRUE)) %>% # avg ret @ t
    slice(n()) %>% # only need one row from each time
    ungroup() %>%
    mutate(time = as.POSIXct(strptime(time, format = "%H:%M"), na.rm = TRUE))
  
  # as.POSIXct(strptime(test1$time, format = "%H:%M:%S"))
  
  ggplot(dat_spreads, aes(x = time, y = AvgNormIntradaySpread)) +
    geom_line() + 
    scale_x_datetime(date_labels = "%H:%M:%S",
                     timezone = "Africa/Johannesburg") +
    labs(x = "\n Time (HH:MM:SS)", 
         y = "Average Spread \n", title = stock,
         subtitle = "Normalised by Average Daily Spread") +
    theme_bw()
  
}

cairo_pdf("HFT_Ass2_fig_AGL_AvgSpreads.pdf", height = 5, width = 5)
func.spreads_plot(dat_AGL_all, Stock1)
dev.off()

cairo_pdf("HFT_Ass2_fig_NPN_AvgSpreads.pdf", height = 5, width = 5)
func.spreads_plot(dat_NPN_all, Stock2)
dev.off()









