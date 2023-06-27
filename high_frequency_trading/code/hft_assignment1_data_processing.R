##
#
# Author: Julian Albert
# Date: 01 August 2019
#
# Description:
# HFT Assignment 1 - Data Load and Clean
#
#------------------------------------------------------------------------------#

# 0. Clean Workspace, Directory and Library ----

## Clean Workspace
rm(list=ls())
dev.off() # Close Plots
setwd("~") # Clear Path to User

## Locations
project_folder <- "/Documents/UCT/Coursework/HFT"
loc_script <- "/Assignment_1/UCT_Assignment/Code"
loc_data <- "/Data"

## Directories
dir_script <- paste("~", project_folder, loc_script, sep = '')
dir_data <- paste("~", project_folder, loc_data, sep = '')

## Set Working Directory to Script Location
setwd(dir_script)

## Libraries - Lazyload
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse, openxlsx, lubridate, zoo, data.table)

## Options
options(digits.secs = 3)

# 1. Load in data ----

## Variables
securities <- c("AGL", "NPN")
n.securities <- length(securities)
n.months <- 5
timezone <- "Africa/Johannesburg"
dat_TAQ_final <- list()

# 2. Cleaning Everything ----

## Start Time
ptm <- proc.time()

## For eeach security
for(security in 1:length(securities))
{
  
  # Quotes are in multiple .csv by month >> read into list
  dat.quotes_CLEAN <- list()
  
  # Per month into one list
  for(month in 1:n.months){

    ## Filenames for Bid and Ask
    filen_ask <- paste(dir_data, "/ask_",
                       substr(securities[security], 1, 3),"00", month, ".csv",
                       sep = "")
    
    filen_bid <- paste(dir_data, "/bid_",
                       substr(securities[security], 1, 3),"00", month, ".csv",
                       sep = "")
    
    ## Read Data for Bid and Ask
    tmp.dat_ask <- fread(filen_ask, drop = "condcode")
    tmp.dat_bid <- fread(filen_bid, drop = "condcode") 

    ## Join the Data and put into Desired Format >> X = Ask, Y = Bid
    tmp.dat_quotes <- full_join(tmp.dat_ask, tmp.dat_bid, by = "times") %>%
      rename(DateTimeL = times, 
             L1.Ask = value.x, Volume.Ask = size.x,
             L1.Bid = value.y, Volume.Bid = size.y) %>%
      arrange(DateTimeL) %>%
      mutate(Type = "Quote", type.x = NULL, type.y = NULL) %>%
      setDT()
    
    ## Clean the quotes >> Only take Last Quote >> Do now to reduce size ASAP
    dat.quotes_CLEAN[[month]] <- tmp.dat_quotes[, .SD[.N], DateTimeL]
  
  }

  ## Remove the Auction >> Set DateTime Format >> Fill NA Asks/Bids
  df_Quotes <- dat.quotes_CLEAN %>%
    bind_rows() %>%
    .[,  DateTimeL := parse_date_time(DateTimeL, "Ymd HMS", tz = timezone)] %>%
    setDF() %>%
    mutate(hms = format(as.POSIXct(DateTimeL), "%H:%M:%S")) %>%
    .[.$hms >= "09:10:00" & .$hms <= "16:49:59", ] %>%
    mutate(hms = NULL, Price = NA, Volume.Trade = NA) %>%
    fill(L1.Ask, Volume.Ask, L1.Bid, Volume.Bid) %>%
    as_tibble()
  
  ## Read in the trades (only one .csv with all trades)
  filen_trades <- paste(dir_data, "/trade_",
                        securities[security], ".csv",
                        sep = "")
  
  ## Remove the Auction
  tmp.dat_trades <- fread(filen_trades, drop = c("condcode", "type")) %>%
    rename(DateTimeL = times, Price.Trade = value, Volume.Trade = size) %>%
    .[,  DateTimeL := parse_date_time(DateTimeL, "Ymd HMS", tz = timezone)] %>%
    mutate(Type = "Trade", hms = format(as.POSIXct(DateTimeL), "%H:%M:%S")) %>%
    .[.$hms >= "09:10:00" & .$hms <= "16:49:59", ] %>%
    mutate(hms = NULL) %>%
    setDT()
  
  ## CLEAN TRADES
  df_Trades <- tibble(DateTimeL = tmp.dat_trades$DateTimeL,
                      L1.Ask = NA, Volume.Ask = NA,
                      L1.Bid = NA, Volume.Bid = NA,
                      Type = tmp.dat_trades$Type,
                      Price = tmp.dat_trades$Price,
                      Volume.Trade = tmp.dat_trades$Volume.Trade)
  
  ## TAQ data is Trades and Quotes
  df_TAQ <- bind_rows(df_Quotes, df_Trades) %>% 
    arrange(DateTimeL) %>%
    setDT() %>%
    .[, MidPrice := 0.5*(L1.Ask + L1.Bid)] %>%
    fill(MidPrice) %>%
    mutate(QuoteRule = ifelse(Price < MidPrice, -1, 
                              ifelse(Price > MidPrice, 1, 0)),
           tmpdiff = c(NA, diff(Price)),
           TickRule = ifelse(tmpdiff > 0, 1, 
                             ifelse(tmpdiff < 0, -1, 0)),
           Trade.Sign = ifelse(!is.na(QuoteRule) & QuoteRule != 0, QuoteRule, 
                               ifelse(QuoteRule == 0, TickRule, NA)))
  
  df_TAQ_CLEAN <- df_TAQ %>%
    mutate(QuoteRule = NULL, tmpdiff = NULL,  TickRule = NULL)
  
  ## Calculate measures on the Trades
  dat_transactions <- df_TAQ_CLEAN %>% filter(Type == "Trade") %>% setDT()
  
  dat_transactions <- dat_transactions %>%
    .[, keyby = .(DateTimeL, Trade.Sign),
      c("VWAP", "TotVolume") := 
        .(weighted.mean(Price, Volume.Trade), 
          sum(Volume.Trade))] %>% # Calc VWAP and sum(Volume)/Trade Type/day
    .[, keyby = .(DateTimeL, Trade.Sign), .SD[.N]] %>%
    .[, by = floor_date(DateTimeL, "day"),
      c("DailyPropVol", "No.DailyTrades", "day", "Inter.Arrival.times") := 
        .(TotVolume/sum(TotVolume), .N, 
          floor_date(DateTimeL, "day"),
          c(NA, diff(DateTimeL)))] %>% # Calc. Total Volume on Day, no Trades, t
    .[, c("Price", "Volume.Trade", "VWAP", "TotVolume",
          "TotalTradingDays") := 
        .(VWAP, TotVolume, NULL, NULL,uniqueN(day))] %>% # Clean and get N
    .[, NormalisedVolume := 
        DailyPropVol*(No.DailyTrades/TotalTradingDays)] %>% # Normalised Volume
    .[, c("day", "MidPrice", "Mid.Price.Change", "MicroPrice") :=
        .(NULL, NA, NA, NA)] # Clean 
  
  ## Calculate measures on the Quotes
  dat_quotes <- df_TAQ_CLEAN %>% filter(Type == "Quote")%>% setDT()
  
  dat_quotes <- dat_quotes %>%
    .[, .SD[.N], by = DateTimeL] %>%
    .[, c("DailyPropVol", "No.DailyTrades", "TotalTradingDays",
          "NormalisedVolume", "Inter.Arrival.times", "MidPrice",
          "Mid.Price.Change", "MicroPrice") := 
        .(NA, NA, NA, NA, NA, 0.5*(L1.Bid + L1.Ask), c(NA, diff(log(MidPrice))),
          (Volume.Ask/(Volume.Ask+Volume.Bid))*L1.Ask +
            (Volume.Bid/(Volume.Ask+Volume.Bid))*L1.Bid)]
  
  ## Reorder into a Nice Format
  tmp_order_names <- c("DateTimeL", "Type", "L1.Ask", "L1.Bid", "MidPrice", 
                       "Price","Trade.Sign", "Volume.Ask", "Volume.Bid", 
                       "Volume.Trade", "NormalisedVolume", "MicroPrice", 
                       "No.DailyTrades", "TotalTradingDays", "DailyPropVol", 
                       "Inter.Arrival.times","Mid.Price.Change")
  
  setcolorder(dat_transactions, tmp_order_names)
  setcolorder(dat_quotes, tmp_order_names)

  ## Some Reason dates are mimatched, start from min(date) end at min(lastdate)
  first_date <- min(first(dat_quotes$DateTimeL),
                    first(dat_transactions$DateTimeL))
  
  last_date <- min(last(dat_quotes$DateTimeL), 
                   last(dat_transactions$DateTimeL))
  
## Clean and Compact Data >> Store Per Ticker
dat_TAQ_final[[security]] <- bind_rows(dat_transactions, dat_quotes) %>%
  arrange(DateTimeL) %>%
  setDT() %>% # data with everything needed
  .[DateTimeL %between% c(first_date, last_date)] %>%
  as_tibble()

}

names(dat_TAQ_final) <- securities
proc.time() - ptm


# 3. Save Cleaned, Classified, Compacted Dataset ----
saveRDS(dat_TAQ_final, file = "dat_TAQ.rds")

#------------------------------------------------------------------------------#
