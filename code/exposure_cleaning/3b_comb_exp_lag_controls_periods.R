## This script combines the exposure and control data into one df
## The output returned will be combined with medicare data to use in the model

dir_out <- "~/Documents/Harvard/NSAPH Wilfires/wildfires_code/data/"

# Load libraries
library(tidyverse)
library(lubridate)

# Load exposure data
load(paste0(dir_out,"exp_lag_periods.RData"))

# Load control data 
load(paste0(dir_out,"control_periods_2006_2018_thresh1.RData"))

# Subset full exposure data to be exposed data
county_wf_summarized_date_array <- exp_lag_df %>%
  group_by(GEOID) %>% 
  filter(event_lag0 == 1) 
county_wf_summarized_date_array$date <- county_wf_summarized_date_array$start_date

# Create WF IDs to match exposed days to their controls
county_wf_summarized_date_array$ID <- seq(1:nrow(county_wf_summarized_date_array))
control_dates_df$ID <- seq(1:nrow(control_dates_df))


# Subset full exposure data to exposed and lagged days
WF_lag_full <- exp_lag_df %>%
  group_by(GEOID) %>% 
  filter( event_lag1 == 1 | event_lag2 == 1 | event_lag3 == 1 | 
           event_lag4 == 1 | event_lag5 == 1 | event_lag6 == 1 | event_lag7 == 1) 

WF_exposed_full <- rbind(county_wf_summarized_date_array,WF_lag_full)

# Re-order columns
WF_exposed_full <- WF_exposed_full[,c("GEOID", "start_date", "end_date",  "date", "event_lag0",     
                                   "event_lag1", "event_lag2", "event_lag3", "event_lag4", "event_lag5",
                                   "event_lag6", "event_lag7") ]

# FInd control period and control lag days

# Set up lags
# These lag columns correspond to the exposure lag columns but will be all 0s
lag_num <- 8 #exposed day plus 7 lags
lags <- matrix(0, nrow = lag_num, ncol = lag_num)
colnames(lags) <- c("event_lag0", "event_lag1", "event_lag2", "event_lag3", 
                    "event_lag4", "event_lag5", "event_lag6", "event_lag7" )
# Control 1
control1 <- c()
control2 <- c()
WF_control <- c()

for(i in 1:nrow(county_wf_summarized_date_array)){
  GEOID <- control_dates_df$wfcounty_id[i]
  start_date <- county_wf_summarized_date_array$start_date[i]
  end_date <- county_wf_summarized_date_array$end_date[i]
  
  # Control 1
  control1_start <- control_dates_df$control1_start[i]
  control1_end <- control_dates_df$control1_end[i]
  
  if(is.na(as.Date(control_dates_df$control1_start[i]))){
    date <- rep(NA,lag_num)
  }else{
    date <- seq(as.Date(control_dates_df$control1_start[i]), as.Date(control_dates_df$control1_end[i]), by="days")
  }
  # Add t
  #t <- seq(1,lag_num)
  
  # Define control period for exposure period
  control1_period <- data.frame(cbind(GEOID, start_date = as.character(control1_start), end_date = as.character(control1_end), 
                                      date = as.character(date[1]), t(lags[1,]) ))
  
  # Define control days for lag days
  control1 <- data.frame(cbind(GEOID, start_date = as.character(control1_start), end_date = as.character(control1_end),
                               date = as.character(date[(length(date)-6):length(date)]), lags[2:8,] ))
  
  # Combine to have final control1
  control1 <- rbind(control1_period,control1)
  
  # Control 2
  control2_start <- control_dates_df$control2_start[i]
  control2_end <- control_dates_df$control2_end[i]
  
  if(is.na(as.Date(control_dates_df$control2_start[i]))){
    date <- rep(NA,lag_num)
  }else{
    date <- seq(as.Date(control_dates_df$control2_start[i]), as.Date(control_dates_df$control2_end[i]), by="days")
  }
  # Define control period for exposure period
  control2_period <- data.frame(cbind(GEOID, start_date = as.character(control2_start), end_date = as.character(control2_end), 
                                      date = as.character(date[1]), t(lags[1,]) ))
  
  # Define control days for lag days
  control2 <- data.frame(cbind(GEOID, start_date = as.character(control2_start), end_date = as.character(control2_end), 
                               date = as.character(date[(length(date)-6):length(date)]), lags[2:8,] ))
  
  # Combine to have final control1
  control2 <- rbind(control2_period,control2)
  
  # Combine control 1 and 2
  temp <- rbind(control1, control2)
  
  # Define wildifire id
  temp$wfGEOID_id <- i
  
  WF_control <- rbind(WF_control, temp)
  
}

# Convert 
WF_control$start_date <- as.Date(WF_control$start_date)
WF_control$end_date <- as.Date(WF_control$end_date)
WF_control$date <- as.Date(WF_control$date)
WF_control$event_lag0 <- as.double(WF_control$event_lag0)
WF_control$event_lag1 <- as.double(WF_control$event_lag1)
WF_control$event_lag2 <- as.double(WF_control$event_lag2)
WF_control$event_lag3 <- as.double(WF_control$event_lag3)
WF_control$event_lag4 <- as.double(WF_control$event_lag4)
WF_control$event_lag5 <- as.double(WF_control$event_lag5)
WF_control$event_lag6 <- as.double(WF_control$event_lag6)
WF_control$event_lag7 <- as.double(WF_control$event_lag7)


# Add wfGEOID_id to exposure data
# Initialize 
WF_exposed_full$wfGEOID_id <- NA
for(i in 1:nrow(county_wf_summarized_date_array)){
  
  # Filter by GEOID and start_date
  # county_wf_summarized_date_array has the info for each exposure period so we can 
  # cycle through each row to find each wf
  GEOID_temp <- county_wf_summarized_date_array$GEOID[i]
  start_date_temp <- county_wf_summarized_date_array$start_date[i]
  # Assign wfGEOID_id
  #exp_model_input %>% filter(GEOID == GEOID_temp, start_date == start_date_temp) %>% mutate(wfGEOID_id = i)
  WF_exposed_full$wfGEOID_id[WF_exposed_full$GEOID == GEOID_temp & WF_exposed_full$start_date == start_date_temp] <- i
  
}


# Combine with exposure data
exp_model_input <- rbind(WF_exposed_full,WF_control)

# Add indicators for control
exp_model_input$control_indicator <- 0
exp_model_input <- exp_model_input %>% 
  mutate( control_indicator = ifelse(event_lag0 == 0 & event_lag1 == 0 & event_lag2 == 0 & event_lag3 == 0 & 
            event_lag4 == 0 & event_lag5 == 0 & event_lag6 == 0 & event_lag7 == 0, 1,0) )


# Remove rows with date = NA
# This are rows where a control was not found
exp_model_input <- exp_model_input %>% drop_na(date,)# Remove rows with date = NA



save(exp_model_input, file = paste0(dir_out,"exp_model_input_periods.RData"))



