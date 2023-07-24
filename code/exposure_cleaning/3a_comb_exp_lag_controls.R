## This script combines the exposure and control data into one df
## The output returned will be combined with medicare data to use in the model

dir_out <- "~/Documents/Harvard/NSAPH Wilfires/wildfires_code/data/"

# Load libraries
library(tidyverse)
library(lubridate)

# Load exposure data
load(paste0(dir_out,"county_wf_lag_array_2006_2018.RData"))

# Load control data 
load(paste0(dir_out,"control_dates_2006_2018_thresh1.RData"))

# Subset full exposure data to be exposed data
county_wf_summarized_date_array <- county_wf_edit_array %>%
  group_by(GEOID) %>% 
  filter(event_lag0 == 1) 

# Create WF IDs to match exposed days to their controls
county_wf_summarized_date_array$ID <- seq(1:nrow(county_wf_summarized_date_array))
control_dates_df$ID <- seq(1:nrow(control_dates_df))


# Subset full exposure data to exposed and lagged days
WF_exposed_full <- county_wf_edit_array %>%
  group_by(GEOID) %>% 
  filter(event_lag0 == 1 | event_lag1 == 1 | event_lag2 == 1 | event_lag3 == 1 | 
           event_lag4 == 1 | event_lag5 == 1 | event_lag6 == 1 | event_lag7 == 1) 

# Define t as the time period
# t = 1 for exposed day, t = 2 for lag day 1,...
# This will help match exposures to their controls

# Set up lags
lag_num <- 8 #exposed day plus 7 lags
lags <- matrix(0, nrow = lag_num, ncol = lag_num)
for(i in 1:lag_num){
  lags[i,i] <- 1
}
colnames(lags) <- c("event_lag0", "event_lag1", "event_lag2", "event_lag3", 
                    "event_lag4", "event_lag5", "event_lag6", "event_lag7" )

# Expand the dataset
# This is done for every exposed county-day
# This results in repeated county-days if a county-day is both an exposed and a lagged day
WF_exposed <- c()
for(i in 1:length(county_wf_summarized_date_array$date)){
  GEOID <- as.character(county_wf_summarized_date_array$GEOID[i])
  WF_StartDate <- county_wf_summarized_date_array$date[i]
  date <- seq(as.Date(WF_StartDate ), WF_StartDate + 7 , by="days")
  # Add t
  t <- seq(1,lag_num)
  temp <- data.frame(cbind(GEOID, WF_StartDate = as.character(WF_StartDate), date = as.character(date)), t, lags)
  
  WF_exposed <- rbind(WF_exposed, temp)
  
}


# Expand controls to start at start date and end at end date
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

for(i in 1:length(county_wf_summarized_date_array$date)){
  GEOID <- control_dates_df$wfcounty_id[i]
  WF_StartDate <- county_wf_summarized_date_array$date[i]
  
  # Control 1
  if(is.na(as.Date(control_dates_df$control1_start[i]))){
    date <- rep(NA,lag_num)
  }else{
    date <- seq(as.Date(control_dates_df$control1_start[i]), as.Date(control_dates_df$control1_end[i]), by="days")
  }
  # Add t
  t <- seq(1,lag_num)
  control1 <- data.frame(cbind(GEOID, WF_StartDate = as.character(WF_StartDate), date = as.character(date)), t, lags)
  
  # Control 2
  if(is.na(as.Date(control_dates_df$control2_start[i]))){
    date <- rep(NA,lag_num)
  }else{
    date <- seq(as.Date(control_dates_df$control2_start[i]), as.Date(control_dates_df$control2_end[i]), by="days")
  }
  # Add t
  t <- seq(1,lag_num)
  control2 <- data.frame(cbind(GEOID, WF_StartDate = as.character(WF_StartDate), date = as.character(date)), t, lags)
  
  # Combine control 1 and 2
  temp <- rbind(control1, control2)
  WF_control <- rbind(WF_control, temp)
  
}


# Combine with exposure data
exp_model_input <- rbind(WF_exposed,WF_control)

save(exp_model_input, file = paste0(dir_out,"exp_model_input.RData"))

# Edit exp_model_input.RData
# Since event_lag0 will be 1 for multiple days in a row we combine dates 
# to create one exposure over multiple dates
# This will follow for all lagged days and control days

#--------------

# Load data
load(paste0(dir_out,"exp_model_input.RData"))

# Remove rows with date = NA
# These are rows where a control was not found
exp_model_input <- exp_model_input %>% drop_na(date,)


# Focus on exposure days and the first unique GEOID
# The goal is to have a column with a group exposure period, (maybe lower and upper day bound?)
# a column of the last exposure day
# this will help create the lagged days since lagged days start after the last exposure day 

#i <- 3

GEOID_df <- c()
WF_df <- c()
for(i in 1:length(unique(exp_model_input$GEOID))){
  # Filter exposure days
  exp_days <- exp_model_input %>%
    filter(event_lag0 == 1) %>%
    filter(GEOID == unique(GEOID)[i])
  
  # Initialize vectors for columns
  exposure_start <- c()
  exposure_end <- c()
  
  # Create groups based on which WF they are in 
  exp_days$group <- 1 + c(0, cumsum(ifelse(diff(as.Date(exp_days$date)) > 1, 1, 0)))
  
  # Find exposure start and end for each WF group
  for(j in unique(exp_days$group)){
    # Filter for each group
    temp <-exp_days %>% filter(group == j)
    
    # how are these assigned
    exposure_start[j] <- temp$date[1]
    exposure_end[j] <- temp$date[nrow(temp)]
  }
  # combine column results for this GEOID
  GEOID_df <- cbind(GEOID = unique(exp_days$GEOID), exposure_start, exposure_end)
  
  # combine with other GEOIDs 
  WF_df <- rbind(WF_df,GEOID_df)
}

WF_df <- data.frame(WF_df)
colnames(WF_df) <- c("GEOID","exposure_start", "exposure_end")
#save(WF_df, file = "WF_combined_no_lag.RData")

# Expand data so that there is a row for each exposure day

WF_expand <- c()

for(i in 1:nrow(WF_df)){
  # Expand exposure days
  exp_days <- seq(as.Date(WF_df$exposure_start[i]), as.Date(WF_df$exposure_end[i]), by="days")
  temp_row <- c()
  temp_mat <- c()
  for(j in 1:length(exp_days)){
    temp_row <- cbind(WF_df[i,], date = exp_days[j])
    temp_mat <- rbind(temp_mat,temp_row)
  }
  WF_expand <- rbind(WF_expand,temp_mat)
}


# Remove rows with date = NA
# This are rows where a control was not found
exp_model_input <- exp_model_input %>% drop_na(date,)

# Add columns for year, month, and day of exposure
exp_model_input <- exp_model_input %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))

temp_comb <- c()
exp_model_input_clean <- c()
temp_comb_exp_join <- c()
final_temp_comb <- c()
final_comb <- c()

for(i in 1:length(unique(WF_expand$GEOID))){
  GEOID_temp <- unique(WF_expand$GEOID)[i]
  #print(GEOID_temp)
  
  # Find dates of last exposure days
  temp_exp_end <- WF_expand %>% filter(GEOID == GEOID_temp) %>% # filter on current GEOID 
    distinct(exposure_end, .keep_all = FALSE)
  
  # Find lag days and last exposed day 
  temp_last_lag <- exp_model_input %>% filter(GEOID == GEOID_temp) %>%  # filter on current GEOID 
    filter(WF_StartDate %in% c(temp_exp_end)$exposure_end) %>% # find lag and control days
    filter(lubridate::year(WF_StartDate) == year) # find last exposure day and lag days
  
  
  # Find exposure days before last exposed day
  temp_exp <- exp_model_input %>% filter(GEOID == GEOID_temp) %>% # filter on current GEOID 
    filter(!WF_StartDate %in% c(temp_exp_end)$exposure_end) %>% # finds exposure and lag days
    filter(event_lag0 == 1) # find exposed days
  
  # Combine and add columns for exposure
  
  # Combine
  temp_comb <- rbind(temp_exp,temp_last_lag)
  
  # Initialize indicator columns for exposure
  temp_comb$event_exp1 <- 0
  temp_comb$event_exp2 <- 0
  temp_comb$event_exp3plus <- 0
  
  # Add values of 1 for correct exposure days
  temp_comb_exp <- temp_comb %>% filter(event_lag0 == 1) 
  temp_comb_exp$date <- as.Date(temp_comb_exp$date)
  
  temp_comb_exp_join <- left_join(temp_comb_exp, WF_expand %>% filter(GEOID == GEOID_temp))
  
  for(j in 1:nrow(temp_comb_exp)){
    # add indicator for first exposure day
    if(temp_comb_exp$date[j] == temp_comb_exp_join$exposure_start[j]){
      temp_comb_exp$event_exp1[j] <- 1
    } 
    # add indicator for second exposure day
    if(temp_comb_exp$date[j] == as.Date(temp_comb_exp_join$exposure_start[j]) + 1){
      temp_comb_exp$event_exp2[j] <- 1
    } 
    # add indicator for third plus exposure day
    if(temp_comb_exp$date[j] > as.Date(temp_comb_exp_join$exposure_start[j]) + 1){
      temp_comb_exp$event_exp3plus[j] <- 1
    } 
    
  }
  
  # Add back lagged days
  final_temp_comb <- rbind(temp_comb_exp,temp_comb %>% filter(event_lag0 != 1) )
  
  # Find control days based on dates in final_temp_comb
  final_control <- exp_model_input %>% filter(GEOID == GEOID_temp) %>%
    filter((as.Date(WF_StartDate) %in% c(final_temp_comb$date)) & (year != lubridate::year(WF_StartDate))) 
  
  # Initialize indicator columns for exposure
  final_control$event_exp1 <- 0
  final_control$event_exp2 <- 0
  final_control$event_exp3plus <- 0
  
  # Combine exposure, lag, and control days
  final_comb <- rbind(final_temp_comb,final_control)
  
  # Add to final clean data
  exp_model_input_clean <- rbind(exp_model_input_clean,final_comb)
  
  
}

save(exp_model_input_clean, file = "exp_model_input_clean_expand.RData")









