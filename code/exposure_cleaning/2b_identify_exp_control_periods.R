## This script takes county_wf_edit_array and creates exposure periods
## Previously, if an exposure day is also a lag day, they are in separate rows
## Here, if a WF period ends if there is 7 lag days without a new WF

dir_out <- "~/Documents/Harvard/NSAPH Wilfires/wildfires_code/data/"

# Load libraries
library(tidyverse)
library(lubridate)

# Load data

load("~/Documents/Harvard/NSAPH Wilfires/county_wf_lag_array_2006_2018.RData")
exp_model_input_clean <- county_wf_edit_array

start_year <- 2006
end_year <- 2018

# I need to filter to be only exposed days
# I will need to create new lagged days based on the end of the exposure period

# Filter data to be exposed days
exp_days <- exp_model_input_clean %>% filter(event_lag0 == 1)

# Get unique county days
county_days <- exp_days %>%
  distinct(GEOID,date)

# Add a column for exposure period start date
county_days$start_date <- as.Date(NA)

# Add a column for exposure period end date
county_days$end_date <- as.Date(NA)

# Add a column for WF indicator (this will help identify exposure end date)
county_days$WF_ind <- NA

# Initialize final data
exp_periods <- c()

# For a county
for(i in 1:length(unique(county_days$GEOID))){
  
  # Get current GEOID
  GEOID_temp <- unique(county_days$GEOID)[i]
  
  # Filter on current county
  exp_days_temp <- county_days %>% filter(GEOID == GEOID_temp)
  
  # Order based on date (earliest to latest date)
  exp_days_temp <- exp_days_temp %>% arrange(ymd(exp_days_temp$date))
  
  # Set exposure start date as the first date
  start_date <- exp_days_temp$date[1]
  exp_days_temp$start_date[1] <- as.Date(start_date)
  WF_ind <- 1
  exp_days_temp$WF_ind[1] <- WF_ind
  
  # Initialize vector of end date
  end_date <- as.Date(c())
  
  # If WF is only one day
  if(nrow(exp_days_temp) == 1){
    
    exp_days_temp$end_date[1] <- start_date
    
  # If there is a WF exposure period 
  }else{
    
    
    for(j in 2:nrow(exp_days_temp)){
      # If the exposure date is within 7 days of the previous date, 
      # they are within the same WF exposure period
      if(exp_days_temp$date[j] < as.Date(exp_days_temp$date[j - 1] + 7)){
        exp_days_temp$start_date[j] <- as.Date(start_date)
        exp_days_temp$WF_ind[j] <- WF_ind
        # If the exposure date is not within 7 days of the previous date, 
        # we start a new exposure period
      } else {
        start_date <- exp_days_temp$date[j]
        exp_days_temp$start_date[j] <- as.Date(start_date)
        
        # Add one to indicate new wildfire
        WF_ind <- WF_ind + 1
        exp_days_temp$WF_ind[j] <- WF_ind
        
        # Make vector of end dates
        end_date <- c(end_date,exp_days_temp$date[j-1])
        
        # Set end date for the previous dates as the last date
        #end_date <- exp_days_temp$date[j-1]
        #exp_days_temp$end_date[j-1]
        
      }
      
    }
    
    # Add last end date to vector
    end_date <- c(end_date,exp_days_temp$date[nrow(exp_days_temp)])
    
    # Fill in end dates
    exp_days_temp$end_date <- end_date[exp_days_temp$WF_ind]
    
  }
  
  # Combine results from each county
  exp_periods <- rbind(exp_periods, exp_days_temp)
  
}

# Lag days start after exposure periods
# Equation changes to beta_1*exp_period + beta_2*lag1 + .... 

# Find unique exposure county-periods
exp_periods <- exp_periods %>% distinct(GEOID, start_date, end_date)

# Initialize columns
exp_periods$date <- as.Date(NA)
exp_periods$event_lag0 <- 1
exp_periods$event_lag1 <- 0
exp_periods$event_lag2 <- 0
exp_periods$event_lag3 <- 0
exp_periods$event_lag4 <- 0
exp_periods$event_lag5 <- 0
exp_periods$event_lag6 <- 0
exp_periods$event_lag7 <- 0

# Add rows for lagged days
exp_lag_df <- c()
for(i in 1:nrow(exp_periods)){
  
  # Add exposure period to exp_lag_df
  exp_lag_df <- rbind(exp_lag_df, exp_periods[i,])
  
  for(j in 1:7){
    # Initialize
    new_row <- c()
    new_row$GEOID <- exp_periods$GEOID[i]
    new_row$start_date <- exp_periods$start_date[i]
    new_row$end_date <- exp_periods$end_date[i]
    new_row$date <- as.Date(NA)
    new_row$event_lag0 <- 0
    new_row$event_lag1 <- 0
    new_row$event_lag2 <- 0
    new_row$event_lag3 <- 0
    new_row$event_lag4 <- 0
    new_row$event_lag5 <- 0
    new_row$event_lag6 <- 0
    new_row$event_lag7 <- 0
    
    # Fill lagged values
    new_row$date <- as.Date(exp_periods$end_date[i] + j)
    new_row[(5+j)] <- 1
    
    # Combine rows
    exp_lag_df <- rbind(exp_lag_df, new_row)
    
    
  }
  
}

# Save data
save(exp_lag_df, file = paste0(dir_out, "exp_lag_periods.RData"))

# Next step: control periods

# How many exposure periods? 5600
exp_lag_df %>% count(is.na(date))

# The goal is to find control periods the years before and after exposure
# by finding control periods entirely below a threshold plus lag below the threshold

# Define control threshold
# A day can only be a control day if the WF PM is below this threshold
thresh <- 1

# Define min_date as the first day of exposure
county_wf_summarized_date_array <- exp_lag_df %>%
  group_by(GEOID) %>% 
  filter(event_lag0 == 1) # filter by exposure period
# Take start and end date columns
county_wf_summarized_date_array <- county_wf_summarized_date_array[,1:3]

# Define min_date as the exposure start date

county_wf_summarized_date_array$min_date <- county_wf_summarized_date_array$start_date

# Define max_date as last lagged day 
county_wf_summarized_date_array$max_date <- county_wf_summarized_date_array$end_date + 7



control_dates_df <- c()
all_fips <- unique(county_wf_summarized_date_array$GEOID)

for (j in all_fips){
  same_county_wf <- county_wf_summarized_date_array %>% 
    filter(GEOID == j)
  for (k in 1:nrow(same_county_wf)){
    # Specify id as current FIPS
    id_temp <- j #same_county_wf$GEOID[k]
    
    #control1_dates <- find_control1(same_county_wf,county_wf_edit_array, start_year, end_year)
    #control2_dates <- find_control2(same_county_wf,county_wf_edit_array, start_year, end_year)
    
    ## FIND CONTROL1
    i <- 1 # initialize i 
    
    # Initialize minumum and max days for the two controls
    min_date_temp <- control1_min_date <- same_county_wf$min_date[k]
    max_date_temp <- control1_max_date <- same_county_wf$max_date[k]
    
    # Specify id as current FIPS
    id_temp <- j #same_county_wf$GEOID[k]
    
    
    # Specify control1 to be the year before exposed year
    year(control1_min_date) <- year(min_date_temp) - i
    year(control1_max_date) <- year(max_date_temp) - i
    
    
    if( year(control1_min_date) >= start_year){ #checking if the year is within our bounds
      # Check if the control days have a PM2.5 level < threshold
      control1_week <- county_wf_edit_array %>% 
        filter(GEOID == j & (date >= control1_min_date & date <= control1_max_date))
      
      # while any of the estimates are greater than our threshold
      while(any(control1_week$smokePM_pred >= thresh)){ 
        # check if we have reached end year
        if (year(control1_min_date) == start_year){
          control1_dates <- c(NA,NA)
          print("start year")
          break
        }
        
        i <- i + 1 
        # Specify control1 to be the next year before 
        year(control1_min_date) <- year(min_date_temp) - i
        year(control1_max_date) <- year(max_date_temp) - i
        control1_week <- county_wf_edit_array %>% 
          filter(GEOID == j & (date >= control1_min_date & date <= control1_max_date))
        
      }
      control1_dates <- c(control1_min_date,control1_max_date)
    }else{
      control1_dates <- c(NA,NA)
    }
    
    ## FIND CONTROL2
    i <- 1 # initialize i 
    
    # Initialize minumum and max days for the two controls
    min_date_temp <- control2_min_date <- same_county_wf$min_date[k]
    max_date_temp <- control2_max_date <- same_county_wf$max_date[k]
    
    # Specify id as current FIPS
    id_temp <- j #same_county_wf$GEOID[k]
    
    
    # Specify control2 to be the year after exposed year
    year(control2_min_date) <- year(min_date_temp) + i
    year(control2_max_date) <- year(max_date_temp) + i
    
    
    if( year(control2_min_date) <= end_year){ #checking if the year is within our bounds
      # Check if the control days have a PM2.5 level < threshold
      control2_week <- county_wf_edit_array %>% 
        filter(GEOID == j & (date >= control2_min_date & date <= control2_max_date))
      
      # while any of the estimates are greater than our threshhoold
      while(any(control2_week$smokePM_pred >= thresh)){ 
        # check if we have reached end year
        if (year(control2_min_date) == end_year){
          control2_dates <- c(NA,NA)
          print("end year")
          break
        }
        
        i <- i + 1 
        # Specify control2 to be the next year after 
        year(control2_min_date) <- year(min_date_temp) + i
        year(control2_max_date) <- year(max_date_temp) + i
        control2_week <- county_wf_edit_array %>% 
          filter(GEOID == j & (date >= control2_min_date & date <= control2_max_date))
        
      }
      control2_dates <- c(control2_min_date,control2_max_date)
    }else{
      control2_dates <- c(NA,NA)
    }
    
    final_row <- c(id_temp, control1_dates, control2_dates)
    control_dates_df <- rbind(control_dates_df, final_row)
  }
  
}


# Clean data frame
control_dates_df <- as.data.frame(control_dates_df)
control_dates_df$V2 <- as.Date(as.numeric(control_dates_df$V2), origin = "1970-01-01")
control_dates_df$V3 <- as.Date(as.numeric(control_dates_df$V3), origin = "1970-01-01")
control_dates_df$V4 <- as.Date(as.numeric(control_dates_df$V4), origin = "1970-01-01")
control_dates_df$V5 <- as.Date(as.numeric(control_dates_df$V5), origin = "1970-01-01")
colnames(control_dates_df) <- c("wfcounty_id", "control1_start", "control1_end", "control2_start", "control2_end")
control_dates_df <- control_dates_df[order(control_dates_df$wfcounty_id),]
rownames(control_dates_df) <- c(1:nrow(control_dates_df))

# Controls are identified in periods essentially. 
# The PM level must be below the threshold for the entire period of exposure plus lag
# Save control bounds
save(control_dates_df, file = paste0(dir_out,"control_periods_2006_2018_thresh",thresh,".RData"))




