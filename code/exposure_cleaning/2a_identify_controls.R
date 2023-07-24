## Script to identify controls

# Load data 
library('lubridate')
library('tidyverse')

dir_out <- "~/Documents/Harvard/NSAPH Wilfires/wildfires_code/data/"

load("~/Documents/Harvard/NSAPH Wilfires/county_wf_lag_array_2006_2018.RData")

start_year <- 2006
end_year <- 2018

# Define control threshold
# A day can only be a control day if the WF PM is below this threshold
thresh <- 1

# Define min_date as the first day of exposure
county_wf_summarized_date_array <- county_wf_edit_array %>%
  group_by(GEOID) %>% 
  filter(event_lag0 == 1) 
county_wf_summarized_date_array <- county_wf_summarized_date_array[,1:2]
colnames(county_wf_summarized_date_array)[2] <- "min_date"
# Define max_date as last lagged day
county_wf_summarized_date_array$max_date <- county_wf_summarized_date_array$min_date + 7



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
save(control_dates_df, file = paste0(dir_out,"control_dates_2006_2018_thresh",thresh,".RData"))



