## This script takes exp_model_input_clean_expand and collapses the rows
## Currently, if an exposure day is also a lag day, they are in separate rows
## Here, we combine these rows using appropriate indicators

dir_out <- "~/Documents/Harvard/NSAPH Wilfires/wildfires_code/data/"

# Load libraries
library(tidyverse)

# Load data
load(paste0(dir_out,"exp_model_input_clean_expand.RData"))

# Filter data to be exposed and lagged days
exp_model_input_clean <- exp_model_input_clean %>% 
  mutate(sumrow = rowSums(exp_model_input_clean[,c(6:12,16:18)]))

exp_lagged <- exp_model_input_clean %>% filter(sumrow != 0)

# Get unique county days
county_days <- exp_lagged %>%
  distinct(GEOID,date) 


exp_collapse <- data.frame()

for(i in 1:nrow(county_days)){
  # Subset data based on unique county-day
  temp <- exp_lagged %>% filter(GEOID == county_days$GEOID[i] & date == county_days$date[i])
  
  # If an exposure day is not a lag day, add to the final data set
  if(nrow(temp) == 1){
    exp_collapse <- rbind(exp_collapse, temp)
    # If an exposure day is also a lag day, merge rows
  }else{
    collapsed_row <- temp[1,]
    # Place collapsed values in the lagged and exposed day indicator columns
    collapsed_row[1,c(5:12,16:18)] <- colSums(rbind(temp[1,c(5:12,16:18)], temp[2,c(5:12,16:18)])) 
    # Add row to final data set
    exp_collapse <- rbind(exp_collapse, collapsed_row)
    
  }
  
}

# What about control days?
# i should only have one control day for each row

# Find control days based on dates in final_temp_comb
GEOID_temp <- unique(exp_collapse$GEOID)
final_control <- exp_model_input_clean  %>%
  filter((as.Date(WF_StartDate) %in% c(exp_collapse$date)) & (year != lubridate::year(WF_StartDate))) 

# Initialize indicator columns for exposure
final_control$event_exp1 <- 0
final_control$event_exp2 <- 0
final_control$event_exp3plus <- 0

# Combine exposure, lag, and control days
final_comb <- rbind(exp_collapse,final_control)

# Save collapsed data
save(final_comb, file = "exp_model_input_collapsed.RData")
