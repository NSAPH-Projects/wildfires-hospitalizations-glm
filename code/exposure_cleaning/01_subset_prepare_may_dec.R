## This script subsets the echolab exposure data to be the Western US ##
## and wildfire season May 1 - December 31 forr 2006-2016 ##
## This data is used in a sensitivity analysis ##

# Load Libraries
library(usmap)
library(tigris)
library(tidyverse)
library(lubridate)

# Import Data
data <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Harvard/NSAPH Wilfires/echolab_exposuredata/county/smokePM2pt5_predictions_daily_county_20060101-20201231.csv")

# Load full set of dates
# We are only interested in May 1 - October 31 for each year (wildfire season)
# Years for our study are 2006-2016 
dates = c(seq.Date(ymd("20060501"), ymd("20061231"), by = "day"),
          seq.Date(ymd("20070501"), ymd("20071231"), by = "day"),
          seq.Date(ymd("20080501"), ymd("20081231"), by = "day"),
          seq.Date(ymd("20090501"), ymd("20091231"), by = "day"),
          seq.Date(ymd("20100501"), ymd("20101231"), by = "day"),
          seq.Date(ymd("20110501"), ymd("20111231"), by = "day"),
          seq.Date(ymd("20120501"), ymd("20121231"), by = "day"),
          seq.Date(ymd("20130501"), ymd("20131231"), by = "day"),
          seq.Date(ymd("20140501"), ymd("20141231"), by = "day"),
          seq.Date(ymd("20150501"), ymd("20151231"), by = "day"),
          seq.Date(ymd("20160501"), ymd("20161231"), by = "day"))
         

# Filter to be Counties in the Western US
states <- c('Arizona', 'California', 'Colorado', 'New Mexico', 'Nevada', 'Utah',
            'Montana', 'Idaho', 'Oregon', 'Washington', 'Wyoming')

FIPS <- c()
for(i in states) {
  FIPS_temp <- paste0(fips(i),list_counties(state = i)[,2]) 
  FIPS <- append(FIPS,FIPS_temp)
}

data.sub <- data %>% filter(GEOID %in% FIPS)

# Add indicator for smoke-day
# All days in the data are smoke-days
# When we expand the dates, we add the non-smoke days
data.sub$smoke_day <- 1

# Get full combination of grid cell-days
# Warning: this may require a large amount of memory
out = expand.grid(GEOID = unique(data.sub$GEOID), date = dates)

# Make data.sub$data type "date"
data.sub$date <- ymd(data.sub$date)

# Match smokePM predictions on smoke days to grid cell-days
out = left_join(out, data.sub, by = c("GEOID", "date"))

# Predict 0 for remaining grid cell-days, which are non-smoke days
out = mutate(out, smokePM_pred = replace_na(smokePM_pred, 0))

# Impute 0 for remaining non-smoke days for smoke_day indicator
out = mutate(out, smoke_day = replace_na(smoke_day, 0))


# Check to see how many dates each GEOID has
# Each should have 2024 days -> good!
out %>% count(GEOID)

## Add "Day of Year" column
out$DOY <- yday(out$date)

# Save clean data
exp_data <- out
save(exp_data,file = "~/Library/Mobile Documents/com~apple~CloudDocs/Harvard/NSAPH Wilfires/wildfires_code/data/exp_data_2006_2016_may_dec.RData")

