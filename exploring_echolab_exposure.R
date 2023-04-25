## Exploring EchoLab Exposure Data ##
## This script subsets the echolab exposure data to be the Western US ##
## Date Created: 09/26/2022 

# Load Libraries
library(usmap)
library(tigris)
library(tidyverse)
library(lubridate)
library(kableExtra)

# Import Data
data <- read_csv("~/Documents/Harvard/NSAPH Wilfires/echolab_exposuredata/county/smokePM2pt5_predictions_daily_county_20060101-20201231.csv")

# Load full set of dates
# We are only interested in May 1 - October 31 for each year
dates = c(seq.Date(ymd("20060501"), ymd("20061031"), by = "day"),
          seq.Date(ymd("20070501"), ymd("20071031"), by = "day"),
          seq.Date(ymd("20080501"), ymd("20081031"), by = "day"),
          seq.Date(ymd("20090501"), ymd("20091031"), by = "day"),
          seq.Date(ymd("20100501"), ymd("20101031"), by = "day"),
          seq.Date(ymd("20110501"), ymd("20111031"), by = "day"),
          seq.Date(ymd("20120501"), ymd("20121031"), by = "day"),
          seq.Date(ymd("20130501"), ymd("20131031"), by = "day"),
          seq.Date(ymd("20140501"), ymd("20141031"), by = "day"),
          seq.Date(ymd("20150501"), ymd("20151031"), by = "day"),
          seq.Date(ymd("20160501"), ymd("20161031"), by = "day"),
          seq.Date(ymd("20170501"), ymd("20171031"), by = "day"),
          seq.Date(ymd("20180501"), ymd("20181031"), by = "day"))


# Filter to be Counties in the Western US
states <- c('Arizona', 'California', 'Colorado', 'New Mexico', 'Nevada', 'Utah',
            'Montana', 'Idaho', 'Oregon', 'Washington', 'Wyoming')
FIPS <- c()
for(i in states) {
  FIPS_temp <- paste0(fips(i),list_counties(state = i)[,2]) 
  FIPS <- append(FIPS,FIPS_temp)
}

data.sub <- data %>% filter(GEOID %in% FIPS)


# Get full combination of grid cell-days
# Warning: this may require a large amount of memory
out = expand.grid(GEOID = unique(data.sub$GEOID), date = dates)

# Make data.sub$data type "date"
data.sub$date <- ymd(data.sub$date)

# Match smokePM predictions on smoke days to grid cell-days
out = left_join(out, data.sub, by = c("GEOID", "date"))

# Predict 0 for remaining grid cell-days, which are non-smoke days
out = mutate(out, smokePM_pred = replace_na(smokePM_pred, 0))


# Check to see how many dates each GEOID has
# Each should have 2392 days -> good!
out %>% count(GEOID)

## make it easier to identify days
#data.sub <- data.sub %>% 
#  mutate(year = substr(data.sub$date, 1, 4)) %>%
#  mutate(day = substr(data.sub$date, start = 5, stop = 8))

## Add "Day of Year" column
out$DOY <- yday(out$date)

# Save clean data
#save(out,file = "~/Documents/Harvard/NSAPH Wilfires/echolab_exposuredata/county/WF_season_data.RData")

#############################################################################

#-------------------------- Explore data ----------------------------

# How many are 0?
length(which(out$smokePM_pred == 0.0)) # 731663
length(out$smokePM_pred) # 990288 total estimates


# Get summary statistics for data I do have
summary(data.sub$smokePM_pred)
summary(out$smokePM_pred)

# How many are greater than 10? Our arbitrary threshold
length(which(out$smokePM_pred > 10)) #37459 out of 339996 obs 11%

# Summary Statistics
t(as.array(summary(out$smokePM_pred))) %>%
  kbl() %>%
  kable_classic_2(full_width = F)

#-------------------- How many are greater than 10? ----------------------------##
out %>% filter(smokePM_pred > 10) %>%
  count(sort = TRUE) %>% View()


# How many in each FIPS?
out %>% filter(out$smokePM_pred > 10) %>%
  count(GEOID, DOY, sort = TRUE) %>% View()
temp <- out %>% filter(out$smokePM_pred > 10) %>%
  count(GEOID, DOY, sort = TRUE)
t(as.array(summary(temp$n))) %>%
  kbl() %>%
  kable_classic_2(full_width = F)

#---------------- How many in each year are > 0?--------------------------------##
out %>% filter(out$smokePM_pred > 0) %>%
  count(Year, sort = TRUE) %>% View()

length(which(temp$n > 1))
####################################################################





