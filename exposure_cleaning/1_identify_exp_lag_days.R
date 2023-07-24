## Find lagged county days and process it for a model input
## This script identifies exposure days as those with PM values above a threshold (10)
## Then lagged days are added as the seven days after exposure

start_year <- as.numeric("2006")
end_year <-  as.numeric("2018")
dir_out <- "~/Documents/Harvard/NSAPH Wilfires/wildfires_code/data/"


# load necessary packages
library('tidyverse') ; library('lubridate')

#setwd('~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/')
#dir.output<- '~/Desktop/HARVARD/Spring2022/IndStudy/GFD_USA/'

# Load wildfire data
load("~/Documents/Harvard/NSAPH Wilfires/echolab_exposuredata/county/WF_season_data.RData")

# Obtain the  unique counties (414 counties)
all_counties = unique(out$GEOID)

#to determine number of states included, use USA_table (should be all except Hawaii)

# add lag to the date -- takes the start date, adds lag value (0-7) and 
# then pulls the year, month, day of the new date 
# fills in event_lag column with 1's and 0's based on lag
add_date_info = function(dat, lag_chosen){
  dat$date = as.Date(dat$date, format="%Y-%m-%d") + lag_chosen
  dat$year     = lubridate::year(dat$date)
  dat$month    = lubridate::month(dat$date)
  dat$day      = lubridate::day(dat$date)
  
  col_name     = paste0('event_lag',lag_chosen)
  dat[col_name]= 1
  
  dat = dat[
    with(dat, order(year,month,day)),
  ]
  
  dat = dat[,c('GEOID','year','month','day','date',col_name)]
  
  return(dat)
}

# lags to have included
lags=c(0:7)

#subset data to just be those with exposures above the threshold
thresh <- 10
out_sub <- out[out$smokePM_pred > thresh,]

# calculate lag values for the unconstrained lag model
#creates a data frame for each lag that is included (0-7) based on function above 
#form: county, year, month, day, eventlag0 = 1; county, year, month, day + 1, eventlag1 = 1
for(lag in lags){
  assign(paste0('county_wf_edit_',lag), add_date_info(out_sub, lag))
  print(head(get(paste0('county_wf_edit_',lag))))
  
}

#create a grid of all county/date combinations 
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

complete_grid = expand.grid(date=dates,GEOID=all_counties)
complete_grid$year = year(dates) 
complete_grid$month = month(dates)
complete_grid$day = day(dates)
county_wf_edit_array = complete_grid

# Merge the grid of county/date combinations with the dataframes that have the lag columns 
# add multiple lags to array for all counties in dataset
for(lag in lags){
  print(paste0('Matching lag ',lag))
  county_wf_edit_array = merge(county_wf_edit_array,get(paste0('county_wf_edit_',lag)),by=c('GEOID','year','month','day','date'), all.x=T)
}

county_wf_edit_array[is.na(county_wf_edit_array)] <- 0

# Merge Smoke PM2.5 level
county_wf_edit_array = merge(county_wf_edit_array,out,by=c('GEOID','date'), all.x=T)


# save as rds for analysis
save(county_wf_edit_array, file = paste0(dir_out,'county_wf_lag_array_',start_year,'_',end_year,'.RData'))


