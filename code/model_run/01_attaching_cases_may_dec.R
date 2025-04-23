# This script attaches medicare data to the exposure PM data

# Shell script should include the following arguments:
# exp_data_path: path to exposure data
# er: 1 for er 0 for total

rm(list=ls())

# arguments from Rscript
args<-commandArgs(TRUE)
for (i in 1:length(args)) { eval (parse (text = args[[i]] )) }

print(seedVal)

# for testing
#seedVal = 1
#exp_data_path="/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data/exp_model_input_periods_lag3.RData" # path to exposure data

# years of analysis
years = c(2000:2016)

###############
## Load data ##
###############

# load ccs lookup
#if we decide to use the CCS data file, then we should just use the multi-level file here !!!
code.lookup.merged = read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/CCS_lookup_2015.csv')
#remove column "X" 
code.lookup.merged$X = NULL
code.lookup.merged = subset(code.lookup.merged, !(ccs_level_1%in%c(11,14,15,17,18)))

# make list of broad causes of hospitalization (level 1 names)
causes_groups = unique(as.character(code.lookup.merged$ccs_level_1_description))

# process for finding broad causes of death and matching sub causes
causes_group = causes_groups[seedVal]
code.lookup.merged.subset = subset(code.lookup.merged, ccs_level_1_description==causes_group)
causes_to_load = unique(as.character(code.lookup.merged.subset$ccs_level_3_description))
causes_to_load = trimws(causes_to_load)

# Check if merged data exists (i.e. merging hospitalizations with exposure)

# check to see if a file exists for the analysis
#input.file <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/total/medicare_",gsub(" ", "_", causes_group),"_merged_data_lag",lag,".RData")

# directory to load data from
if(er == 1){
  years = c(2006:2016)
  dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')
} else{
  years = c(2006:2016)
  dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_total/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')
}

# CCS level 1 input file
input.file = paste0(dir.input,'medicare_',gsub(" ", "_", causes_group),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds')

# check to see if a file exists for the analysis
if(file.exists(input.file)){
  print('data already processed... loading...')
  dat = readRDS(paste0(input.file))
  head(dat)
}

# process if file doesn't already exists
if(!file.exists(input.file)){
  
  print('data not processed... processing now...')
  
  # load hospitalizations data
  dat.all = data.frame()
  for(cod.arg in causes_to_load){
    cod.arg = gsub(" ", "_", cod.arg) ; cod.arg = gsub("/", "_", cod.arg)
    input.file.2 = paste0(dir.input,'medicare_',cod.arg,'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds')
    if(file.exists(input.file.2)){
      print(cod.arg)
      dat = readRDS(input.file.2)
      head(dat)
      dat.all=rbind(dat.all,dat)
    }
    if(!file.exists(input.file.2)){
      print(paste0('Cannot find ', cod.arg, ', so skipping over...'))
    }
  }
  
  # resummarise by CCS level 1
  library(plyr)
  dat = ddply(dat.all,.(fipscounty,year,month,day),summarize,cases=sum(cases),population=mean(population))
  
  # load for future attempts so do not need to waste lots of time processing data again
  saveRDS(dat,input.file)
  
  # get rid to save space
  rm(dat.all)
}

#############################
## Attach temperature data ##
#############################
print('attaching weather data')

dat.temp <- readRDS("/n/dominici_nsaph_l3/Lab/projects/analytic/heatvars_county_2000-2020/Heatvars_County_2000-2020_v1.2.Rds")

dat.temp = dat.temp[,c(1,2,5)]
#what is the point of converting to character and then numeric !!! 
dat.temp$StCoFIPS = as.numeric(as.character(dat.temp$StCoFIPS))
library(lubridate)
dat.temp$year <- year(dat.temp$Date)
dat.temp$month <- month(dat.temp$Date)
dat.temp$day <- day(dat.temp$Date)
#dat.temp$Date = as.Date(dat.temp$Date, format="%Y-%m-%d")
#dat.temp$day = as.numeric(as.character(dat.temp$day)) 
#dat.temp$month = as.numeric(as.character(dat.temp$month))
#dat.temp$year = as.numeric(as.character(dat.temp$year))

print('merging weather data with hospitalization data')
colnames(dat)
colnames(dat.temp)
dat = merge(dat,dat.temp,by.x=c('fipscounty','day','month','year'),by.y=c('StCoFIPS','day','month','year'),all.x=TRUE)

print('preview of merged data to check')
head(dat)

# sample for model testing
years = c(2006:2016)
dat.sample = subset(dat,year%in%years)

# Load exposure data
load(exp_data_path)
exp_model_input <- exp_data

# change fips/GEOID to integer cause that's how dat.sample treats fipscounty
# this ensure we merge properly
exp_model_input$GEOID = as.integer(exp_model_input$GEOID)
fips = unique(exp_model_input$GEOID)
min(fips)

# subset medicare data to fips in exposure data
dat.sample = subset(dat.sample,fipscounty%in%fips)

# add date info 
#library(lubridate)
dat.sample$date = with(dat.sample,paste0(day,'/',month,'/',year))
dat.sample$date = as.Date(dat.sample$date, format="%d/%m/%Y")
dat.sample$dow = as.factor(weekdays(dat.sample$date))
dat.sample$doy = as.numeric(strftime(dat.sample$date, format = "%j"))

# make year numeric (I think it is already numeric, but doesn't hurt)
dat.sample$year = as.numeric(dat.sample$year)

#library(plyr)

# create log of population
dat.sample$logpop <- log(dat.sample$population)

################################
## Attach smoke and SES data ##
###############################
library(readr)

brfss_county_interpolated <- read_csv("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data/brfss_county_interpolated.csv")
colnames(brfss_county_interpolated)
dat.sample = merge(dat.sample,brfss_county_interpolated,by.x=c('fipscounty','year'),by.y=c('fips','year'),all.x=TRUE)

census_county_interpolated <- read_csv("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data/census_county_interpolated.csv")
census_county_interpolated$fips <- as.numeric(census_county_interpolated$fips)
colnames(census_county_interpolated)
census_county_interpolated$population <- NULL
dat.sample = merge(dat.sample,census_county_interpolated,by.x=c('fipscounty','year'),by.y=c('fips','year'),all.x=TRUE)

################################################
## EDIT 02/13/25: Attach FFS denominator data ##
################################################
# Load FFS denominator data
medicare_denom_2006_2016 <- readRDS("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/in_progress_denom_FFS/medicare_denom_2006_2016.rds")
medicare_denom_2006_2016$county <- as.numeric(medicare_denom_2006_2016$county)
dat.sample$population <- NULL # wrong data

dat.sample = merge(dat.sample,medicare_denom_2006_2016,by.x=c('fipscounty','year'),by.y=c('county','year'),all.x=TRUE)



##############################
## Merge with exposure data ##
##############################


#library(dplyr)
# Merge 

dat.sample.multiple = dat.sample
dat.sample = NULL
dat.merged.multiple = merge(exp_model_input,dat.sample.multiple,by.x=c('date','GEOID'),
                            by.y=c('date','fipscounty'),all.x=TRUE)



if(er == 1){
  save(dat.merged.multiple, file = paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/er/medicare_",gsub(" ", "_", causes_group),"_merged_data_may_dec.RData"))
  
} else{
  save(dat.merged.multiple, file = paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/total/medicare_",gsub(" ", "_", causes_group),"_merged_data.RData"))
}
