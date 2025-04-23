# this code will take processed merged cases and denom files
# make a complete grid for each file

#edited from RMP on 6/26 - SA 
#edited from SA on 5/17/23 - SV

rm(list=ls())

library(plyr)


# expand grid of stuff temporary
years = c(2006:2016)

print(years)

# load processed admissions file
dir.input.local = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_rates_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.admissions)=1:nrow(dat.admissions)
names(dat.admissions)


library(plyr)

# isolate a particular cause of hospitalisations
ccs_codes = sort(unique(dat.admissions$category_code))

# ccs names
ccs.names = read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_diag_name')
ccs.names$full_diag_name = as.character(ccs.names$full_diag_name)
head(ccs.names)

# output directory
dir.output = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# fully expand dates, fips then match to loaded admissions data
dates = seq(as.Date("2000-01-01"), as.Date("2016-12-31"), by="days")

fipscounty = sort(unique(dat.admissions$fipscounty))
head(fipscounty)
complete.grid = expand.grid(dates=dates,fipscounty=fipscounty)
head(complete.grid)

library(lubridate)

#recoded using lubridate 
complete.grid$year = year(dates)
complete.grid$month = month(dates)
complete.grid$day = day(dates)
complete.grid$dates = NULL

# PREPARE DENOM FILES

# load processed denom file
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/in_progress_denom/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)
dat.denom$fips = paste0('0',dat.denom$fips)
dat.denom$fips = substr(dat.denom$fips, nchar(dat.denom$fips)-5+1, nchar(dat.denom$fips))
dat.denom$state.fips = substr(dat.denom$fips, 1,2)
print("dat.denom")

#attach state recognized fips codes 
fips.lookup <- read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/state_fips_lookup.csv')
print("fips.lookup")
#remove Hawaii (just concerned about western US)
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(15)),]
print("HI")
#remove third column with two letter state abbreviations 
fips.lookup = fips.lookup[,c(1:2)]
head(dat.denom)

dat.denom$state.fips = as.numeric(dat.denom$state.fips)

dat.denom = merge(dat.denom,fips.lookup,by.x='state.fips',by.y='fips',all.x=TRUE)

dat.denom$full_name = as.character(dat.denom$full_name)

dat.denom$full_name[is.na(dat.denom$full_name)==TRUE] <- 'Died within year'


# CYCLE THROUGH EACH CCS
print(ccs_codes)
for(ccs_code in ccs_codes){
  print(ccs_code)

  # print full name
  full_diag_name = as.character(subset(ccs.names,css_category==ccs_code)[2])
  full_diag_name = trimws(full_diag_name)
  full_diag_name = gsub('/',' ',full_diag_name)
  print(paste0(ccs_code,': ',full_diag_name))

  # filter out single ccs category
  dat.admissions.single = subset(dat.admissions,category_code==ccs_code)
  dat.admissions.single$month = as.numeric(dat.admissions.single$month)
  dat.admissions.single$day = as.numeric(dat.admissions.single$day)
  
  # merge deaths counts with complete grid to ensure there are rows with zero deaths
  dat.complete = merge(complete.grid,dat.admissions.single,by=c('fipscounty','year','month','day'),all.x='TRUE') 
  
  # assign missing cases to have value 0
  dat.complete$cases = ifelse(is.na(dat.complete$cases)==TRUE,0,dat.complete$cases)
  
  # match admissions and wf data by fips and date
  dat.complete$month = as.numeric(dat.complete$month)
  dat.complete$year = as.numeric(dat.complete$year)
  dat.complete$day = as.numeric(dat.complete$day)
  
  # MERGE CASES AND DENOM FILES
  dat.denom$fips = as.numeric(dat.denom$fips)
  dat.denom = dat.denom[,c('year','fips','population')]
  dat.merged = merge(dat.complete[,c('year','fipscounty','month','day','cases')], dat.denom[,c('year','fips','population')], by.x=c('year','fipscounty'),by.y=c('year','fips'),all.x=TRUE)
  
  # only take years from beginning of actual intended dataset
  dat.merged = subset(dat.merged,year%in%years)
  
  # exclude those weird georgia counties (this is done again because of the re-merge with the denominator files) 
  # since we restrict to western us this won't matter
  gfips_to_exclude = c(13007, 13037, 13061, 13087, 13099, 13131, 13201, 13239, 13243, 13253, 13273)
  dat.merged = subset(dat.merged,!(year%in%c(1999:2001)&fipscounty%in%c(gfips_to_exclude)))
  
  # calculate rates
  dat.merged$rates = with(dat.merged,cases/population)
  
  # check missing values
  dat.merged.na = dat.merged[rowSums(is.na(dat.merged)) > 0,]
  
  print(paste0('Total number of cases = ',sum(dat.merged$cases)))
  print(paste0('Unmatched cases = ',sum(dat.merged.na$cases)))
  
  head(dat.merged)
  
  saveRDS(dat.merged, paste0(dir.output,'medicare_',gsub(" ", "_", full_diag_name),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds'))
}

