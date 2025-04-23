# this code will take all processed years and load them
# then process them together because some admissions are in the previous year
# this script subsets to emergency room visits

rm(list=ls())


# years of data 
years = c(2006:2016)

# codes from SSA_STATE_CD info here https://www.resdac.org/cms-data/variables/medpar-beneficiary-residence-ssa-standard-state-code

# location of files
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/in_progress/')

# loop through and load processed admissions file for next stage of processing
dat.all = data.frame()
for(year in years){
    print(paste0('loading ',year))

    # load current year's medicare data
    dat.current = readRDS(paste0(dir.input,'medicare_admissions_er_processing_',year,'.rds'))
    
    library(lubridate)
    # fix date categories (partially recoded from RMP)
    dat.current$date = format(as.Date(dat.current$ADATE, "%d%B%Y"))
    dat.current$year = year(dat.current$date)
    dat.current$month = month(dat.current$date)
    dat.current$day = day(dat.current$date)

    # fix ccs category number 
    dat.current$category_code = as.numeric(dat.current$category_code)

    # summarising by date of admission (which isn't necessarily in the file for the year on record)
    # hospitalizations are dated by discharge year which may not always be in the same year of admission 
    print('summarising file...')
    library(plyr)
    dat.current = ddply(dat.current,.(SSA_STATE_CD,SSA_CNTY_CD,category_code,emergency, day,month,year),summarise,cases=sum(cases))
    dat.all=rbind(dat.all,dat.current)
}

# only take emergency values
dat.all.emergency = subset(dat.all,emergency==1)
dat.all.non.emergency = subset(dat.all,emergency==0)

# summarise one final time to get over entire multi-year period
dat_admissions_emergency_sum_total = ddply(dat.all.emergency,.(SSA_STATE_CD,SSA_CNTY_CD,category_code,day,month,year),summarise,cases=sum(cases))
dat_admissions_nonemergency_sum_total = ddply(dat.all.non.emergency,.(SSA_STATE_CD,SSA_CNTY_CD,category_code,day,month,year),summarise,cases=sum(cases))

#reorder
dat_admissions_sum_total = dat_admissions_emergency_sum_total[order(dat_admissions_emergency_sum_total$SSA_STATE_CD,dat_admissions_emergency_sum_total$SSA_CNTY_CD, dat_admissions_emergency_sum_total$category_code,dat_admissions_emergency_sum_total$year,dat_admissions_emergency_sum_total$month,dat_admissions_emergency_sum_total$day),]
dat_admissions_nonemergency_sum_total = dat_admissions_nonemergency_sum_total[order(dat_admissions_nonemergency_sum_total$SSA_STATE_CD,dat_admissions_nonemergency_sum_total$SSA_CNTY_CD, dat_admissions_nonemergency_sum_total$category_code,dat_admissions_nonemergency_sum_total$year,dat_admissions_nonemergency_sum_total$month,dat_admissions_nonemergency_sum_total$day),]

# save processed admissions file
dir.output.local = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/in_progress/')
ifelse(!dir.exists(dir.output.local), dir.create(dir.output.local, recursive=TRUE), FALSE)
saveRDS(dat_admissions_sum_total, paste0(dir.output.local,'medicare_admissions_er_',years[1],'_',years[length(years)],'.rds'))
saveRDS(dat_admissions_nonemergency_sum_total, paste0(dir.output.local,'medicare_admissions_non_er_',years[1],'_',years[length(years)],'.rds'))
