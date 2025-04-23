# this code will take all processed years of denominators and load them
# then process them together

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part5.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# years of data
years = c(2006:2016)

# codes from SSA_STATE_CD info here https://www.resdac.org/cms-data/variables/medpar-beneficiary-residence-ssa-standard-state-code

# location of files
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/in_progress_denom_FFS/')

# loop through and load processed admissions file for next stage of processing
dat.all = data.frame()
for(year in years){
    print(paste0('loading ',year))

    # load current year's medicare data
    dat.current = readRDS(paste0(dir.input,'medicare_denom_processing_',year,'.rds'))
    #names(dat.current)[2] = 'fips'

    dat.all=rbind(dat.all,dat.current)
}

# summarise one final time to get over entire multi-year period
library(plyr)
dat_denom_sum_total = ddply(dat.all,.(year,county),summarise,population=sum(population))

# reorder
dat_denom_sum_total = dat_denom_sum_total[order(dat_denom_sum_total$year,dat_denom_sum_total$county),]

# save processed denom file
ifelse(!dir.exists(dir.input), dir.create(dir.input, recursive=TRUE), FALSE)
saveRDS(dat_denom_sum_total, paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))

