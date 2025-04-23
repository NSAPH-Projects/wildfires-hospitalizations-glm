# this code will take a selected year's admission files and load it
# then batch process it in intervals of 50,000

# this file uses the CCS R package where we have a diagnosis categorization based on ICD-9 codes
# and ICD-10 mappings were provided later. We group 13726 ICD-9 codes and 72446 iCD-10 codes into 308 CCS diagnosis categories.
# NOTE: Use CCS_DX_mapping (CCSR was made only for ICD-10 and does not have mappings for ICD-9)

rm(list=ls())

library(plyr)
library(dplyr)
library(lubridate)

# arguments from Rscript
args<-commandArgs(TRUE)
for (i in 1:length(args)) { eval (parse (text = args[[i]] )) }


# expand grid of stuff
# changed to 2006+ since earliest wildfires is in 2006 
years = c(2006:2016)
year = years[seedVal]

print(year)

#library(here)
# 1a Declare root directory
project.folder = c('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/')

# 1. SET UP CCS LOOKUP
if(year%in%c(2000:2016)){
    
    #use merged categories + mapping from CCS R package 
    setwd('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/')
    #code: ICD-9 or ICD-10 code, category_code: CCS category (308), 
    #category_desc: CCS category description, code chapter: broad cause (18), vocabulary_id: type of code  
    code.lookup = readRDS('CCS_DX.rds')
    #names(code.lookup) = c('icd9','ccs_category','ccs_category_description')
}


# 2. BATCH PROCESS THE ADMISSIONS FILES

# set location for loading and processing admissions files
library(fst)
setwd('/n/dominici_nsaph_l3/Lab/projects/analytic/')

# load admissions file with year of interest and figure out number of rows in file
file_admissions = paste0('admissions_by_year/admissions_',year,'.fst')
metadata = fst.metadata(file_admissions)
num_rows = metadata$nrOfRows
#num_rows = 10
intervals = seq(from=1, to = num_rows, by=50000)
#intervals = seq(from=1, to = num_rows, by=10)
intervals = c(intervals,num_rows)

# loop through batches of 50,000 of admissions file, summarise each batch, then add to a larger file



dat_admissions_sum_total = data.frame()
dat_admissions_sum_total_na = data.frame()
dat_admissions_sum_unique_qid_total = c()

for(i in seq((length(intervals)-1))){

    print(paste0('Processing batch ',i,' of ',length(intervals)-1))

    dat_admissions = read_fst(file_admissions, from=intervals[i], to=(intervals[i+1]-1))
    
    # code for emergency or non-emergency via
    # https://med.noridianmedicare.com/web/jea/topics/claim-submission/type-of-admission-or-visit-codes
    # 1=emergency, 2=urgent, 5=trauma 
    dat_admissions$emergency = ifelse(dat_admissions$ADM_TYPE%in%c('1','2','5'),1,0)
    
    # filter to emergency
    dat_admissions_er <- dat_admissions %>% filter(emergency == 1)
    
    # Extract the FIPS codes for western states
    western_states_fips <- c(6, 41, 53, 32, 16, 30, 56, 49,  8, 35,  4)
    
    model_codes <- unique(code.lookup$code_chapter)[c(1:10,11:12,18)]
    code.lookup_subset <- code.lookup %>% filter(code_chapter %in% model_codes)
    
    
    # Filter to may to october and western states
    # Convert ADATE to Date format
    df_may_oct <- dat_admissions_er %>%
      mutate(ADATE = as.Date(ADATE, format = "%d%b%Y")) %>%
      filter(month(ADATE) >= 5 & month(ADATE) <= 10) %>%
      filter(SSA_STATE_CD %in% western_states_fips) %>% 
      filter(DIAG1 %in% code.lookup_subset$code) # filter to codes of interest
    
    # Print result
    print(western_states_fips)
    
    # save qid
    dat_admissions_sum_unique_qid <- unique(df_may_oct$QID)

    # summarise by state, county, year, date of admission, ICD-9 code 
    ##DIAG1: ICD-9 code for primary diagnosis (until October 1st, 2015)
  
    dat_admissions_sum = ddply(dat_admissions,.(SSA_STATE_CD,SSA_CNTY_CD,YEAR,ADATE,DIAG1, emergency),nrow)
    names(dat_admissions_sum)[names(dat_admissions_sum) == "V1"] <- "cases"

    # match up the cause of hospitalization groupings with the DIAG1 coding
    # CCS_DX only considers 86172 codes 
    dat_admissions_sum = merge(dat_admissions_sum,code.lookup,by.x=c('DIAG1'),by.y=c('code'),all.x=TRUE)

    # add na files to total na file to check missing codes
    dat_admissions_sum_na = dat_admissions_sum[rowSums(is.na(dat_admissions_sum)) > 0,]
    dat_admissions_sum_total_na = rbind(dat_admissions_sum_total_na, dat_admissions_sum_na)

    # summarise again by new groupings
    dat_admissions_sum = ddply(dat_admissions_sum,.(SSA_STATE_CD,SSA_CNTY_CD,YEAR,ADATE,category_code, emergency),summarise,cases=sum(cases))
    
    print(paste0('Number of total rows = ',dim(dat_admissions_sum)[1]))
    print(paste0('Number of complete rows = ',dim(na.omit(dat_admissions_sum))[1]))

    # bind to total file
    dat_admissions_sum_total = rbind(dat_admissions_sum_total, dat_admissions_sum)
    dat_admissions_sum_unique_qid_total = c(dat_admissions_sum_unique_qid_total, dat_admissions_sum_unique_qid)

}


# summarise one final time to get over entire
dat_admissions_sum_total = ddply(dat_admissions_sum_total,.(SSA_STATE_CD,SSA_CNTY_CD,YEAR,ADATE,category_code, emergency),summarise,cases=sum(cases))

# check to see if total number of original rows (i.e., ind. cases) is total number of final cases in file
print(year)
print(paste0('Original number of total rows: ',num_rows,'. Number of total rows in final file: ',sum(dat_admissions_sum_total$cases)))

# save processed admissions file for next stage of processing
dir.output = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/in_progress/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
saveRDS(dat_admissions_sum_total, paste0(dir.output,'medicare_admissions_er_processing_',year,'.rds'))

# save unique qid
saveRDS(dat_admissions_sum_unique_qid_total, paste0(dir.output,'dat_admissions_unique_qid_total_',year,'_v2.rds'))


# save missing icd9 records too
dat_admissions_sum_total_na = dat_admissions_sum_total_na[,1]
dat_admissions_sum_total_na = unique(dat_admissions_sum_total_na)
write.csv(dat_admissions_sum_total_na, paste0(dir.output,'NA_medicare_admissions_er_processing_',year,'.csv'))


