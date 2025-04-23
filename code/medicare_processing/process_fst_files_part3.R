# this code will take a selected year's denominator files and load it
# then batch process it in intervals of 50,000

#SA edited from RMP - 5/31/2022
#SV edited from SA 

rm(list=ls())


# arguments from Rscript
args<-commandArgs(TRUE)
for (i in 1:length(args)) { eval (parse (text = args[[i]] )) }



# expand grid of years
years = c(2006:2016)
year = years[seedVal]

print(year)

# Load necessary libraries
library(fst)
library(plyr)
library(dplyr)

# Set working directory
setwd('/n/dominici_nsaph_l3/Lab/projects/analytic')

# Load denom file with year of interest
file_denom = paste0('denom_by_year/v1/confounder_exposure_merged_nodups_health_',year,'.fst')
metadata = fst.metadata(file_denom)
num_rows = metadata$nrOfRows
intervals = seq(from=1, to = num_rows, by=50000)
intervals = c(intervals,num_rows)

# Initialize data frames
dat_denom_sum_total = data.frame()
dat_denom_pop_total = data.frame()
dat_denom_sum_total_na = data.frame()
dat_admissions_sum_unique_qid_total = c()
dat_qid_info_total <- data.frame()


for(i in seq((length(intervals)-1))){
  
  print(paste0('Processing batch ',i,' of ',length(intervals)-1))
  
  dat_denom = read_fst(file_denom, from=intervals[i], to=(intervals[i+1]-1))
  names(dat_denom)[names(dat_denom) == "year.x"] <- "year"
  
  # Edit 02122025
  # Subset to FFS participants
  dat_denom = dat_denom %>% filter(hmo_mo < 12)
  
  # Subset to western counties
  western_states_fips <- c( "CA", "OR", "WA", "NV", "ID", "MT", "WY", "UT", "CO", "NM", "AZ")
  dat_denom = dat_denom %>% filter(statecode %in% western_states_fips)
  dat_admissions_sum_unique_qid <- unique(dat_denom$qid)
  
  # summarise by year, county 
  # Edit 02122025: fips->county
  dat_denom_pop = ddply(dat_denom,.(year,county),nrow)
  names(dat_denom_pop)[names(dat_denom_pop) == "V1"] <- "population"
  
  unique(dat_denom$race)
  
  # Compute summary statistics
  dat_denom_sum = dat_denom %>%
    group_by(year, county) %>%
    summarise(
      male_n = sum(sex == 1, na.rm = TRUE),
      female_n = sum(sex == 2, na.rm = TRUE),
      age_n = sum(age, na.rm = TRUE),
      age_sq_n = sum(age^2, na.rm = TRUE),
      dual_n = sum(dual == 1, na.rm = TRUE),
      race_white_n = sum(race == 1, na.rm = TRUE),
      race_black_n = sum(race == 2, na.rm = TRUE),
      race_other_n = sum(race == 3, na.rm = TRUE),
      race_asian_n = sum(race == 4, na.rm = TRUE),
      race_hisp_n = sum(race == 5, na.rm = TRUE),
      race_native_n = sum(race == 6, na.rm = TRUE),
      race_unknown_n = sum(race == 0, na.rm = TRUE)
    ) %>%
    ungroup()  # Ensures the result is no longer grouped
  
  # Extract unique qid + demographic info from this batch
  dat_qid_info <- dat_denom %>%
    select(qid, sex, age, dual, race) %>%
    distinct(qid, .keep_all = TRUE)
  
  # Append to total holder
  dat_qid_info_total <- rbind(dat_qid_info_total, dat_qid_info)
  
  
  # Bind to total file
  dat_denom_sum_total = rbind(dat_denom_sum_total, dat_denom_sum)
  dat_denom_pop_total = rbind(dat_denom_pop_total, dat_denom_pop)
  dat_admissions_sum_unique_qid_total = c(dat_admissions_sum_unique_qid_total, dat_admissions_sum_unique_qid)
  
  
}

# Final summarization across entire dataset
dat_denom_sum_total = dat_denom_sum_total %>%
  summarise(
    population = sum(dat_denom_pop_total$population),
    male_n = sum(male_n),
    female_n = sum(female_n),
    age_n = sum(age_n),
    age_sq_n = sum(age_sq_n), 
    dual_n = sum(dual_n),
    race_white_n = sum(race_white_n),
    race_black_n = sum(race_black_n),
    race_other_n = sum(race_other_n),
    race_asian_n = sum(race_asian_n),
    race_hisp_n = sum(race_hisp_n),
    race_native_n = sum(race_native_n),
    race_unknown_n = sum(race_unknown_n)
  )

# Deduplicate across batches
dat_qid_info_total <- dat_qid_info_total %>%
  distinct(qid, .keep_all = TRUE)


# Save processed denom file for next stage
dir.output = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/in_progress_denom_FFS/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
saveRDS(dat_denom_sum_total, paste0(dir.output, 'medicare_summary_processing_', year, '.rds'))
dat_denom_sum_total <- dat_denom_pop_total #rename
saveRDS(dat_denom_sum_total, paste0(dir.output, 'medicare_denom_processing_', year, '.rds'))

saveRDS(dat_admissions_sum_unique_qid_total, paste0(dir.output,'dat_admissions_unique_qid_total_',year,'.rds'))

# Save
saveRDS(dat_qid_info_total, paste0(dir.output, 'unique_qid_sex_age_dual_race_', year, '.rds'))

