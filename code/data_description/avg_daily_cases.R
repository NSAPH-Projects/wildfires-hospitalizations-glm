# This scrip summarizes Average Daily Hospitalization Rates (per 100k) with SD

####################
## Load libraries ##
####################
library(fixest)
library(DescTools)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(kableExtra)


#####################
## 1. Set up data ##
####################

# years of analysis
years = c(2006:2016)

# load ccs lookup
#if we decide to use the CCS data file, then we should just use the multi-level file here !!!
code.lookup.merged = read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/CCS_lookup_2015.csv')
#remove column "X" 
code.lookup.merged$X = NULL
code.lookup.merged = subset(code.lookup.merged, !(ccs_level_1%in%c(11,14,15,17,18)))

# make list of broad causes of hospitalization (level 1 names)
causes_groups = unique(as.character(code.lookup.merged$ccs_level_1_description))

##########################################
## 2. Initialize data.frame for results ##
#########################################
results <- data.frame()

#################################################################
## 3. For each cause, load data, count cases, and save results ##
#################################################################
for (i in 1:13){
  cause_group = causes_groups[i]
  
  # Load data
  er = 1
  if(er == 1){
    load(paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/er/medicare_",gsub(" ", "_", cause_group),"_merged_data.RData"))
    
  } else{
    load(paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/total/medicare_",gsub(" ", "_", cause_group),"_merged_data.RData"))
  }
  
  # Create rate variable
  dat.merged.multiple$cases_rate <- dat.merged.multiple$cases/dat.merged.multiple$population * 100000
  
  # Assess the quantiles of smoke PM
  quantile(dat.merged.multiple$smokePM_pred, .999)
  
  # Convert to data.table
  pdata <- as.data.table(dat.merged.multiple, ~GEOID + date)
  
  
  weighted_daily_avg_cases <- pdata %>%
    summarise(
      weighted_avg =  weighted.mean(cases, population, na.rm = TRUE)
    )
  
  weighted_daily_avg_rates <- pdata %>%
    summarise(
      weighted_avg =  weighted.mean(cases_rate, population, na.rm = TRUE)
    )
  
  # Save results
  results <- rbind(results,data.frame(cause = cause_group, avg_cases = weighted_daily_avg_cases$weighted_avg, avg_rates =  weighted_daily_avg_rates$weighted_avg))
  
}

#Test

# Define weighted SD function
weighted.sd <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    valid <- !is.na(x) & !is.na(w)
    x <- x[valid]
    w <- w[valid]
  }
  mu <- weighted.mean(x, w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

# Initialize results
results <- data.frame()

# Loop through causes
for (i in seq_along(causes_groups)) {
  cause_group <- causes_groups[i]
  
  # Load data
  load(paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/er/medicare_", gsub(" ", "_", cause_group), "_merged_data.RData"))
  
  # Calculate rate per 100k
  dat.merged.multiple$cases_rate <- dat.merged.multiple$cases / dat.merged.multiple$population * 100000
  
  # Convert to data.table
  pdata <- as.data.table(dat.merged.multiple)
  
  # Weighted average and SD of cases_rate
  avg_daily_rate <- weighted.mean(pdata$cases_rate, pdata$population, na.rm = TRUE)
  sd_daily_rate <- weighted.sd(pdata$cases_rate, pdata$population)
  
  # Weighted average of raw daily cases
  avg_daily_cases <- weighted.mean(pdata$cases, pdata$population, na.rm = TRUE)
  
  # Add to results
  results <- rbind(
    results,
    data.frame(
      cause = cause_group,
      avg_daily_cases = avg_daily_cases,
      avg_daily_rate_per_100k = avg_daily_rate,
      sd_daily_rate_per_100k = sd_daily_rate
    )
  )
}

# Display result
results %>%
  arrange(desc(avg_daily_rate_per_100k)) %>%
  kbl(digits = 2, caption = "Average Daily Hospitalization Rates (per 100k) with SD") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


