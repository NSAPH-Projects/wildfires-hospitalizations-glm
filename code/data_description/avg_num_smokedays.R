# This script calculates the average number of smoke-days

# Load libraries
library(tidyverse)
library(viridis)
library(lubridate)
library(cowplot)

# Load data
load("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data/exp_data_2006_2016.RData")

#########################
## PM data description ##
#########################
# Subset to smoke-days
data_sub <- exp_data %>% filter(smoke_day == 1)

# Get average smoke days across counties for the whole study period
average_smoke_days <- data_sub %>%
  group_by(GEOID) %>%
  summarize(total_smoke_days = sum(smoke_day, na.rm = TRUE)) %>%
  summarize(
    avg_smoke_days = mean(total_smoke_days, na.rm = TRUE),
    sd_smoke_days = sd(total_smoke_days, na.rm = TRUE)
  )

print(average_smoke_days )
