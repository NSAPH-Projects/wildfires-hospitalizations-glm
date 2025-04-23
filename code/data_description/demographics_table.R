# Code to create the demographics table in paper supplement

# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

# Directory where your new unique_qid files are saved
dir.input <- '/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/in_progress_denom_FFS/'

# List all unique_qid files across years
files <- list.files(path = dir.input, pattern = "unique_qid_sex_age_dual_race_.*\\.rds", full.names = TRUE)

# Load and combine all into one data frame
all_qid_data <- do.call(rbind, lapply(files, readRDS))

# Just in case — remove duplicates by qid
all_qid_data <- all_qid_data %>% distinct(qid, .keep_all = TRUE)

# Basic demographics
total_population <- nrow(all_qid_data)

summary_all_years <- all_qid_data %>%
  summarise(
    total_population = n(),
    
    # Sex
    male_n = sum(sex == 1, na.rm = TRUE),
    male_perc = round(mean(sex == 1, na.rm = TRUE) * 100, 2),
    
    female_n = sum(sex == 2, na.rm = TRUE),
    female_perc = round(mean(sex == 2, na.rm = TRUE) * 100, 2),
    
    # Age
    #mean_age = round(mean(age, na.rm = TRUE), 2),
    #age_sd = round(sd(age, na.rm = TRUE), 2),
    
    # Dual eligible
    dual_n = sum(dual == 1, na.rm = TRUE),
    dual_perc = round(mean(dual == 1, na.rm = TRUE) * 100, 2),
    
    # Race
    race_white_n = sum(race == 1, na.rm = TRUE),
    race_white_perc = round(mean(race == 1, na.rm = TRUE) * 100, 2),
    
    race_black_n = sum(race == 2, na.rm = TRUE),
    race_black_perc = round(mean(race == 2, na.rm = TRUE) * 100, 2),
    
    race_asian_n = sum(race == 4, na.rm = TRUE),
    race_asian_perc = round(mean(race == 4, na.rm = TRUE) * 100, 2),
    
    race_hisp_n = sum(race == 5, na.rm = TRUE),
    race_hisp_perc = round(mean(race == 5, na.rm = TRUE) * 100, 2),
    
    race_native_n = sum(race == 6, na.rm = TRUE),
    race_native_perc = round(mean(race == 6, na.rm = TRUE) * 100, 2),
    
    race_other_n = sum(race == 3, na.rm = TRUE),
    race_other_perc = round(mean(race == 3, na.rm = TRUE) * 100, 2),
    
    race_unknown_n = sum(race == 0, na.rm = TRUE),
    race_unknown_perc = round(mean(race == 0, na.rm = TRUE) * 100, 2)
  )

# Convert your wide summary into long format
summary_long <- summary_all_years %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "value")

var_order <- c(
  "Male", "Female",
  "Dual Eligible",
  "White", "Black", "Asian", "Hispanic",
  "Native American", "Other Race", "Unknown Race"
)


# Separate counts and percentages using naming convention
summary_long <- summary_long %>%
  mutate(
    type = case_when(
      grepl("_perc$", var) ~ "percent",
      grepl("_n$", var)    ~ "count",
      TRUE                 ~ "other"
    ),
    var_clean = var %>%
      str_remove("_perc$") %>%
      str_remove("_n$")
  )

# Keep only demographic rows (skip other metrics for now)
summary_clean <- summary_long %>%
  filter(type %in% c("count", "percent")) %>%
  select(var_clean, type, value) %>%
  pivot_wider(names_from = type, values_from = value)

# Arrange rows nicely
summary_clean <- summary_clean %>%
  select(var_clean, count, percent) %>%
  arrange(var_clean)

# Optional: rename rows for clarity

summary_clean$var_clean <- recode(summary_clean$var_clean,
                                  "male" = "Male",
                                  "female" = "Female",
                                  "dual" = "Dual Eligible",
                                  "race_white" = "White",
                                  "race_black" = "Black",
                                  "race_asian" = "Asian",
                                  "race_hisp" = "Hispanic",
                                  "race_native" = "Native American",
                                  "race_other" = "Other Race",
                                  "race_unknown" = "Unknown Race"
)

# Set as factor with desired order
summary_clean$var_clean <- factor(summary_clean$var_clean, levels = var_order)

summary_clean <- summary_clean %>%
  arrange(var_clean)


# Display it
print(summary_clean)


## number unique qid hospitalizations ##
# Define directory containing yearly summary files
dir.input <- '/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/in_progress/'


# List all saved qid files
files <- list.files(path = dir.input, pattern = "dat_admissions_unique_qid_total.*\\_v2.rds", full.names = TRUE)

# Load all data into a single dataframe
all_years_qid_data <- do.call(c, lapply(files, readRDS))

length(unique(all_years_qid_data))
