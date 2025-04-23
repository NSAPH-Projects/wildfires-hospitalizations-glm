# Script for figure 2 in the paper
# Describes smoke concentration and counts of hospitalizations

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

# Assign year variable
data_sub$year <- year(data_sub$date)

# For each county-year, calculate the number of smokedays 
temp <- data_sub %>% group_by(GEOID, year) %>% count()

# Find the minimum and maximum number of smokedays in a year
min(temp$n) # 2 smoke days in 2017 for county 53029
max(temp$n) # 122 smoke days in 2018 for county 06019

# Find minimum and maximu, number of smokedays for all counties
temp_county <- data_sub %>%
  group_by(GEOID)  %>% count()
min(temp_county$n) # 178
max(temp_county$n) # 894

# For each year, take the average of smoke-days across counties
temp2 <- temp %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(
    mean_n = mean(n)
  )

mean(temp2$mean_n) # 45
# Counties experience an average of 45 smoke-days a year.

##


####################################################
## Panel 1: Smoke Concentration By Year Bar Chart ##
####################################################

# Subset to smoke-days
data_sub <- exp_data %>% filter(smoke_day == 1)

# Make bins for predicted smoke concentration
data_sub$bin_0_5 <- ifelse(data_sub$smokePM_pred < 5, 1, 0)
data_sub$bin_5_10 <- ifelse(data_sub$smokePM_pred >= 5 & data_sub$smokePM_pred < 10, 1, 0)
data_sub$bin_10_25 <- ifelse(data_sub$smokePM_pred >= 10 & data_sub$smokePM_pred < 25, 1, 0)
data_sub$bin_25_40 <- ifelse(data_sub$smokePM_pred >= 25 & data_sub$smokePM_pred < 40, 1, 0)
data_sub$bin_40_plus <- ifelse(data_sub$smokePM_pred >= 40, 1, 0)

# Get percentages for each bin
sum(data_sub$bin_0_5)/nrow(data_sub) # 80.5 percent
sum(data_sub$bin_5_10)/nrow(data_sub) # 11.8 percent
sum(data_sub$bin_10_25)/nrow(data_sub) # 6.0 percent
sum(data_sub$bin_25_40)/nrow(data_sub) # 1.1 percent
sum(data_sub$bin_40_plus)/nrow(data_sub) # 0.6 percent



data_sub <- data_sub %>%
  mutate(
    bin_smokePM_pred = case_when(
      smokePM_pred < 5 ~ "0-5",
      smokePM_pred >= 5 & smokePM_pred < 10 ~ "5-10",
      smokePM_pred >= 10 & smokePM_pred < 25 ~ "10-25",
      smokePM_pred >= 25 & smokePM_pred < 40 ~ "25-40",
      smokePM_pred >= 40 ~ "40+"
    ))

# Set breaks manually
breaks <- c(seq(0, 100, by = 5), Inf)


# Save plot
p1 <- ggplot(data= data_sub, aes(x=smokePM_pred, fill = factor(bin_smokePM_pred, levels = c("0-5","5-10", "10-25", "25-40", "40+" )))) +
  geom_histogram(aes(y = after_stat(count)/nrow(data_sub)*100), breaks=breaks) +
  labs(x = expression(paste("Smoke PM"[2.5]," (", mu, g, m^-3,
                            ")", sep="")), y = "Share of smoke-days", fill = "") +
  scale_fill_manual(values = c("40+" = "#8B0000", "25-40" = "#D9534F" , "10-25" = "#CD853F", "5-10" ="#FFC107" ,"0-5" ="#FFF176"),
                    labels = c("40+ (0.6%)", "25-40 (1.1%)", "10-25 (6.0%)", "5-10 (11.8%)", "0-5 (80.5%)"),
                    guide = guide_legend(reverse = TRUE)) +
                    #labels = c("0-5 (80.5%)","5-10 (11.8%)", "10-25 (6.0%)", "25-40 (1.1%)", "40+ (0.6%)" )) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  # Set breaks at every 10 x-value interval
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Set y-axis labels as percentages
  theme_minimal() +
  theme(text = element_text(size = 21),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(r = 5, t = 5),  # Adjust margins as needed
    legend.position = c(0.85, 0.8),    # Adjust coordinates (0.95, 0.95 for top-right corner)
    legend.direction = "vertical",       # Set legend direction to vertical
    axis.text = element_text( color = "black"), # Set axis text size and color
    axis.title = element_text(size = 21),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 16)
  ) 


############################################
## Panel 2: Smoke Concentration Histogram ##
############################################

# Assign year variable
data_sub$year <- year(data_sub$date)

# For each county-year, calculate the number of smokedays in each category
plot2 <- data_sub %>% group_by(GEOID, year) %>% count(bin_smokePM_pred)

plot2 %>% group_by(year,bin_smokePM_pred) %>% mean(n)

plot2 <- plot2 %>% group_by(year, bin_smokePM_pred) %>% summarise(
  avg_n_smoke_days = mean(n), total = sum(n)
)

# Save plot
p2 <- ggplot(plot2, aes(fill=factor(bin_smokePM_pred, levels = c("0-5","5-10", "10-25", "25-40", "40+" )), y=total, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  labs(x = "Year", y = "Total Number of Smoke-Days", fill = "") +
  scale_fill_manual(values = c("40+" = "#8B0000", "25-40" = "#D9534F" , "10-25" = "#CD853F", "5-10" ="#FFC107" ,"0-5" ="#FFF176"),
                    #labels = c("0-5","5-10", "10-25", "25-40", "40+" )) +
                    labels = c("40+", "25-40", "10-25", "5-10", "0-5" ),
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("2006,", "2007,", "2008,", "2009,", "2010,",
                              "2011,", "2012,", "2013,", "2014,", "2015,",
                              "2016"),
                   limit = c(2006:2016)) + # "#B87333" "#CD853F"
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Set y-axis labels as percentages
  theme_minimal() +
  theme(text = element_text(size = 21),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text = element_text(color = "black"), # Set axis text size and color
    axis.title = element_text(size = 21),
    legend.key.size = unit(.75, "lines"),
    legend.text = element_text(size = 16),
    legend.position = c(0.85, 0.8)
  )


###################################################
## Panel 3: Counts of Hospitalizations Bar Chart ##
###################################################

output.folder <- paste(wd,'/figures/')
# load ccs lookup
code.lookup.merged = readRDS('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/CCS_DX.rds')
code.lookup.merged = subset(code.lookup.merged, !(code_chapter%in%c("Complications of pregnancy; childbirth; and the puerperium", 
                                                                    "Congenital anomalies",
                                                                    "Certain conditions originating in the perinatal period",
                                                                    "Symptoms; signs; and ill-defined conditions and factors influencing health status",
                                                                    "Residual codes; unclassified; all E codes [259. and 260.]")))

# make list of broad causes of hospitalization (level 1 names)
causes_groups = unique(as.character(code.lookup.merged$code_chapter))



years <- c(2006:2016)

# directory to load data from
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/er/')


library(stringr)

# Get dates for WF season May 1 to October 31
dates <- unique(exp_data$date)

# Make a data frame with columns for year and rows for causes with number of cases filled in
cases_mat <- matrix(NA, nrow = length(causes_groups), ncol = length(years) )
for (i in 1:length(causes_groups)){
  # CCS level 1 input file
  input.file = paste0(dir.input,'medicare_',gsub(" ", "_", causes_groups[i]),'_merged_data.RData')
  load(paste0(input.file))
  
  # Summarize cases by year
  data_total <- dat.merged.multiple %>% group_by(year) %>% summarise(total_cases = sum(cases))
  # Store results
  cases_mat[i,] <- c(data_total[,2])$total_cases
  
}


# Rename causes CCS level 1
causes_groups = gsub('Diseases of the circulatory system', 'Cardiovascular diseases', causes_groups)
causes_groups = gsub('Diseases of the respiratory system', 'Respiratory diseases', causes_groups)

causes_groups = gsub('Neoplasms', 'Cancers', causes_groups)

causes_groups = gsub('Injury and poisoning', 'Injuries', causes_groups)
causes_groups = gsub('Mental Illness', 'Neuropsychiatric disorders', causes_groups)

causes_groups = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', causes_groups)
causes_groups = gsub('Diseases of the digestive system', 'Digestive system diseases', causes_groups)
causes_groups = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', causes_groups)
causes_groups = gsub('Diseases of the genitourinary system', 'Genitourinary diseases', causes_groups)
causes_groups = gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases', causes_groups)
causes_groups = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases', causes_groups)
causes_groups = gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases', causes_groups)
causes_groups = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous tissue diseases', causes_groups)

cases_mat <- data.frame(cases_mat)
colnames(cases_mat) <- years 
rownames(cases_mat) <- causes_groups


cases_mat$total <- rowSums(cases_mat)
total_cases <- sum(cases_mat$total)


colors <- c(
  "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
  "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
  "#FFD700", "#636363", "#969696", "#FFD700"  # Adding darker yellow
)

cases_mat_long <- cases_mat %>%
  tibble::rownames_to_column(var = "cause_group") %>%
  select(-total) %>%
  as_tibble() %>%
  tidyr::pivot_longer(!cause_group, names_to = "year", values_to = "n_cases") 

# reorder CCS level 1 causes for plotting
cases_mat_long$cause_group = factor(cases_mat_long$cause_group,
                                             levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                                      'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                                      'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                                      'Nervous system diseases','Skin and subcutaneous tissue diseases'))



## Create barchart stratified by cause (not year)
plot3 <- cases_mat_long %>% group_by(cause_group) %>% summarise(total_cases = sum(n_cases))

p3 <- plot3 %>%
  ggplot(aes(x = fct_rev(cause_group), y = total_cases, fill = cause_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(x = "Visit Type", y = "Unscheduled Hospitalizations", fill = "") +
  theme_minimal() +
  theme(text = element_text(size = 23),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_text( color = 'black'),
        axis.text.y = element_text( color = 'black'),
        legend.position = "none"
        #legend.text = element_text(size = rel(.75)),
        #legend.key.size = unit(.5, "lines")
  ) +
  coord_flip()

###################
## Arrange plots ##
###################
# Save plot
pdf("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data_description/figures/figure2_v2.pdf",paper='special',width=20,height=15)

# Arrange plots
#plot_grid(
#  plot_grid(p1, p2, nrow = 1, ncol = 2, labels = c('A', 'B')),
#  plot_grid(NULL, p3, NULL, nrow = 1, rel_widths = c(0.5, 6.5, 0.5), labels = c(NULL, 'C', NULL)),
#  nrow = 2
#)


# Arrange plots
plot_grid(
  plot_grid(p1,p2,nrow = 2, ncol = 1, labels = c('A', 'B'),rel_heights  = c(4.5, 4.5),label_size = 18),
  plot_grid(p3, ncol=1, nrow = 1, rel_heights  = c(8), labels = c('C'), label_size = 18),
  nrow = 1, rel_widths = c(2.5,3)
)

dev.off()


# Script for figure 2 in the paper
# Describes smoke concentration and counts of hospitalizations

# Load libraries
library(tidyverse)
library(viridis)
library(lubridate)
library(cowplot)

# Load data
load("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data/exp_data_2006_2016.RData")

#########################
## Panel 1: Smoke Concentration By Year Bar Chart ##
#########################

# Subset to smoke-days
data_sub <- exp_data %>% filter(smoke_day == 1)

# Assign bins
data_sub <- data_sub %>%
  mutate(
    bin_smokePM_pred = case_when(
      smokePM_pred < 5 ~ "0-5",
      smokePM_pred >= 5 & smokePM_pred < 10 ~ "5-10",
      smokePM_pred >= 10 & smokePM_pred < 25 ~ "10-25",
      smokePM_pred >= 25 & smokePM_pred < 40 ~ "25-40",
      smokePM_pred >= 40 ~ "40+"
    ))

# Set breaks manually
breaks <- c(seq(0, 100, by = 5), Inf)

# Save plot
p1 <- ggplot(data= data_sub, aes(x=smokePM_pred, fill = factor(bin_smokePM_pred, levels = c("0-5","5-10", "10-25", "25-40", "40+" )))) +
  geom_histogram(aes(y = after_stat(count)/nrow(data_sub)*100), breaks=breaks) +
  labs(x = expression(paste("Smoke PM"[2.5]," (", mu, g, m^-3, ")", sep="")), 
       y = "Share of smoke-days", fill = "") +
  scale_fill_manual(values = c("40+" = "#8B0000", "25-40" = "#D9534F" , "10-25" = "#CD853F", "5-10" ="#FFC107" ,"0-5" ="#FFF176"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme_minimal() +
  theme(text = element_text(size = 21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text( color = "black"), 
        axis.title = element_text(size = 21),
        legend.position = c(0.85, 0.8),
        legend.key.size = unit(.75, "lines"),
        legend.text = element_text(size = 16)
  ) 

############################################
## Panel 2: Smoke Concentration Histogram ##
############################################

# Assign year variable
data_sub$year <- year(data_sub$date)

# For each county-year, calculate the number of smoke-days in each category
plot2 <- data_sub %>% group_by(GEOID, year) %>% count(bin_smokePM_pred)
plot2 <- plot2 %>% group_by(year, bin_smokePM_pred) %>% summarise(
  avg_n_smoke_days = mean(n), total = sum(n)
)

# Save plot
p2 <- ggplot(plot2, aes(fill=factor(bin_smokePM_pred, levels = c("0-5","5-10", "10-25", "25-40", "40+" )), y=total, x=factor(year))) + 
  geom_bar(position="dodge", stat="identity") +  # Changed to dodge for side-by-side bars
  labs(x = "Year", y = "Total Number of Smoke-Days", fill = "") +
  scale_fill_manual(values = c("40+" = "#8B0000", "25-40" = "#D9534F" , "10-25" = "#CD853F", "5-10" ="#FFC107" ,"0-5" ="#FFF176"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(text = element_text(size = 21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"), 
        axis.title = element_text(size = 21),
        legend.key.size = unit(.75, "lines"),
        legend.text = element_text(size = 16),
        legend.position = c(0.85, 0.8)
  )

############################################
## Panel 3: Counts of Hospitalizations Bar Chart ##
############################################

# Load causes data
code.lookup.merged = readRDS('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/CCS_DX.rds')

# Remove unwanted categories
code.lookup.merged = subset(code.lookup.merged, !(code_chapter %in% c(
  "Complications of pregnancy; childbirth; and the puerperium", 
  "Congenital anomalies",
  "Certain conditions originating in the perinatal period",
  "Symptoms; signs; and ill-defined conditions and factors influencing health status",
  "Residual codes; unclassified; all E codes [259. and 260.]"
)))

causes_groups = unique(as.character(code.lookup.merged$code_chapter))
years <- c(2006:2016)
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/er/')

cases_mat <- matrix(NA, nrow = length(causes_groups), ncol = length(years))
for (i in 1:length(causes_groups)){
  input.file = paste0(dir.input, 'medicare_', gsub(" ", "_", causes_groups[i]), '_merged_data.RData')
  load(paste0(input.file))
  data_total <- dat.merged.multiple %>% group_by(year) %>% summarise(total_cases = sum(cases))
  cases_mat[i,] <- c(data_total[,2])$total_cases
}

cases_mat <- data.frame(cases_mat)
colnames(cases_mat) <- years 
rownames(cases_mat) <- causes_groups
cases_mat$total <- rowSums(cases_mat)

cases_mat_long <- cases_mat %>%
  tibble::rownames_to_column(var = "cause_group") %>%
  select(-total) %>%
  tidyr::pivot_longer(!cause_group, names_to = "year", values_to = "n_cases") 

# Rename causes CCS level 1 for Panel C
cases_mat_long$cause_group <- gsub('Diseases of the circulatory system', 'Cardiovascular diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Diseases of the respiratory system', 'Respiratory diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Neoplasms', 'Cancers', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Injury and poisoning', 'Injuries', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Mental illness', 'Neuropsychiatric disorders', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Diseases of the digestive system', 'Digestive system diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Diseases of the genitourinary system', 'Genitourinary diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous tissue diseases', cases_mat_long$cause_group)

# Rename other causes as "Other"
cases_mat_long$cause_group <- gsub('Congenital anomalies', 'Other', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Residual codes unclassified all E codes 259 and 260', 'Other', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Certain conditions originating in the perinatal period', 'Other', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Complications of pregnancy childbirth and the puerperium', 'Other', cases_mat_long$cause_group)
cases_mat_long$cause_group <- gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other', cases_mat_long$cause_group)


# Set colors (Reordered to match alphabetical order of facet labels)
colors <- c(
  "#a6cee3",  # Blood diseases
  "#e31a1c",  # Cancers
  "#1f78b4",  # Cardiovascular diseases
  "#b2df8a",  # Digestive system diseases
  "#fb9a99",  # Endocrine disorders
  "#fdbf6f",  # Genitourinary diseases
  "#cab2d6",  # Infectious and parasitic diseases
  "#ff7f00",  # Injuries
  "#FFD700",  # Musculoskeletal and connective tissue diseases
  "#636363",  # Nervous system diseases
  "#6a3d9a",  # Neuropsychiatric disorders
  "#33a02c",  # Respiratory diseases
  "#969696"  # Skin and subcutaneous tissue diseases
  
)
# Plot with specified colors
p3 <- cases_mat_long %>%
  group_by(cause_group) %>% summarise(total_cases = sum(n_cases)) %>%
  ggplot(aes(x = fct_rev(cause_group), y = total_cases, fill = cause_group)) +
  geom_bar(stat = "identity", width = 0.4) +  
  scale_fill_manual(values = colors) +
  labs(x = "Visit Type", y = "Unscheduled Hospitalizations", fill = "") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'),
        axis.text.y = element_text(color = 'black'),
        legend.position = "none"
  ) +
  coord_flip()

###################
## Arrange plots ##
###################

pdf("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data_description/figures/figure2_v2.pdf", paper='special', width=20, height=12)

plot_grid(
  plot_grid(p1, p3, ncol = 2, labels = c('A', 'C'), rel_widths = c(1, 1)),  # Panel C in top-right
  p2, nrow = 2, labels = c('', 'B'), label_size = 18
)

dev.off()



ggsave("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data_description/figures/figure2_v2.pdf",
       plot = plot_grid(
         plot_grid(p1, p3, ncol = 2, labels = c('A', 'C'), rel_widths = c(1, 1), label_size = 25),  
         p2, nrow = 2, labels = c('', 'B'), label_size = 25
       ),
       width = 25, height = 18, units = "in", dpi = 600
)
