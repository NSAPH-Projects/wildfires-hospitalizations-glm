# This plot takes the level 3 sub-causes used in the paper and creates a
# barchart, faceted by level 1 cause, showing what % of the total hospitalizations 
# for that cause that each of the analyzed sub-causes accounts for

library(dplyr)
library(ggplot2)
library(tidyverse)

###############################################
## Get total hospitalizations level 1 causes ##
###############################################
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
# Load exposure data
load("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data/exp_data_2006_2016.RData")
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

# Summarize over year
denom <- cases_mat_long %>% group_by(cause_group) %>% summarise(total_cases = sum(n_cases))

#######################################################
## Get total hospitalizations for level 3 sub-causes ##
########################################################


# Load CCS level 1 and 3 names
code.lookup.merged = read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL
code.lookup.merged = unique(code.lookup.merged[,c('ccs_level_3','ccs_level_3_description','ccs_level_1','ccs_level_1_description')])
code.lookup.merged$ccs_level_3_description = as.character(code.lookup.merged$ccs_level_3_description)
code.lookup.merged$ccs_level_1_description = as.character(code.lookup.merged$ccs_level_1_description)

# Seeds of interest
# For this paper, this refers to the level 3 subcauses within genitourinary,
# cardiovascular, respiratory, and digestive system diseases that have > 25000
# hospitalizations.
seeds <- c(157,109,100,129,128,149,106,127,108,101,146,153,99,135,145,102,117,
           155,152,114,118,122,103,131,112,159)

# Subset to level 3 of interest
code_subset = code.lookup.merged %>% filter(ccs_level_3 %in% seeds)
ccs_level_3 = unique(as.character(code_subset$ccs_level_3_description))
ccs_level_3 = trimws(ccs_level_3) ; ccs_level_3 = gsub("/", "_", ccs_level_3); ccs_level_3 = gsub(" ", "_", ccs_level_3)

# Initialize vector to save the sum of the sub-causes
res <- c()

# Load data
for(i in ccs_level_3){
  
  # Load data
  load(paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/er/level3/medicare_",gsub(" ", "_", i),"_merged_data.RData"))
  
  # Add the sum of the cases to the dataframe
  res[i] <- sum(dat.merged.multiple$cases) # In order of ccs_level_3
}

######################################
## Organize data frame for plotting ##
######################################

# Rename causes CCS level 1
code_subset$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory diseases', code_subset$ccs_level_1_description)

code_subset$ccs_level_1_description = gsub('Neoplasms', 'Cancers', code_subset$ccs_level_1_description)

code_subset$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Mental Illness', 'Neuropsychiatric disorders', code_subset$ccs_level_1_description)

code_subset$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system diseases', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases', code_subset$ccs_level_1_description)
code_subset$ccs_level_1_description = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous tissue diseases', code_subset$ccs_level_1_description)

# Join the total level 1 case counts to code_subset
code_subset <- code_subset %>% left_join(denom, by= c("ccs_level_1_description" = "cause_group"))

# Calculate percent of level 3 sub-causes in each level 1 cause
code_subset$perc_cases <- res/code_subset$total_cases * 100
code_subset$num_hosp <- res

# Re-name level 3 sub-causes
code_subset$ccs_level_1_description[which(code_subset$ccs_level_1_description == "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)")] <- "Pneumonia"
code_subset$ccs_level_1_description[which(code_subset$ccs_level_1_description == "Respiratory failure; insufficiency; arrest (adult)")] <- "Respiratory failure"
code_subset$ccs_level_1_description[which(code_subset$ccs_level_1_description == "Peri-; endo-; and myocarditis; cardiomyopathy (except that caused by tuberculosis or sexually transmitted disease)")] <- "Peri-; endo-; and myocarditis; cardiomyopathy"

##########
## Plot ##
##########
colors <- c("#1f78b4", "#33a02c", "#b2df8a", "#fdbf6f")

# Save plot
pdf("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data_description/figures/level3_barchart.pdf",paper='special',width=15,height=13)

# Create the plot
ggplot(code_subset, aes(x = str_wrap(ccs_level_3_description, width = 20), y = perc_cases,color = ccs_level_1_description)) +
  geom_bar(stat = "identity", aes(fill = ccs_level_1_description)) +
  facet_wrap(. ~ ccs_level_1_description, scales = "free") + 
  labs(x = "Level 3 Sub-Cause", y = "Percent of Level 3 Sub-Cause Unscheduled Hospitalizations Contributed to Level 1 Cause") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(text = element_text(size = 12),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90, vjust = .5, hjust=1), axis.text.y = element_text(size=6),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = "none")

dev.off()

####################################################################################################

##############################################
## Plot just cardiovascular and respiratory ##
##############################################
colors <- c("#1f78b4", "#33a02c", "#b2df8a", "#fdbf6f")

# Save plot
pdf("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data_description/figures/level3_barchart_subset.pdf",paper='special',width=15,height=13)

code_subset <- code_subset %>% filter(ccs_level_1 %in% c(7,8))
# Create the plot
ggplot(code_subset, aes(x = str_wrap(ccs_level_3_description, width = 20), y = perc_cases,color = ccs_level_1_description)) +
  geom_bar(stat = "identity", aes(fill = ccs_level_1_description)) +
  facet_wrap(. ~ ccs_level_1_description, scales = "free") + 
  labs(x = "Level 3 Sub-Cause", y = "Percent of Level 3 Sub-Cause Unscheduled Hospitalizations Contributed to Level 1 Cause") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(text = element_text(size = 12),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90, vjust = .5, hjust=1),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = "none")
dev.off()

###############
## Version 2 ##
###############
# This version rotates the plot
# Save plot
pdf("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data_description/figures/level3_barchart_subset_v2.pdf",paper='special',width=19,height=14)

# Create the plot
ggplot(code_subset, aes(x = fct_rev(str_wrap(ccs_level_3_description, width = 22)), y = perc_cases,color = ccs_level_1_description)) +
  geom_bar(stat = "identity", aes(fill = ccs_level_1_description)) +
  facet_wrap(. ~ ccs_level_1_description, scales = "free") + 
  labs(x = "Level 3 Sub-Cause", y = "Percent of Level 3 Sub-Cause Unscheduled Hospitalizations Contributed to Level 1 Cause") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(text = element_text(size = 26),panel.grid.major = element_blank(),axis.text.x = element_text( size = 23,color = "black"),axis.text.y = element_text( size = 20, color = "black"),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = "none") +
        coord_flip()
dev.off()


###################################################
## Plot in final version of the paper supplement ##
###################################################
# This version rotates the plot
# Save plot
code_subset <- code_subset %>% filter(ccs_level_1 %in% c(8))
colors <- c("#33a02c")

pdf("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/data_description/figures/level3_barchart_resp_v2.pdf",paper='special',width=19,height=14)

# Create the plot
ggplot(code_subset, aes(x = fct_rev(str_wrap(ccs_level_3_description, width = 22)), y = perc_cases,color = ccs_level_1_description)) +
  geom_bar(stat = "identity", aes(fill = ccs_level_1_description)) +
  facet_wrap(. ~ ccs_level_1_description, scales = "free") + 
  labs(x = "Level 3 Sub-Cause", y = "Percent of Level 3 Sub-Cause Unscheduled Hospitalizations Contributed to Level 1 Cause") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(text = element_text(size = 26),panel.grid.major = element_blank(),axis.text.x = element_text( size = 23,color = "black"),axis.text.y = element_text( size = 20, color = "black"),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = "none") +
  coord_flip()
dev.off()


# Create clean summary table
table_level3 <- code_subset %>%
  select(ccs_level_3_description, num_hosp, perc_cases) %>%
  View()
 

