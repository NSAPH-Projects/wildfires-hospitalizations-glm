library(ggplot2)
library(tidyverse)

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

kts=c(10,25,40)

## Load poisson results
results_df <- data.frame() # initialize data frame
for(i in 1:13){
  cause_group = causes_groups[i]
  
  results_dir <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feglm_splines/knts",kts[1],"_",kts[2],"_",kts[3],"/all_lags/clusterSE/")
  load(paste0(results_dir,'results_CM_',cause_group,'.RData'))
  
  results_df <- rbind(results_df,results)
}

###################################
## 5. Process Results for ggplot ##
###################################
results <- results_df
colnames(results)[4] <- "SE"
# Calculate confidence intervals
results$lower <- as.numeric(results$est) - 1.96 * as.numeric(results$SE)
results$upper <- as.numeric(results$est) + 1.96 * as.numeric(results$SE)

# Rename causes CCS level 1
results$cause = gsub('Diseases of the circulatory system', 'Cardiovascular diseases', results$cause)
results$cause = gsub('Diseases of the respiratory system', 'Respiratory diseases', results$cause)

results$cause = gsub('Neoplasms', 'Cancers', results$cause)

results$cause = gsub('Injury and poisoning', 'Injuries', results$cause)
results$cause = gsub('Mental illness', 'Neuropsychiatric disorders', results$cause)

results$cause = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', results$cause)
results$cause = gsub('Diseases of the digestive system', 'Digestive system diseases', results$cause)
results$cause = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', results$cause)
results$cause = gsub('Diseases of the genitourinary system', 'Genitourinary diseases', results$cause)
results$cause = gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases', results$cause)
results$cause = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases', results$cause)
results$cause = gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases', results$cause)
results$cause = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous tissue diseases', results$cause)

results$cause = gsub('Congenital anomalies', 'Other', results$cause)
results$cause = gsub('Residual codes unclassified all E codes 259 and 260', 'Other', results$cause)
results$cause = gsub('Certain conditions originating in the perinatal period', 'Other', results$cause)
results$cause = gsub('Complications of pregnancy childbirth and the puerperium', 'Other', results$cause)
results$cause = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other', results$cause)

# reorder CCS level 1 causes for plotting
results$cause = factor(results$cause,
                       levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                'Nervous system diseases','Skin and subcutaneous tissue diseases'))


#####################
## 6. Save results ##
######################

# Save results
results_dir <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feglm_splines/knts",kts[1],"_",kts[2],"_",kts[3],"/all_lags/clusterSE/")
ifelse(!dir.exists(results_dir), dir.create(results_dir, recursive=TRUE), FALSE)
save(results, file = paste0(results_dir,'results_CM.RData'))
#####################
## 7. Plot results ##
#####################

# Set colors
colors <- c(
  "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
  "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
  "#FFD700", "#636363", "#969696", "#FFD700"  # Adding darker yellow
)


# Plot results
plot_dir <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/figures/feglm_splines/knts",kts[1],"_",kts[2],"_",kts[3],"/all_lags/clusterSE/nonExtreme_subset/")
ifelse(!dir.exists(plot_dir), dir.create(plot_dir, recursive=TRUE), FALSE)

pdf(paste0(plot_dir,'results_CM_temp.pdf'),paper='special',width=15,height=20)

# Plot
# Plot
# Combine the original data frame with the new "All Causes" data frame
CreateAllFacet <- function(df, col){
  df$facet <- df[[col]]
  temp <- df
  temp$facet <- "All"
  merged <-rbind(temp, df)
  
  # ensure the facet value is a factor
  merged[[col]] <- as.factor(merged[[col]])
  
  return(merged)
}

combined_results <- CreateAllFacet(results,"cause")

# To avoid having CIs for the "All" panel, set lower and upper = est
combined_results[which(combined_results$facet=="All"),]$upper <- as.numeric(combined_results[which(combined_results$facet=="All"),]$est)
combined_results[which(combined_results$facet=="All"),]$lower <- as.numeric(combined_results[which(combined_results$facet=="All"),]$est)

# Set colors (Reordered to match alphabetical order of facet labels)
colors <- c(
  "#1f78b4",  # Blood diseases
  "#33a02c",  # Cancers
  "#e31a1c",  # Cardiovascular diseases
  "#ff7f00",  # Digestive system diseases
  "#6a3d9a",  # Endocrine disorders
  "#a6cee3",  # Genitourinary diseases
  "#b2df8a",  # Infectious and parasitic diseases
  "#fb9a99",  # Injuries
  "#fdbf6f",  # Musculoskeletal and connective tissue diseases
  "#cab2d6",  # Nervous system diseases
  "#FFD700",  # Neuropsychiatric disorders
  "#636363",  # Respiratory diseases
  "#969696",  # Skin and subcutaneous tissue diseases
  "#FFD700"   # All (Keeping it distinct)
)

# Sort facets alphabetically, keeping "All" last
facet_levels <- sort(unique(combined_results$facet[combined_results$facet != "All"]))
facet_levels <- c(facet_levels, "All")  # Ensure "All" is last

# Reorder facet variable
combined_results$facet <- factor(combined_results$facet, levels = facet_levels)

# Plot the data
combined_results %>%
  ggplot() +
  geom_line(aes(x = as.numeric(xbin), y = as.numeric(est), color = cause)) +
  geom_ribbon(aes(x = as.numeric(xbin), ymin = as.numeric(lower), ymax = as.numeric(upper), fill = cause), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +  
  labs(x = expression(paste("Smoke PM"[2.5], " Concentration (", mu, "g/m"^3, ")")),
       y = "Change in Unscheduled Hospitalizations per 100k") +
  facet_wrap(vars(facet), ncol = 2, scales = "free_y") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(text = element_text(size = 22), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_line(color = "#4D4D4D", linetype = "dotted"), # Add vertical gridlines
        axis.text.x = element_text(angle = 0),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),
        strip.background = element_blank(),
        legend.position = "none")

dev.off()






