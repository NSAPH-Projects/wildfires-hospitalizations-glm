## Script to make multiplot for sensitivity analyses for each cause
## Plots S1 to S13


# Load libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

# Define functions --------------------------------------------------------------------------------------------
subset_plot <- function(data, single_cause, single_color, plot_title){
  
  # Subset data
  subset <- data %>% filter(cause == single_cause)
  
  # Plot single cause
  plot <- subset %>%
    ggplot() +
    geom_line(aes(x = as.numeric(xbin), y = as.numeric(est), color = single_color)) +
    geom_ribbon(aes(x = as.numeric(xbin), ymin = as.numeric(lower), ymax = as.numeric(upper), fill = cause), alpha = 0.2) +
    geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +  
    labs(x = expression(paste("Smoke PM"[2.5], " Concentration (", mu, "g/m"^3, ")")),
         y = "Change in Unscheduled \nHospitalizations per 100k") +
    ggtitle(plot_title) +  # Add title
    scale_color_manual(values = single_color) +
    scale_fill_manual(values = single_color) +
    theme_bw() + 
    theme(text = element_text(size = 18),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 0, size = 20),
          axis.text.y = element_text(size = 20),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center and bold title
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "#4D4D4D", linetype = "dotted"), # Add vertical gridlines
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black"),
          strip.background = element_blank(),
          legend.position = "none")
  
  return(plot)
}

subset_plot_bin <- function(data, single_cause, single_color, plot_title){
  
  # Subset data
  subset <- data %>% filter(cause == single_cause)
  
  # Factors for bin
  subset$bin <- factor(c("0-5", "5-10", "10-25", "25-40", "40+"), 
                       levels = c("0-5", "5-10", "10-25", "25-40", "40+"))
  
  # Plot single cause
  plot <- subset %>%
    ggplot(aes(x = bin, y = as.numeric(est))) +
    geom_point(aes(color = single_color), size = 2, shape = 16) +
    geom_errorbar(aes(ymax = as.numeric(upper), ymin = as.numeric(lower), color = single_color), width = .2, size = 0.5) +
    geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +  
    labs(x = expression(paste("Smoke PM"[2.5], " Concentration (", mu, "g/m"^3, ")")),
         y = "Change in Unscheduled \nHospitalizations per 100k") +
    ggtitle(plot_title) +  # Add title
    scale_color_manual(values = single_color) +
    scale_fill_manual(values = single_color) +
    theme_bw() + 
    theme(text = element_text(size = 18),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 0, size = 20),
          axis.text.y = element_text(size = 20),
          panel.grid.major.x = element_line(color = "#4D4D4D", linetype = "dotted"), # Add vertical gridlines
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center and bold title
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black"),
          strip.background = element_blank(),
          legend.position = "none")
  
  return(plot)
}




# Load data --------------------------------------------------------------------------------------------------
results_dir <- "/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/"

# Main (non-extreme subset, knots  10,25,40)
load(paste0(results_dir, "feols_splines/knts10_25_40/all_lags/clusterSE/nonExtreme_subset/results_CM_unifCI.RData"))
main <- results

# Cropped view
load(paste0(results_dir, "feols_splines/knts10_25_40/all_lags/clusterSE/nonExtreme_subset/results_CM_cropped.RData"))
main_cropped <- results

# California
load(paste0(results_dir, "feols_splines/knts10_25_40/all_lags/clusterSE/nonExtreme_subset/CA/results_CM.RData"))
CA <- results

# All data fix this
load(paste0(results_dir, "feols_splines/knts10_25_40/all_lags/clusterSE/results_CM.RData"))
kts_10_25_40 <- results

# All data full
load(paste0(results_dir, "feols_splines/knts10_25_40/all_lags/clusterSE/results_CM_full.RData"))
kts_10_25_40_full <- results

# Non-extreme subset, knots 5, 10, 25
load(paste0(results_dir, "feols_splines/knts5_10_25/all_lags/clusterSE/nonExtreme_subset/results_CM_unifCI.RData"))
kts_5_10_25 <- results

# Non-extreme subset, knots 5, 20, 35
load(paste0(results_dir, "feols_splines/knts5_20_35/all_lags/clusterSE/nonExtreme_subset/results_CM_unifCI.RData"))
kts_5_20_35 <- results

# Non-extreme subset, knots 5, 10, 25, 40
load(paste0(results_dir, "feols_splines/knts5_10_25_40/all_lags/clusterSE/nonExtreme_subset/results_CM.RData"))
kts_5_10_25_40 <- results

# Main Model adjusting for SES
#load("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feols_splines/knts10_25_40/all_lags/clusterSE/nonExtreme_subset/results_CM_unifCI_cov.RData")
#cov <- results

# Main Model adjusting for SES
load("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feols_splines/knts10_25_40/all_lags/clusterSE/nonExtreme_subset/may_dec/results_CM.RData")
may_dec <- results

# Poisson Model adjusting for SES
load("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feglm_splines/knts10_25_40/all_lags/clusterSE/results_CM.RData")
pois <- results

# Binned non-extreme
load(paste0(results_dir, "feols_binned/all_lags/nonExtreme_subset/binned_results_CM.RData"))
binned_nonExtreme <- results

# Binned full Note not updated model
load(paste0(results_dir, "feols_binned/all_lags/binned_results_CM.RData"))
binned_full <- results

# Plot -------------------------------------------------------------------------------------------------------------




##########################################################################
# Load necessary library for labeling
library(cowplot)

# Define causes
causes <- c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
            'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
            'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
            'Nervous system diseases','Skin and subcutaneous tissue diseases')

# Set colors
colors <- c(
  "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
  "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
  "#FFD700", "#636363", "#969696", "#FFD700"  # Adding darker yellow
)

# Define facet titles
facet_titles <- c("(A) Main Model", "(B) Main Model Cropped", "(C) California Model",
                  "(D) Full Data Model Cropped (Knots 10,25,40)", "(E) Full Data Model (Knots 10,25,40)",
                  "(F) Main Model (Knots 5,10,25)", "(G) Main Model (Knots 5,20,35)", "(H) Main Model (Knots 5,10,25,40)", 
                  "(I) Study Period May-December", "(J) Poisson GLM Model ", 
                  "(K) Binned (Non-Extreme)", "(L) Binned (Full)")

for(i in 1:length(causes)){
  plot_main <- subset_plot(main, causes[i], colors[i], facet_titles[1])
  plot_main_cropped <- subset_plot(main_cropped, causes[i], colors[i], facet_titles[2])
  plot_CA <- subset_plot(CA, causes[i], colors[i], facet_titles[3])
  plot_kts_10_25_40 <- subset_plot(kts_10_25_40, causes[i], colors[i], facet_titles[4])
  plot_kts_10_25_40_full <- subset_plot(kts_10_25_40_full, causes[i], colors[i], facet_titles[5])
  plot_kts_5_10_25 <- subset_plot(kts_5_10_25, causes[i], colors[i], facet_titles[6])
  plot_kts_5_20_35 <- subset_plot(kts_5_20_35, causes[i], colors[i], facet_titles[7])
  plot_kts_5_10_25_40 <- subset_plot(kts_5_10_25_40, causes[i], colors[i], facet_titles[8])
  #plot_cov <- subset_plot(cov, causes[i], colors[i], facet_titles[9])
  plot_may_dec <- subset_plot(may_dec, causes[i], colors[i], facet_titles[9])
  plot_pois <- subset_plot(pois, causes[i], colors[i], facet_titles[10])
  plot_binned_nonExtreme <- subset_plot_bin(binned_nonExtreme, causes[i], colors[i], facet_titles[11])
  plot_binned_full <- subset_plot_bin(binned_full, causes[i], colors[i], facet_titles[12])
  
  # Save plot
  pdf(paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/figures/SA/",causes[i],".pdf"),
      paper = 'special', width = 25, height = 20)
  
  ## Combine plots into four rows and three columns
  final_plot <- plot_grid(
    plot_main, plot_main_cropped, plot_CA,            # Row 1
    plot_kts_10_25_40, plot_kts_10_25_40_full, plot_kts_5_10_25,  # Row 2
    plot_kts_5_20_35, plot_kts_5_10_25_40, plot_may_dec,  # Row 3
    plot_pois, plot_binned_nonExtreme, plot_binned_full,  # Row 4
    nrow = 4, ncol = 3,  # 4 rows, 3 columns
    #labels = c('A', 'B', 'C', 
     #          'D', 'E', 'F', 
     #          'G', 'H', 'I', 
     #          'J', 'K', 'L'),  # Labels for each subplot
    label_size = 18
  )
  
  
  print(final_plot)
  
  dev.off()
}
