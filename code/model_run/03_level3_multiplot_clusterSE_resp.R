# Script that that takes results for each diseases and plots them together in one plot

args<-commandArgs(TRUE)
for (i in 1:length(args)) { eval (parse (text = args[[i]] )) }


library(ggplot2)
library(dplyr)

# Directory to get results
#dir_output_model_summary = "/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feols_bootstrap/sep_splines/county_month/knts10_25_40/all_Weights/level3/"

# Load results
load(paste0(dir_output_model_summary,'results_CM.RData'))

# Directory to save plots
#kts <- c(10,25,40)
#plot_dir <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/figures/feols_bootstrap/sep_splines/county_month/knts",kts[1],"_",kts[2],"_",kts[3],"/all_Weights/level3/")
print(plot_dir)

# Years of the study
start_year = 2006
end_year = 2016
years=c(start_year:end_year)

# Filter to level 3 causes of interest
names(results)
unique(results$cause)
names(results) <- trimws(names(results))
results <- results %>% filter(cause %in% c("Aspiration_pneumonitis;_food_vomitus",
                                           "Asthma", "Chronic_obstructive_pulmonary_disease_and_bronchiectasis",
                                           "Pneumonia_(except_that_caused_by_tuberculosis_or_sexually_transmitted_disease)",
                                           "Respiratory_failure;_insufficiency;_arrest_(adult)"))

# Switch underscores to spaces
results$cause = gsub("_", " ", results$cause)

# Rename some causes
results$cause[which(results$cause == "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)")] <- "Pneumonia"
results$cause[which(results$cause == "Respiratory failure; insufficiency; arrest (adult)")] <- "Respiratory failure"

# Check if plotting directory exists
ifelse(!dir.exists(plot_dir), dir.create(plot_dir, recursive=TRUE), FALSE)

dim(results)
unique(results$cause)
results$cause <- factor(results$cause)


# Color for CCS level 1 cause
color <- "#33a02c"

# Different scale
pdf(paste0(plot_dir,'resp_multiplot_limit25000.pdf'),paper='special',width=15,height=10)
ggplot(results) +
  geom_line(aes(x = as.numeric(xbin), y = as.numeric(est), color = color)) +
  geom_ribbon(aes(x = as.numeric(xbin),ymin = as.numeric(lower), ymax = as.numeric(upper), fill = color), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +  
  labs(x = expression(paste("Smoke PM"[2.5], " Concentration (", mu, "g/m"^3, ")")),
       y = "Change in Unscheduled Hospitalizations per 100k") +
  facet_wrap(vars(cause),ncol=2,scales = "free_y") +
  scale_color_manual(values = color) +
  scale_fill_manual(values = color) +
  theme_bw() + 
  theme(text = element_text(size = 22),panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "#4D4D4D", linetype = "dotted"), # Add vertical gridlines
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = "none")

dev.off()

 

