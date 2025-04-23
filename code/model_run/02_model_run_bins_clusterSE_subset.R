# This script takes the data and fits fixed effects model
# with binned exposure variables
   
####################
## Load libraries ##
####################
library(fixest)
library(DescTools)
library(ggplot2)
library(boot)
library(data.table)
library(tidyr)
library(tibble)
library(dplyr)
library("mgcv")
library(tis)
library(lubridate)
library(splines) 
   
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

# load holidays
holidays <- federalHolidays(years, board = T, businessOnly = T)

 
###################################
## 2. Define Regression Function ##
###################################
regression_function <- function(data) {
  xvar="smokePM_pred"
  yvar="cases_rate"
  fe="year + dow + GEOID + county_month"
  xunit="GEOID"
  controls="+ ns(Tmean_C, df = 2) + holiday_indicator"

  fmla <- as.formula(paste(yvar,"~","bin_var_l0 + bin_var_l1 + bin_var_l2 + bin_var_l3 +
                   bin_var_l4 + bin_var_l5 + bin_var_l6 + bin_var_l7",
                                                   controls," |",fe))


  # Fit the feols model
  mod <- feols(fmla, data=data,weights = data$population, cluster = c("GEOID"))


  # Return SE and coef
  return(mod)
}
 
##########################################
## 3. Initialize data.frame for results ##
#########################################
results <- data.frame()

#################################################################
## 4. For each cause, run regression function and save results ##
#################################################################
for (i in 1:13){
  cause_group = causes_groups[i]

  # Load data
  er = 1
  if(er == 1){
    load(paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/er/pop_weighted_temp/medicare_",gsub(" ", "_", cause_group),"_merged_data.RData"))
    
  } else{
    load(paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/data/feols/total/pop_weighted_temp/medicare_",gsub(" ", "_", cause_group),"_merged_data.RData"))
  }
  
  
  # Create rate variable
  dat.merged.multiple$cases_rate <- dat.merged.multiple$cases/dat.merged.multiple$population * 100000
  
  # Assess the quantiles of smoke PM
  quantile(dat.merged.multiple$smokePM_pred, .999)
  
  # Subset data to non-extreme values
  dat.merged.multiple <-dat.merged.multiple %>% filter(smokePM_pred < ceiling(quantile(smokePM_pred,.999)))
  
  # Add holidays
  # Convert "date" column to the same format as holidays
  dat.merged.multiple$date_numeric <- as.numeric(format(as.Date(dat.merged.multiple$date), "%Y%m%d"))
  
  # Convert holidays vector to numeric format
  holiday_dates <- as.numeric(holidays)
  
  # Create binary holiday indicator
  dat.merged.multiple$holiday_indicator <- ifelse(dat.merged.multiple$date_numeric %in% holiday_dates, 1, 0)
  
  # Remove intermediate column if not needed
  dat.merged.multiple$date_numeric <- NULL
  
  dat.merged.multiple <- dat.merged.multiple %>% filter(!is.na(population))
  
  # Convert to data.table
  pdata <- as.data.table(dat.merged.multiple, ~GEOID + date)
  
  # Define lag days
  lgs=0:7
  
  # Set knots
  #kts=c(10,25,40)
  
  # finds lags from a week prior for all the exposure variables
  anscols = paste0("smokePM_l",lgs)
  pdata[, (anscols) := data.table::shift(.SD, lgs, fill=NA, "lag"), .SDcols="smokePM_pred", by=GEOID]
  
  # Create a county-month fixed effect
  pdata$county_month <- factor(paste(pdata$GEOID, pdata$month, sep = "_"))

  
  # Remove the first week of observations. These won't have lagged days
  pdata <- pdata %>% drop_na(smokePM_l1,smokePM_l2,smokePM_l3,smokePM_l4,smokePM_l5,smokePM_l6,smokePM_l7)
  

  # Create binned dummy variables
  # For each lagged day (0-7) there will be a dummy variable
  # Binning continuous variable into categories
  pdata$bin_var_l0 <- cut(pdata$smokePM_l0, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed
  pdata$bin_var_l1 <- cut(pdata$smokePM_l1, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed
  pdata$bin_var_l2 <- cut(pdata$smokePM_l2, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed
  pdata$bin_var_l3 <- cut(pdata$smokePM_l3, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed
  pdata$bin_var_l4 <- cut(pdata$smokePM_l4, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed
  pdata$bin_var_l5 <- cut(pdata$smokePM_l5, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed
  pdata$bin_var_l6 <- cut(pdata$smokePM_l6, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed
  pdata$bin_var_l7 <- cut(pdata$smokePM_l7, breaks = c(-Inf,0,5,10,25,40,Inf), 
                                     labels = c("0", "0-5", "5-10", "10-25", "25-40", "40+" ),  include.lowest = FALSE, na.rm = FALSE )  # Adjust breaks as needed


  # Run regression
  reg_results <- regression_function(pdata)
  
  

  ########################
  ## Predict new values ##
  ########################
  
  
  # remove tmean from vector of coefficients from model
  b <- data.frame(coef(reg_results))[1:(length(coef(reg_results))-3),]
  
  # create the new data matrix for each bin
  new_data_df <- matrix(0,nrow=5,ncol=length(b))
  colnames(new_data_df) <- names(coef(reg_results))[1:length(b)]
  rownames(new_data_df) <- c( "0-5", "5-10", "10-25", "25-40", "40+" )
  for(i in 1:40){
    row <- rep(1:5,8)
    new_data_df[row[i],i] <- 1 #5 x 40
  }

  
  # evaluate at x for all integers 0 through 50 
  response_curve_b = t(t(as.matrix(b))%*%t(new_data_df)) # 51 x 1
  
  # get variance covariance matrix from model and remove tmean 40 x 40
  model_vcov <- vcov(reg_results)[1:(length(coef(reg_results))-3),1:(length(coef(reg_results))-3)] #removes tmean
  
  # get predicted SEs
  se.fit <- sqrt(diag(as.matrix(new_data_df) %*% model_vcov %*% as.matrix(t(new_data_df))))
  
  # function for mvn random numbers
  rmvn <- function(n, mu, sig) { ## MVN random deviates
    L <- mroot(sig)
    m <- ncol(L)
    t(mu + L %*% matrix(rnorm(m*n), m, n))
  }
  
  # specify the number of simulations to generate
  set.seed(42)
  N <- 10000
  
  BUdiff <- rmvn(N, mu = rep(0, nrow(model_vcov)), sig = model_vcov)
  BUdiff <- as.matrix(BUdiff)
  BUdiff <- apply(BUdiff, 2, as.numeric)  # Ensures all columns are numeric
  
  # Obtain linear predictor matrix at new data points
  Cg <- new_data_df
  Cg  <- apply(Cg , 2, as.numeric)  # Ensures all columns are numeric
  
  # predictions * BUdiff
  simDev <- Cg %*% t(BUdiff)
  
  # absolute values of the standardized deviations from the true model
  se.fit[se.fit == 0] <- 1e-6  # Prevent division by zero
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  
  # maximum of the absolute standardized deviations 
  masd <- apply(absDev, 2L, max)
  
  # find critical value
  crit <- quantile(masd, prob = 0.95, type = 8)
  
  # Save results
  results <- rbind(results, cbind(cause = cause_group, 
                                  bin = bin, 
                                  est = response_curve_b, SE = se.fit, uprP = response_curve_b + (2 * se.fit),
                                  lwrP = response_curve_b - (2 * se.fit),
                                  uprS = response_curve_b + (crit * se.fit),
                                  lwrS = response_curve_b - (crit * se.fit)))
}

   
###################################
## 5. Process Results for ggplot ##
###################################

colnames(results) <- c("cause ","xbin","est", "SE", "uprP", "lwrP", "uprS","lwrS")

# Calculate confidence intervals
results$lower <- results$lwrS
results$upper <- results$uprS


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
results_dir <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feols_binned/all_lags/nonExtreme_subset/")
ifelse(!dir.exists(results_dir), dir.create(results_dir, recursive=TRUE), FALSE)
save(results, file = paste0(results_dir,'binned_results_CM.RData'))

#####################
## 7. Plot results ##
#####################

# Set colors
colors <- c(
  "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
  "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
  "#FFD700", "#636363", "#969696", "#FFD700"  # Adding darker yellow
)

results$bin <- factor(c("0-5", "5-10", "10-25", "25-40", "40+"), levels = c("0-5", "5-10", "10-25", "25-40", "40+") )

# Plot results
plot_dir <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/figures/feols_binned/all_lags/nonExtreme_subset/")
ifelse(!dir.exists(plot_dir), dir.create(plot_dir, recursive=TRUE), FALSE)

pdf(paste0(plot_dir,'binned_results.pdf'),paper='special',width=15,height=20)

# Plot
ggplot(results, aes(x = bin, y = as.numeric(est))) +
  geom_point(aes(color = cause),size=2,shape=16) +
  geom_errorbar(aes(ymax=upper,ymin=lower, color = cause),width=.2,size=0.5) +
  geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +
  facet_wrap(~cause, scales = "free_y",ncol=2) +
  xlab('Bin') + ylab('Change in Unscheduled Hospitalizations per 100k') +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  #scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  # ggtitle('Additional deaths by types of intentional injuries') +
  theme_bw() + theme(text = element_text(size = 22),
                     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
                     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                     legend.position = 'none',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

dev.off()

