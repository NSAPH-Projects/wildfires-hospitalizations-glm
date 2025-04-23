# This script takes the data and fits non-linear fixed effects models

# This script follows the approach from:
# https://github.com/sheftneal/pnas-ed-smoke-2023/blob/main/scripts/2-analysis-main-figs/figure02.R

# arguments from Rscript
# should include lag number + one exposure day (lag_num), path to exposure data (exp_data_path)
args<-commandArgs(TRUE)
for (i in 1:length(args)) { eval (parse (text = args[[i]] )) }

print(kts)


####################
## Load libraries ##
####################

library(fixest)
library(DescTools)
library(ggplot2)
library(boot)
library(data.table)
library(dplyr)
library(tidyr)
library(splines)
library("mgcv")
library(tis)
library(lubridate)

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
regression_function <- function(data, kts) {
  xvar="smokePM_pred"
  yvar="cases"
  fe="year + dow + GEOID + county_month"
  xunit="GEOID"
  controls="+ ns(Tmean_C, df = 2) + holiday_indicator" 
  #kts=c(10,25,40)
  
  fmla <- as.formula(paste(yvar, "~", paste0("ns(smokePM_l",0:7,",knots=c(", 
                                             paste(as.character(kts),collapse = ","),")) ",
                                             collapse = "+"),
                           controls, " |", fe, " + offset(logpop)"))
  
  # Fit the model
  mod <- feglm(fmla, data = data, 
               weights = data$population, cluster = c("GEOID"),
               family = quasipoisson())
  
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
#for (i in 1:13){
i <- seedVal
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

# Subset data to non-extreme values
dat.merged.multiple <-dat.merged.multiple %>% filter(smokePM_pred < ceiling(quantile(smokePM_pred,.999)))
print(nrow(dat.merged.multiple ))

# Assess the quantiles of smoke PM
quantile(dat.merged.multiple$smokePM_pred, .999)

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

# Run regression
reg_results <- regression_function(pdata, kts = kts)

########################
## Predict new values ##
########################



# Get all unique county-days (WITH date and dow)
unique_county_days <- pdata %>%
  select(GEOID, date, year, month, dow, county_month, population, logpop, Tmean_C) %>%
  distinct()

# Expand for each exposure level
xbin = 0:49

# Set boundary knots
bk = attr(ns(dat.merged.multiple$smokePM_pred,knots=kts ), "Boundary.knots")

deltagbeta_mat <- data.frame()

for (j in 1:length(xbin)){
  exp <- xbin[j]
  temp_data <- cbind(unique_county_days, exp = exp)
  
  # Apply lags to smokePM_pred
  lgs <- 0:7
  for (lag in lgs) {
    temp_data[[paste0("smokePM_l", lag)]] <- temp_data$exp
  }
  
  # Apply natural splines to each lag
  for (lag in 0:7) {
    spline_matrix <- ns(temp_data[[paste0("smokePM_l", lag)]], knots = kts, Boundary.knots = bk)
    
    # Rename columns to match what was used in the model
    colnames(spline_matrix) <- paste0("ns_smokePM_l", lag, "_", seq_len(ncol(spline_matrix)))
    
    # Add to temp_data
    temp_data <- cbind(temp_data, spline_matrix)
  }
  
  # Apply Splines to Tmean_C
  tmean_spline <- ns(temp_data$Tmean_C, df = 2)
  colnames(tmean_spline) <- paste0("ns_Tmean_C_", seq_len(ncol(tmean_spline)))
  temp_data <- cbind(temp_data, tmean_spline)
  
  # Predict Rates at Each Exposure Level
  temp_data$predicted_rate <- predict(reg_results, newdata = temp_data, type = "response")
  
  # Remove NAs-- this is because FE that were removed from the model cant be predicted
  temp_data <- temp_data[!is.na(temp_data$predicted_rate),]
  
  ####################################
  ## Compute Standard Errors (SE) ##
  ####################################
  # Extract variance-covariance matrix (34 x 34)
  model_vcov <- vcov(reg_results) # new dim (34 by 34) 
  
  # Get beta
  beta <- as.matrix(coef(reg_results))# new dim (34 by 1) 
  
  # Initialize gradient matrix
  gradient_matrix <- matrix(0, nrow = nrow(temp_data), ncol = length(beta))
  
  if(j==1){
    zero_data <- temp_data
  }
  
  # Loop through each row (county-day) to compute deltag(β)
  for (cd in 1:nrow(temp_data)) {
    # Define x_i(w) and x_i(0)
    X_w <- t(as.matrix(temp_data[cd, 19:52])) # dim = 34 x 1
    X_0 <- t(as.matrix(zero_data[cd, 19:52])) # dim = 34 x 1
    
    # Compute exp(X'w β) and exp(X'0 β)
    exp_Xw_beta <- exp(t(X_w) %*% beta)
    exp_X0_beta <- exp(t(X_0) %*% beta)
    
    # Compute gradient for this observation
    gradient_matrix[cd, ] <- (X_w %*% exp_Xw_beta - X_0 %*% exp_X0_beta)
  }
  
  # Average across all observations to get deltag(β) (1 x p)
  gradient_avg <- t(as.matrix(colMeans(gradient_matrix))) # (1 x 34)
  
  # Variance of R(w) (scalar)
  variance_Rw <- gradient_avg %*% model_vcov %*% t(gradient_avg)
  
  # R(w) (scalar)
  Rw <- mean(temp_data$predicted_rate - zero_data$predicted_rate)
  
  
  
  # Save results
  results <- rbind(results, 
                   cbind(cause = cause_group, 
                         xbin = exp, 
                         est = Rw, 
                         SE = sqrt(variance_Rw)))
  
  
  
}  
  
# Set values for X
xbin = 0:49

# Set boundary knots
bk = attr(ns(dat.merged.multiple$smokePM_pred,knots=kts ), "Boundary.knots")
bk

# Apply splines to the exposure variables and rename properly
knot_labels <- paste0("knots = c(", paste(kts, collapse = ", "), ")") # Correct knot format

spline_list <- list()
for (lag in 0:7) {
  var_name <- paste0("smokePM_l", lag)
  
  # Apply natural splines with the same knots
  spline_terms <- as.data.frame(ns(xbin, knots = kts, Boundary.knots = bk))
  
  # Rename to match the model's expected names
  colnames(spline_terms) <- paste0("ns(", var_name, ", ", knot_labels, ")", seq_len(ncol(spline_terms)))
  
  # Store in list
  spline_list[[lag + 1]] <- spline_terms
}

# Combine all splines into a single matrix
new_data_df <- do.call(cbind, spline_list)

# remove tmean from vector of coefficients from model
b <- data.frame(coef(reg_results))[1:((length(kts)+1)*length(lgs)),]

# evaluate at x for all integers 0 through 50 
response_curve_b = t(t(as.matrix(b))%*%t(new_data_df)) # 51 x 1

# get variance covariance matrix from model and remove tmean
model_vcov <- vcov(reg_results)[1:((length(kts)+1)*length(lgs)),1:((length(kts)+1)*length(lgs))] #removes tmean

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
results <- rbind(results, cbind(cause = cause_group,  xbin = xbin, est = response_curve_b, SE = se.fit, uprP = response_curve_b + (2 * se.fit),
                                lwrP = response_curve_b - (2 * se.fit),
                                uprS = response_curve_b + (crit * se.fit),
                                lwrS = response_curve_b - (crit * se.fit)))

#####################
## 5. Save results ##
######################

# Save results
results_dir <- paste0("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feglm_splines/knts",kts[1],"_",kts[2],"_",kts[3],"/all_lags/clusterSE/")
ifelse(!dir.exists(results_dir), dir.create(results_dir, recursive=TRUE), FALSE)
save(results, file = paste0(results_dir,'results_CM_',cause_group,'_unifCI.RData'))

