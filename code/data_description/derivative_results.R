# This script applies a test of nonlinearity to each cause of hospitalization

library(tidyverse)

load("/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/model_run/results/feols_splines/knts10_25_40/all_lags/clusterSE/nonExtreme_subset/derivative_results.RData")

test_nonlinearity <- function(df) {
  non_linear_pairs <- 0
  # Create all possible pairs of exposure points
  for (i in 1:(nrow(df) - 1)) {
    for (j in (i + 1):nrow(df)) {
      
      # Check if confidence intervals do not overlap
      if (df$upper[i] < df$lower[j] | df$lower[i] > df$upper[j]) {
        non_linear_pairs <- non_linear_pairs + 1
      }
    }
  }
  return(non_linear_pairs)  # Return the count of non-overlapping pairs
}

# Apply test to each cause
nonlinearity_results <- derivative_results %>%
  group_by(facet) %>%
  summarise(non_linear_pairs = test_nonlinearity(cur_data()),
            total_pairs = choose(n(), 2),  # Total number of possible pairs
            proportion_non_linear = non_linear_pairs / total_pairs) %>% 
  arrange(desc(non_linear_pairs))
# Print results
print(nonlinearity_results)
