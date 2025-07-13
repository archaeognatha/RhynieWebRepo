# Load necessary libraries
library(tidyverse)

# Read and inspect data
df <- read_csv("TrophospeciesMetrics.csv")

# label webs as Devonian or Modern
df <- df %>%
  mutate(Period = ifelse(startsWith(Analysis, "Rhynie"),"devonian","modern")) %>%
  mutate(Period = as.factor(Period))

### CODE FOR PERMUTATION ANALYSIS b/w Rhynie_TS and both modern webs
metrics_results <- data.frame(Metric = character(), RhynieModernDiff = numeric(), normalizedDiff = numeric(), PVal = numeric(), ProportionWithin95ofModern = numeric())
# Subset data
df_rhynie <- df %>% filter(Analysis == "Rhynie_all_TS")
df_modern <- df %>% filter(Analysis %in% c("DigelSoil_TS", "EcoWeb_TS"))

# Pick the metrics of interest, e.g., "C"
metrics = c("C", "L_D", "Mean_NTP", "Mean_NTP_norm", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "Std_path_len", "MeanInDegree", "StdInDegree", "Basal", "Herbiv", "Carniv", "Top")

for (metric in metrics){
  # Extract the values
  rhynie_vals <- df_rhynie[[metric]]
  modern_vals <- df_modern[[metric]]

  # Quantile analysis:
  quantiles_modern <- quantile(modern_vals, probs = c(0.025, 0.975), na.rm = TRUE)
  within_band <- rhynie_vals >= quantiles_modern[1] & rhynie_vals <= quantiles_modern[2]
  cat("Proportion of Rhynie within 95% of modern:", mean(within_band), "\n")
  ProportionWithin95ofModern = mean(within_band)
  
  # Permutation test: sample 100,000 rhynie subsets and compute difference
    n_iter <- 100000
  
  obs_diffs <- numeric(n_iter)
  perm_diffs <- numeric(n_iter)
  
  for (i in 1:n_iter) {
    # Step 1: subsample Rhynie
    sampled_rhynie <- sample(rhynie_vals, n_modern)
    
    # Step 2: observed difference in means
    obs_diff <- mean(sampled_rhynie, na.rm = TRUE) - mean(modern_vals, na.rm = TRUE)
    obs_diffs[i] <- obs_diff
    
    # Step 3: build combined pool and shuffle
    combined <- c(sampled_rhynie, modern_vals)
    shuffled <- sample(combined) # sample permutates because no size argument provided
    
    # Step 4: reassign into two new groups
    group1 <- shuffled[1:n_modern]
    group2 <- shuffled[(n_modern + 1):(2 * n_modern)]
    
    # Step 5: null difference
    perm_diffs[i] <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
  }
  
  # Step 6: compare observed differences to null distribution
  p_val <- mean(abs(perm_diffs) >= abs(obs_diffs))
  cat("Permutation p-value for", metric, ":", p_val, "\n")
  cat("Mean observed difference", metric, ":", mean(obs_diffs), "\n")
  
  # push results to dataframe
  metrics_results[nrow(metrics_results)+1,] = c(metric, mean(obs_diffs), mean(obs_diffs)/mean(modern_vals), p_val, ProportionWithin95ofModern) 
}

write.csv(metrics_results, "permutationtest_RhynieTSvsModern.csv", row.names = FALSE)

### CODE FOR PERMUTATION ANALYSIS b/w Rhynie_TS and Rhynie_lumped

metrics_results <- data.frame(Metric = character(), RhynieModernDiff = numeric(), normalizedDiff = numeric(), PVal = numeric())
# Subset data
df_TS <- df %>% filter(Analysis == "Rhynie_all_TS")
df_lumped <- df %>% filter(Analysis == "Rhynie_all_lumped")

# Pick the metrics of interest, e.g., "C"
metrics = c("C", "L_D", "Mean_NTP", "Mean_NTP_norm", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "Std_path_len", "MeanInDegree", "StdInDegree", "Basal", "Herbiv", "Carniv", "Top")

for (metric in metrics){
  # Extract the values
  TS_vals <- df_TS[[metric]]
  lumped_vals <- df_lumped[[metric]]

  
  # Permutation test: sample 10,000 rhynie subsets and compute difference
  n_iter <- 10000
  
  obs_diffs <- numeric(n_iter)
  perm_diffs <- numeric(n_iter)
  
  for (i in 1:n_iter) {
    # observed difference in means
    obs_diff <- mean(lumped_vals, na.rm = TRUE) - mean(TS_vals, na.rm = TRUE)
    obs_diffs[i] <- obs_diff
    
    # Step 3: build combined pool and shuffle
    combined <- c(lumped_vals, TS_vals)
    shuffled <- sample(combined) # sample permutates because no size argument provided
    
    # Step 4: reassign into two new groups
    group1 <- shuffled[1:length(lumped_vals)]
    group2 <- shuffled[(length(lumped_vals) + 1):length(combined)]
    
    # Step 5: null difference
    perm_diffs[i] <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
  }
  
  # Step 6: compare observed differences to null distribution
  p_val <- mean(abs(perm_diffs) >= abs(obs_diffs))
  cat("Permutation p-value for", metric, ":", p_val, "\n")
  cat("Mean observed difference", metric, ":", mean(obs_diffs), "\n")
  
  # push results to dataframe
  metrics_results[nrow(metrics_results)+1,] = c(metric, mean(obs_diffs), mean(obs_diffs)/mean(modern_vals), p_val) 
}
write.csv(metrics_results, "permutationtest_RhynieTSvsRhynieLumped.csv", row.names = FALSE)

# Optional: plot
hist(perm_diffs, breaks = 40, main = paste("Permutation Test for", metric),
     xlab = "Difference in means (Rhynie - Modern)")
abline(v = mean(obs_diffs), col = "red", lwd = 2)



### CODE FOR PERMUTATION BASED MANOVA

# Load vegan for adonis
library(vegan)

# Again, create group variable
df <- df_test %>%
  filter(Analysis %in% c("Rhynie_all_TS", "DigelSoil", "EcoWeb")) %>%
  
# Select variables for MANOVA
metrics = c("C", "L_D", "Mean_NTP", "Mean_NTP_norm", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "Std_path_len", "MeanInDegree", "StdInDegree", "Basal", "Herbiv", "Carniv", "Top")

# Remove any rows with missing values in these metrics
df_clean <- df %>%
  select(Group, all_of(metrics)) %>%
  na.omit()

# Run permutation MANOVA
adonis_result <- adonis(df_clean[, metrics] ~ Group, data = df_clean, permutations = 999)

# Output
print(adonis_result)


### Statistical comparisons of samples

# subset dataframe for comparing full (Trophic Species) rhynie to modern webs
df_unlumped <- df %>%
  filter(!startsWith(Analysis, "Rhynie")|(startsWith(Analysis, "Rhynie_all")&!endsWith(Analysis,"lumped"))) %>%
  mutate(Analysis = as.factor(Analysis)) 

# subset dataframe for comparing lumped rhynie to modern webs
df_lumped <- df %>%
  filter(!startsWith(Analysis, "Rhynie")|Analysis == "Rhynie_all_lumped") %>%
  mutate(Analysis = as.factor(Analysis)) 

# subset dataframe for comparing rhynie web to terrestrial and aquatic subsets (not lumped)
df_habitats <- df %>%
  filter(startsWith(Analysis, "Rhynie")&!endsWith(Analysis,"lumped")) %>%
  mutate(Analysis = as.factor(Analysis)) 

# subset dataframe for comparing full and lumped rhynie webs (complete)
df_rhynielumping <- df %>%
  filter(startsWith(Analysis, "Rhynie_all")) %>%
  mutate(Analysis = as.factor(Analysis)) 

empirical_pvals <- sapply(rhynie_vals, function(x) mean(modern_vals <= x))
hist(empirical_pvals, main = "Empirical p-values for Rhynie Webs", xlab = "Empirical p-value")

