# START HERE ####
# Load necessary libraries
library(tidyverse)

# Read and inspect data
df <- read_csv("TrophospeciesMetrics.csv")

# label webs as Devonian or Modern
df <- df %>%
  mutate(Period = ifelse(startsWith(Analysis, "Rhynie"),"devonian","modern")) %>%
  mutate(Period = as.factor(Period))

# PERMUTATION + OVERLAP ANALYSIS #### 
## permutation analysis b/w Rhynie, Modern webs, and Niche Model output ####

metrics_results <- data.frame(Metric = character(), RhynieModernDiff = numeric(), normalizedDiff_modern = numeric(), PVal_modern = numeric(), ProportionWithin95ofModern = numeric(), 
                              RhynieNicheDiff = numeric(), normalizedDiff_niche = numeric(), PVal_niche = numeric(), ProportionWithin95ofNiche = numeric ())
# Subset data
df_rhynie <- df %>% filter(Analysis == "Rhynie_all_lumped")
df_modern <- df %>% filter(Analysis %in% c("DigelSoil_TS", "EcoWeb_TS"))
df_niche <- df %>% filter(Analysis == "Niche_lumped")

n_modern <- length(df_modern)

# Pick the metrics of interest, e.g., "C"
metrics = c("C", "L_D", "Mean_NTP", "Mean_NTP_norm", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "Std_path_len", "MeanInDegree", "StdInDegree", "Basal", "Herbiv", "Carniv", "Top")

for (metric in metrics){
  # Extract the values
  rhynie_vals <- df_rhynie[[metric]]
  modern_vals <- df_modern[[metric]]
  niche_vals <- df_niche[[metric]]
  
  # Quantile analysis for modern webs:
  quantiles_modern <- quantile(modern_vals, probs = c(0.025, 0.975), na.rm = TRUE)
  within_band_modern <- rhynie_vals >= quantiles_modern[1] & rhynie_vals <= quantiles_modern[2]
  cat("Proportion of Rhynie within 95% of modern:", mean(within_band_modern), "\n")
  ProportionWithin95ofModern = mean(within_band_modern)
  
  # Quantile analysis for niche model:
  quantiles_niche <- quantile(niche_vals, probs = c(0.025, 0.975), na.rm = TRUE)
  within_band_niche <- rhynie_vals >= quantiles_niche[1] & rhynie_vals <= quantiles_niche[2]
  cat("Proportion of Rhynie within 95% of Niche Model:", mean(within_band_niche), "\n")
  ProportionWithin95ofNiche = mean(within_band_niche)
  
  # Permutation test: sample 100,000 rhynie subsets and compute difference
    n_iter <- 100000
  
  obs_diffs_modern <- numeric(n_iter)
  perm_diffs_modern <- numeric(n_iter)
  perm_diffs_niche <- numeric(n_iter)
  
  # observed difference in means (for niche model comparison)
  obs_diff_niche <- mean(rhynie_vals, na.rm = TRUE) - mean(niche_vals, na.rm = TRUE)
  
  for (i in 1:n_iter) {
    # Step 1: subsample Rhynie (for modern comparison)
    sampled_rhynie <- sample(rhynie_vals, n_modern)
    
    # Step 2: observed difference in means (for modern comparison)
    obs_diff_modern <- mean(sampled_rhynie, na.rm = TRUE) - mean(modern_vals, na.rm = TRUE)
    obs_diffs_modern[i] <- obs_diff_modern

    # Step 3: build combined pool and shuffle (for both comparisons)
    combined_modern <- c(sampled_rhynie, modern_vals)
    shuffled_modern <- sample(combined_modern) # sample permutates because no size argument provided
    combined_niche <- c(rhynie_vals, niche_vals)
    shuffled_niche <- sample(combined_niche) # sample permutates because no size argument provided
    
    # Step 4: reassign into two new groups
    group1_modern <- shuffled_modern[1:n_modern]
    group2_modern <- shuffled_modern[(n_modern + 1):(2 * n_modern)]
    group1_niche <- shuffled_niche[1:(length(shuffled_niche)/2)]
    group2_niche <- shuffled_niche[((length(shuffled_niche)/2) + 1):length(shuffled_niche)]
    
    # Step 5: null difference
    perm_diffs_modern[i] <- mean(group1_modern, na.rm = TRUE) - mean(group2_modern, na.rm = TRUE)
    perm_diffs_niche[i] <- mean(group1_niche, na.rm = TRUE) - mean(group2_niche, na.rm = TRUE)
}
  # Step 6: compare observed differences to null distribution
  p_val_modern <- mean(abs(perm_diffs_modern) >= abs(obs_diffs_modern))
  cat("Permutation p-value for", metric, ":", p_val_modern, "\n")
  cat("Mean observed difference", metric, ":", mean(obs_diffs_modern), "\n")
  p_val_niche <- mean(abs(perm_diffs_niche) >= abs(obs_diff_niche))
  
  # push results to dataframe
  metrics_results[nrow(metrics_results)+1,] = c(metric, mean(obs_diffs_modern), mean(obs_diffs_modern)/mean(modern_vals), p_val_modern, ProportionWithin95ofModern, 
                                                obs_diff_niche, obs_diff_niche/mean(niche_vals), p_val_niche, ProportionWithin95ofNiche) 
}

write.csv(metrics_results, "permutationtest_RhynieLumpedvsModernvsNiche.csv", row.names = FALSE)

## permutation analysis b/w Rhynie_TS and Rhynie_lumped ####

metrics_results <- data.frame(Metric = character(), RhynieModernDiff = numeric(), normalizedDiff = numeric(), PVal = numeric(), ProportionWithin95ofLumped = numeric())
# Subset data
df_TS <- df %>% filter(Analysis == "Rhynie_all_TS")
df_lumped <- df %>% filter(Analysis == "Rhynie_all_lumped")

# Pick the metrics of interest, e.g., "C"
metrics = c("C", "L_D", "Mean_NTP", "Mean_NTP_norm", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "Std_path_len", "MeanInDegree", "StdInDegree", "Basal", "Herbiv", "Carniv", "Top")

for (metric in metrics){
  # Extract the values
  TS_vals <- df_TS[[metric]]
  lumped_vals <- df_lumped[[metric]]
  
  # Quantile analysis:
  quantiles_lumped <- quantile(lumped_vals, probs = c(0.025, 0.975), na.rm = TRUE)
  within_band_lumped <- TS_vals >= quantiles_lumped[1] & TS_vals <= quantiles_lumped[2]
  cat("Proportion of complete vals within 95% of lumped:", mean(within_band_lumped), "\n")
  ProportionWithin95ofLumped = mean(within_band_lumped)
  
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
  metrics_results[nrow(metrics_results)+1,] = c(metric, mean(obs_diffs), mean(obs_diffs)/mean(modern_vals), p_val, ProportionWithin95ofLumped) 
}
write.csv(metrics_results, "permutationtest_RhynieTSvsRhynieLumped.csv", row.names = FALSE)

## permutation analysis b/w Rhynie, EcoWeb, and DigelSoil ####

metrics_results <- data.frame(Metric = character(), RhynieEcoDiff = numeric(), normalizedDiff_eco = numeric(), PVal_eco = numeric(), ProportionWithin95ofEcoWeb = numeric(), 
                              RhynieSoilDiff = numeric(), normalizedDiff_soil = numeric(), PVal_soil = numeric(), ProportionWithin95ofDigelSoil = numeric ())
# Subset data
df_rhynie <- df %>% filter(Analysis == "Rhynie_all_lumped")
df_eco <- df %>% filter(Analysis == "EcoWeb_TS")
df_soil <- df %>% filter(Analysis == "DigelSoil_TS")

n_eco <- length(df_eco)
n_soil <- length(df_soil)

# Pick the metrics of interest, e.g., "C"
metrics = c("C", "L_D", "Mean_NTP", "Mean_NTP_norm", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "Std_path_len", "MeanInDegree", "StdInDegree", "Basal", "Herbiv", "Carniv", "Top")

for (metric in metrics){
  # Extract the values
  rhynie_vals <- df_rhynie[[metric]]
  eco_vals <- df_eco[[metric]]
  soil_vals <- df_soil[[metric]]
  
  # Quantile analysis for EcoWeb webs:
  quantiles_eco <- quantile(eco_vals, probs = c(0.025, 0.975), na.rm = TRUE)
  within_band_eco <- rhynie_vals >= quantiles_eco[1] & rhynie_vals <= quantiles_eco[2]
  cat("Proportion of Rhynie within 95% of EcoWeb:", mean(within_band_eco), "\n")
  ProportionWithin95ofEcoWeb = mean(within_band_eco)
  
  # Quantile analysis for DigelSoil webs:
  quantiles_soil <- quantile(soil_vals, probs = c(0.025, 0.975), na.rm = TRUE)
  within_band_soil <- rhynie_vals >= quantiles_soil[1] & rhynie_vals <= quantiles_soil[2]
  cat("Proportion of Rhynie within 95% of DigelSoil:", mean(within_band_soil), "\n")
  ProportionWithin95ofDigelSoil = mean(within_band_soil)
  
  # Permutation test: sample 100,000 rhynie subsets and compute difference
  n_iter <- 100000
  
  obs_diffs_eco <- numeric(n_iter)
  perm_diffs_eco <- numeric(n_iter)
  obs_diffs_soil <- numeric(n_iter)
  perm_diffs_soil <- numeric(n_iter)
  
  
  for (i in 1:n_iter) {
    # Step 1: subsample Rhynie
    sampled_rhynie_eco <- sample(rhynie_vals, n_eco)
    sampled_rhynie_soil <- sample(rhynie_vals, n_soil)
    
    # Step 2: observed difference in means
    obs_diff_eco <- mean(sampled_rhynie_eco, na.rm = TRUE) - mean(eco_vals, na.rm = TRUE)
    obs_diffs_eco[i] <- obs_diff_eco
    obs_diff_soil <- mean(sampled_rhynie_soil, na.rm = TRUE) - mean(soil_vals, na.rm = TRUE)
    obs_diffs_soil[i] <- obs_diff_soil
    
    # Step 3: build combined pool and shuffle
    combined_eco <- c(sampled_rhynie_eco, eco_vals)
    shuffled_eco <- sample(combined_eco) # sample permutates because no size argument provided
    combined_soil <- c(sampled_rhynie_soil, soil_vals)
    shuffled_soil <- sample(combined_soil) # sample permutates because no size argument provided
    
    # Step 4: reassign into two new groups
    group1_eco <- shuffled_eco[1:n_eco]
    group2_eco <- shuffled_eco[(n_eco + 1):(2 * n_eco)]
    group1_soil <- shuffled_soil[1:n_soil]
    group2_soil <- shuffled_soil[(n_soil + 1):(2 * n_soil)]
    
    # Step 5: null difference
    perm_diffs_eco[i] <- mean(group1_eco, na.rm = TRUE) - mean(group2_eco, na.rm = TRUE)
    perm_diffs_soil[i] <- mean(group1_soil, na.rm = TRUE) - mean(group2_soil, na.rm = TRUE)
  }
  # Step 6: compare observed differences to null distribution
  p_val_eco <- mean(abs(perm_diffs_eco) >= abs(obs_diffs_eco))
  p_val_soil <- mean(abs(perm_diffs_soil) >= abs(obs_diff_soil))
  
  # push results to dataframe
  metrics_results[nrow(metrics_results)+1,] = c(metric, mean(obs_diffs_eco), mean(obs_diffs_eco)/mean(eco_vals), p_val_eco, ProportionWithin95ofEcoWeb, 
                                                mean(obs_diffs_soil), mean(obs_diffs_soil)/mean(soil_vals), p_val_soil, ProportionWithin95ofDigelSoil) 
}

write.csv(metrics_results, "permutationtest_RhynieLumpvsEcovsSoil.csv", row.names = FALSE)


## Optional: plot histogram of permutations ####
hist(perm_diffs, breaks = 40, main = paste("Permutation Test for", metric),
     xlab = "Difference in means (Rhynie - Modern)")
abline(v = mean(obs_diffs), col = "red", lwd = 2)


# PERMUTATION-BASED MANOVA ####

## code for PERMANOVA comparisons to niche and Rhynie subsets ####

# Load vegan for adonis
library(vegan)

# Select metrics for MANOVA
metrics = c("C", "Mean_NTP", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "MeanInDegree", "Basal", "Herbiv", "Carniv", "Top")

# Select which SLNs to include in the MANOVA 
analyses = c("Rhynie_all_lumped", "Rhynie_all_TS")

# Remove any rows with missing values and select only the chosen analyses
df_permanova <- df %>%
  filter(Analysis %in% analyses) %>% 
  group_by(Analysis) %>%
  slice_head(n = 100)

# Run PERMANOVA
## you can specify Period instead of Analysis for adonis2 to compare all modern vs Rhynie
adonis_result <- adonis2(df_permanova[, metrics] ~ as.factor(Analysis), data = df_permanova, permutations = 999)

# Output
print(adonis_result)

## code for PERMANOVA comparisons to modern webs ####

# Select metrics for MANOVA
metrics = c("C", "Mean_NTP", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_longest_chain", "Mean_path_len", "MeanInDegree", "Basal", "Herbiv", "Carniv", "Top")

group_col = "Analysis" # column that indicates which web each row belongs to (Analysis)

rhynie_label <- "Rhynie_all_lumped"
modern_label <- c("Niche_lumped")   # set to whichever modern dataset you want to compare

n_reps <- 500          # number of subsampling repeats
n_perm <- 999          # permutations for each PERMANOVA

# Subset the two groups of interest and drop rows with NA in metric columns
df_sub <- df %>%
  filter(.data[[group_col]] %in% c(rhynie_label, modern_label)) %>%
  select(all_of(c(group_col, metrics))) %>%
  na.omit()

# Count modern sample size
modern_rows_all <- which(df_sub[[group_col]] %in% modern_label)
n_mod <- length(modern_rows_all)
if(n_mod < 2) stop("Not enough modern webs to compare (n_mod < 2).")

# Standardize metrics (mean = 0, sd = 1) across the subset
df_std <- df_sub
df_std[metrics] <- scale(df_std[metrics])

# Explicit distance matrix (Euclidean on standardized metrics) for full data
dist_full <- dist(df_std[metrics], method = "euclidean")


# ---------------------------- #
# Repeated balanced PERMANOVA (subsample Rhynie each repeat) and dispersion test
# ---------------------------- #
rhynie_rows_all <- which(df_std[[group_col]] == rhynie_label)

results <- vector("list", n_reps)

bd_results <- vector("list", n_reps)

for(i in seq_len(n_reps)){
  rh_sample_rows <- sample(rhynie_rows_all, size = n_mod, replace = FALSE)
  subset_rows <- c(rh_sample_rows, modern_rows_all)
  
  sub_dm <- dist(df_std[subset_rows, metrics], method = "euclidean")
  sub_grp <- factor(df_std[[group_col]][subset_rows])
  
  ad <- adonis2(sub_dm ~ sub_grp, permutations = n_perm)
  
  # Betadisper on same subset
  bd <- betadisper(sub_dm, sub_grp)
  bd_anova <- anova(bd)
  bd_perm <- permutest(bd, permutations = n_perm)
  
  results[[i]] <- list(
    rep = i,
    pseudoF = ad$F[1],
    R2 = ad$R2[1],
    p = ad$`Pr(>F)`[1],
    bd_F = bd_anova$`F value`[1],
    bd_p = bd_perm$tab$`Pr(>F)`[1]
  )
}

res_df <- bind_rows(results)

# Summaries
summary_stats <- tibble(
  pseudoF_median = median(res_df$pseudoF),
  pseudoF_IQR_low = quantile(res_df$pseudoF, 0.25),
  pseudoF_IQR_high = quantile(res_df$pseudoF, 0.75),
  R2_median = median(res_df$R2),
  R2_IQR_low = quantile(res_df$R2, 0.25),
  R2_IQR_high = quantile(res_df$R2, 0.75),
  prop_significant = mean(res_df$p < 0.05),
  dispersion_F = mean(res_df$bd_F),
  dispersion_prop_sig = mean(res_df$bd_p < 0.05),
  dispersion_mean_p = mean(res_df$bd_p)
)

print("=== Summary of repeated balanced PERMANOVA ===")
print(summary_stats)


# PCA - to look at overlap between webs ####

# subset dataframe for comparing relevant analyses
df_lumped <- df %>%
  filter(!startsWith(Analysis, "Rhynie")|Analysis == "Rhynie_all_lumped"|Analysis == "Rhynie_all_TS") %>%
  #filter(Analysis != "Niche_TS") %>%
  group_by(Analysis) %>%
  slice_head(n = 100) %>%
  ungroup() %>%
  mutate(Analysis = as.factor(Analysis)) 

# select columns with numeric metrics we'd like to include in the PCA
metrics = df_lumped[, c("C", "Mean_NTP", "Max_NTP", "TrOmniv", "q_inCoherence", "Mean_path_len", "MeanInDegree", "Herbiv", "Carniv", "Top")]

# optional: scale the metrics
metrics_scaled <- scale(metrics)

# run PCA
pca_result <- prcomp(metrics_scaled, center = TRUE, scale. = TRUE)

# Get PCA scores and combine with metadata
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Web_ID <- df_lumped$SLN_ID
pca_scores$Analysis <- df_lumped$Analysis

# Loadings (contributions of metrics to PCs)
loadings <- pca_result$rotation
print(loadings)

# Optional: visual inspection of importance
summary(pca_result)

## PC1 vs PC2 plot ####
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Analysis, label = "")) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "PCA of Food Web Metrics", x = "PC1", y = "PC2") +
  scale_color_manual(values = c("Rhynie_all_lumped" = "#D6016D", "Rhynie_all_TS" = "#DB90FF", 
                                "EcoWeb_TS" = "#ADD40C", "DigelSoil_TS" = "#7D845E",
                                "Niche_lumped" = "blue", "Niche_TS" = "cyan"))

## PC2 vs PC3 plot ####
ggplot(pca_scores, aes(x = PC2, y = PC3, color = Analysis, label = "")) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "PCA of Food Web Metrics", x = "PC2", y = "PC3") +
  scale_color_manual(values = c("Rhynie_all_lumped" = "#D6016D", "Rhynie_all_TS" = "#DB90FF", 
                                "EcoWeb_TS" = "#ADD40C", "DigelSoil_TS" = "#7D845E",
                                "Niche_lumped" = "blue", "Niche_TS" = "cyan"))

# Non-linear least squares model ####
library(propagate)
library(nlstools)

logistic_model <- nls(Max_NTP ~ SSlogis(S, a, b, c),
                      data = df_modern)

summary(logistic_model)

confint2(logistic_model) # confidence intervals for parameters

# Scatterplot of observed data
plot(df_modern$S, df_modern$Max_NTP,
     xlab = "Species Richness (S)",
     ylab = "Max NTP",
     main = "Logistic Fit",
     pch = 16, col = "darkgray")

# Add the fitted logistic curve
S_vals <- seq(min(df_modern$S), max(df_modern$S), length.out = 200)
predicted_vals <- predict(logistic_model, newdata = data.frame(S = S_vals))

lines(S_vals, predicted_vals, col = "blue", lwd = 2)

### generate fit value (pseudo R^2)
# Predicted values
fitted_vals <- predict(logistic_model)

# Actual values
obs_vals <- df_modern$Max_NTP

# Residual Sum of Squares
rss <- sum((obs_vals - fitted_vals)^2)

# Total Sum of Squares
tss <- sum((obs_vals - mean(obs_vals))^2)

# Pseudo R²
r_squared <- 1 - (rss / tss)
r_squared

S_vals <- seq(min(df_modern$S), max(df_modern$S), length.out = 100)
newdata <- data.frame(S = S_vals)

pred_ci <- predictNLS(logistic_model, newdata = newdata, interval = "confidence", alpha = 0.05)

# Plot with confidence bands
plot(df_modern$S, df_modern$Max_NTP, pch = 16, col = "gray40",
     xlab = "Species Richness (S)", ylab = "Max NTP", main = "Logistic Fit with Confidence Interval")
lines(S_vals, pred_ci$summary[,1], col = "blue", lwd = 2)
lines(S_vals, pred_ci$summary[,11], col = "blue", lty = 2)
lines(S_vals, pred_ci$summary[,12], col = "blue", lty = 2)

# (Deprecated?) create subsets of df for analyses ####

# subset dataframe for comparing full (Trophic Species) rhynie to modern webs
df_unlumped <- df %>%
  filter(!startsWith(Analysis, "Rhynie")|(startsWith(Analysis, "Rhynie_all")&!endsWith(Analysis,"lumped"))) %>%
  mutate(Analysis = as.factor(Analysis)) 

# subset dataframe for comparing lumped rhynie to modern webs
df_lumped <- df %>%
  filter(!startsWith(Analysis, "Rhynie")|Analysis == "Rhynie_all_lumped") %>%
  filter(Analysis != "Niche_TS") %>%
  group_by(Analysis) %>%
  slice_head(n = 100) %>%
  ungroup() %>%
  mutate(Analysis = as.factor(Analysis)) 

# subset dataframe for comparing rhynie web to terrestrial and aquatic subsets (not lumped)
df_habitats <- df %>%
  filter(startsWith(Analysis, "Rhynie")&!endsWith(Analysis,"lumped")) %>%
  mutate(Analysis = as.factor(Analysis)) 

# subset dataframe for comparing full and lumped rhynie webs (complete)
df_rhynielumping <- df %>%
  filter(startsWith(Analysis, "Rhynie_all")) %>%
  mutate(Analysis = as.factor(Analysis)) 
