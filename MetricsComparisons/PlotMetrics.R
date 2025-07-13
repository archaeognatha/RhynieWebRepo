# Load necessary libraries
library(tidyverse)

# Read and inspect data
df <- read_csv("TrophospeciesMetrics.csv")

# subset the dataframe with the first 100 rhynie replicates and remove the lumped terr/aqu webs 
# ...to make the plots below easier to read
df_small <- df %>%
  filter(!Analysis %in% c("Rhynie_terr_lumped", "Rhynie_aqu_lumped")) %>%
  group_by(Analysis) %>%
  slice_head(n = 100) %>%
  ungroup()

# Plot C vs S, colored by Analysis
ggplot(df_small, aes(x = S, y = C, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Connectance vs. Species richness",
       x = "Number of Species (S)",
       y = "Connectance (C)")

# Plot L/S vs S, colored by Analysis
ggplot(df_small, aes(x = S, y = L_D, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Link Density vs. Species richness",
       x = "Number of Species (S)",
       y = "Link Density (L/S)")

# mean trophic level (mean_NTP) vs S
ggplot(df_small, aes(x = S, y = Mean_NTP, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Mean Trophic Position vs Species Richness",
       x = "Number of species (S)",
       y = "Mean Trophic Position")


# max trophic level (mean_NTP) vs S
ggplot(df_small, aes(x = S, y = Max_NTP, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Max Trophic Position vs Species Richness",
       x = "Number of species (S)",
       y = "Max Trophic Position")

# generality (mean_InDegree) vs S
ggplot(df_small, aes(x = S, y = Mean_NTP, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Mean In Degree (generality) vs Species Richness",
       x = "Number of species (S)",
       y = "Mean In Degree")

# incoherence (q) vs S
ggplot(df_small, aes(x = S, y = q_inCoherence, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "q (incoherence) vs Species Richness",
       x = "Number of species (S)",
       y = "q")

# max chain length vs S
ggplot(df_small, aes(x = S, y = Mean_longest_chain, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Longest Chain Length vs Species Richness",
       x = "Number of species (S)",
       y = "Mean Longest Chain Length")

# mean path length vs S
ggplot(df_small, aes(x = S, y = Mean_path_len, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Mean path Length vs Species Richness",
       x = "Number of species (S)",
       y = "Mean path length between nodes")

# % apex taxa vs S
ggplot(df_small, aes(x = S, y = Top, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Trophic Top Species vs Species Richness",
       x = "Number of species (S)",
       y = "Percentage of nodes without predators")

# % primary consumer taxa vs S
ggplot(df_small, aes(x = S, y = Top, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Primary consumers vs Species Richness",
       x = "Number of species (S)",
       y = "Percentage of primary consumers")

# Group comparisons

# boxplot of connectance
ggplot(df, aes(x = Analysis, y = C, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Connectance of Rhynie and Modern webs")

# boxplot of link density
ggplot(df, aes(x = Analysis, y = L_D, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Link density of Rhynie and Modern webs")

# boxplot of mean trophic level
ggplot(df, aes(x = Analysis, y = Mean_NTP, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Mean Trophic Position of Rhynie and Modern webs")

# boxplot of mean trophic level (normalized by S)
ggplot(df, aes(x = Analysis, y = Mean_NTP_norm, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Normalized Mean Trophic Position of Rhynie and Modern webs (mean NTP/S)")

# boxplot of max trophic level
ggplot(df, aes(x = Analysis, y = Max_NTP, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Max Trophic Position of Rhynie and Modern webs")

# boxplot of Trophic Omnivory (feeding at multiple levels)
ggplot(df, aes(x = Analysis, y = TrOmniv, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Trophic Omnivory (% of taxa that feed at multiple levels)")

# boxplot of q (incoherence)
ggplot(df, aes(x = Analysis, y = q_inCoherence, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "% incoherence (q) of Rhynie and Modern webs")

# boxplot of longest chain
ggplot(df, aes(x = Analysis, y = Mean_longest_chain , fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Level of mean longest chain length in Rhynie and Modern webs")

# boxplot of mean path length
ggplot(df, aes(x = Analysis, y = Mean_path_len , fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Level of mean path length in Rhynie and Modern webs")

# boxplot of trophic generality (in-degree)
ggplot(df, aes(x = Analysis, y = MeanInDegree , fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Mean in-degree in Rhynie and Modern webs")

# boxplot of variation of trophic generality (std. dev. in-degree)
ggplot(df, aes(x = Analysis, y = StdInDegree , fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Standard deviation of in-degree in Rhynie and Modern webs")

# boxplot of % carnivores
ggplot(df, aes(x = Analysis, y = Carniv, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "% carnivory of Rhynie and Modern web consumers")

# boxplot of % herbivores
ggplot(df, aes(x = Analysis, y = Herbiv, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "% primary consumer of Rhynie and Modern web consumers")

# boxplot of % top species
ggplot(df, aes(x = Analysis, y = Top, fill = Analysis)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "% taxa in Rhynie and Modern webs without predators")

### PANEL BOXPLOT FIGURE 1: Modern webs, Rhynie Complete, Rhynie Lumped

metrics = c("C", "LinkDensity", "NTP_mean_norm", "NTP_max", "InDegree", "TrOmniv", "q", "char_path_len", "mean_longest_path_len", "priConsumers", "secConsumers", "Top")

# Filter to the analyses of interest and rename metrics and analyses
df_plot <- df %>%
  filter(Analysis %in% c("DigelSoil_TS", "EcoWeb_TS", "Rhynie_all_TS", "Rhynie_all_lumped")) %>%
  mutate(Analysis = fct_recode(Analysis, "Modern_DigelSoil" = "DigelSoil_TS", "Modern_EcoWeb" = "EcoWeb_TS", "Rhynie_complete" = "Rhynie_all_TS", "Rhynie_lumped" = "Rhynie_all_lumped")) %>%
  rename(LinkDensity = L_D, NTP_mean_norm = Mean_NTP_norm, NTP_max = Max_NTP, q = q_inCoherence,
         mean_longest_path_len = Mean_longest_chain, char_path_len = Mean_path_len, InDegree = MeanInDegree,
         priConsumers = Herbiv, secConsumers = Carniv)

# Reshape to long format for ggplot
df_long <- df_plot %>%
  pivot_longer(
    cols = metrics,
    names_to = "Metric",
    values_to = "Value"
  )

# re-code the levels in Analysis to make sure it plots in the right order
df_long$Analysis <- factor(df_long$Analysis,
                           levels = c("Modern_EcoWeb", "Modern_DigelSoil", "Rhynie_complete", "Rhynie_lumped"))

# Define color palette for consistency
analysis_colors <- c(
  "Modern_EcoWeb" = "#ADD40C",
  "Modern_DigelSoil" = "#7D845E",
  "Rhynie_complete" = "#D6016D",
  "Rhynie_lumped" = "#DB90FF"
)

# Create the boxplot figure
ggplot(df_long, aes(x = Analysis, y = Value, fill = Analysis)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = analysis_colors) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "Food Web Metrics by Analysis", x = NULL, y = NULL)

### PANEL BOXPLOT FIGURE 2: Rhynie Complete, Rhynie aquatic, Rhynie terrestrial

metrics = c("C", "LinkDensity", "NTP_mean_norm", "NTP_max", "InDegree", "TrOmniv", "q", "char_path_len", "mean_longest_path_len", "priConsumers", "secConsumers", "Top")

# Filter to the Rhynie unlumped variants and rename metrics and analyses
df_rhynie <- df %>%
  filter(Analysis %in% c("Rhynie_all_TS", "Rhynie_terr_TS", "Rhynie_aqu_TS")) %>%
  mutate(Analysis = fct_recode(Analysis, "Rhynie_complete" = "Rhynie_all_TS", "Rhynie_terr" = "Rhynie_terr_TS", "Rhynie_aqu" = "Rhynie_aqu_TS")) %>%
  rename(LinkDensity = L_D, NTP_mean_norm = Mean_NTP_norm, NTP_max = Max_NTP, q = q_inCoherence,
         mean_longest_path_len = Mean_longest_chain, char_path_len = Mean_path_len, InDegree = MeanInDegree,
         priConsumers = Herbiv, secConsumers = Carniv)

# Reshape to long format
df_rhynie_long <- df_rhynie %>%
  pivot_longer(
    cols = metrics,
    names_to = "Metric",
    values_to = "Value"
  )

# re-code the levels in Analysis to make sure it plots in the right order
df_rhynie_long$Analysis <- factor(df_rhynie_long$Analysis,
                           levels = c("Rhynie_complete", "Rhynie_terr", "Rhynie_aqu"))

# Define color palette for consistency
analysis_colors <- c(
  "Rhynie_complete" = "#D6016D",
  "Rhynie_terr" = "#E8BE21",
  "Rhynie_aqu" = "#2F9FDE"
)

# Plot
ggplot(df_rhynie_long, aes(x = Analysis, y = Value, fill = Analysis)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = analysis_colors) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "Rhynie Web Subsets (unlumped)", x = NULL, y = NULL)
