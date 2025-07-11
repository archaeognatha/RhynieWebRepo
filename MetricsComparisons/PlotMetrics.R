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

### Statistical comparisons of samples

# label webs as Devonian or Modern
df <- df %>%
  mutate(Period = ifelse(startsWith(Analysis, "Rhynie"),"devonian","modern")) %>%
  mutate(Period = as.factor(Period))

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
