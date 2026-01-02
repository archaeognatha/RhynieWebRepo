# START HERE ####
# install.packages("tidyverse")
# Load necessary libraries
library(tidyverse)

# Read and inspect data
df <- read.csv("Combined_Data_TS.csv")

# label webs as Devonian, Eocene, Modern, or simulation (niche model)
df <- df %>%
  mutate(Period = ifelse(startsWith(Analysis, "Rhynie"),"devonian", 
                         ifelse(startsWith(Analysis, "Messel"),"eocene",
                                ifelse(startsWith(Analysis, "Niche"), "simulation", "modern")))) %>%
  mutate(Period = as.factor(Period))

# PANEL SCATTERPLOT FIGURE 1: Modern webs vs. Rhynie complete, lumped, terrestrial, aquatic ####

metrics = c("C", "InDegree",
            "NTP_mean", "NTP_max", 
            "diameter", "max_chain_len", 
            "char_path_len", "std_path_len", "loop",
            "TrOmniv", "q",
            "priConsumers", "herbivores", "secConsumers", "Top")

## Scatterplot 1 filter to analyses of interest and rename metrics and analyses #### 
#     also, only include 100 SLNs for each Rhynie analysis so as not to overcrowd plots
df_plot <- df %>%
  filter(Analysis %in% c("Digel_Soil", "DeRuiter_Soil", "EcoWeb", 
                        "Coachella", "LittleRockLake", "TuesdayLake", "YthanEstuary", 
                        "Messel_all", "Messel_hicert",
                        "Rhynie_all_TS", "Rhynie_all_lumped", "Rhynie_terr_lumped", "Rhynie_aqu_lumped")) %>%
  mutate(Analysis = fct_recode(Analysis, "Modern_DigelSoil" = "Digel_Soil", 
                               "Modern_DeRuiterSoil" = "DeRuiter_Soil", "Modern_EcoWeb" = "EcoWeb", 
                               "Modern_Coachella" = "Coachella", "Modern_LitleRock" = "LittleRockLake",
                               "Modern_TuesLake" = "TuesdayLake", "Modern_Ythan" = "YthanEstuary",
                               "Messel_all" = "Messel_all", "Messel_high_certainty" = "Messel_hicert",
                               "Rhynie_complete" = "Rhynie_all_TS", "Rhynie_lumped" = "Rhynie_all_lumped",
                               "Rhynie_terr" = "Rhynie_terr_lumped", "Rhynie_aqu" = "Rhynie_aqu_lumped" )) %>%
  rename(NTP_mean = mean_NTP, NTP_max = max_NTP, q = q_inCoherence,
         char_path_len = mean_path_len, InDegree = meanInDegree, 
         priConsumers = Herbiv, herbivores = Herbiv_true, secConsumers = Carniv) %>%
  group_by(Analysis) %>%
  slice_head(n = 100)

# Reshape to long format for ggplot
df_long <- df_plot %>%
  pivot_longer(
    cols = all_of(metrics),
    names_to = "Metric",
    values_to = "Value"
  )

# re-code the levels in Analysis to make sure it plots in the right order
df_long$Analysis <- factor(df_long$Analysis,
                           levels = c("Modern_DigelSoil", "Modern_DeRuiterSoil", 
                                      "Modern_EcoWeb", "Modern_Coachella", "Modern_Ythan", 
                                      "Modern_LittleRock", "Modern_TuesLake",
                                      "Messel_all", "Messel_high_certainty",
                                      "Rhynie_complete", "Rhynie_lumped", "Rhynie_terr", "Rhynie_aqu"))
# re-code the levels in Metric to make sure the facets appear in the right order
df_long$Metric <- factor(df_long$Metric, levels = metrics)

## Scatterplot 1 metric labels ####
metric_labels <- c(C = "Connectance (C)", 
                   InDegree = "In-degree", 
                   NTP_mean = "Mean ntp", 
                   NTP_max = "Max ntp", 
                   diameter = "Network diameter", 
                   max_chain_len = "Max chain length",
                   char_path_len = "Characteristic path length", 
                   std_path_len = "Path length std. dev.", 
                   loop = "Loop frequency",
                   TrOmniv = "Trophic omnivory", 
                   q = "Coherence (q)", 
                   priConsumers = "Primary consumers",
                   herbivores = "Herbivores", 
                   secConsumers= "Secondary consumers", 
                   Top = "Apex species")

## Scatterplot 1 color palette ####
analysis_colors <- c(
  "Modern_DigelSoil" = "#A58F52",
  "Modern_DeRuiterSoil" = "chocolate4",
  "Modern_EcoWeb" = "green",
  "Modern_Coachella" = "#CBD44F",
  "Modern_Ythan" = "#4FD489",
  "Modern_LittleRock" = "#32B097",
  "Modern_TuesLake" = "#32B065",
  "Messel_all" = "red",
  "Messel_high_certainty" = "red3",
  "Rhynie_complete" = "#DB90FF",
  "Rhynie_lumped" = "#D6016D",
  "Rhynie_terr" = "#E8BE21",
  "Rhynie_aqu" = "#2F9FDE"
)


## Scatterplot 1 datapoint shapes ####
analysis_shapes <- c(
  "Modern_DigelSoil" = 15, "Modern_DeRuiterSoil" = 15,
  "Modern_EcoWeb" = 15, "Modern_Coachella" = 15,
  "Modern_Ythan" = 15, "Modern_TuesLake" = 15,
  "Messel_all" = 18, "Messel_high_certainty" = 18,
  "Rhynie_complete" = 20,"Rhynie_lumped" = 20,
  "Rhynie_terr" = 20, "Rhynie_aqu" = 20
)

# (optional) choose a subset of metrics to view:
metrics_active <- metrics[11:15]
df_long_view <- df_long %>%
  filter(Metric %in% metrics_active)

## Scatterplot 1 GENERATE FIGURE (ggplot2 call) ####
ggplot(df_long_view, aes(x = S, y = Value)) +
  geom_point(alpha = 0.6, size = 1, aes(colour = Analysis, shape = Analysis)) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3, labeller = labeller(Metric = metric_labels[metrics_active])) +
  scale_color_manual(values = analysis_colors,
                     breaks = c("Modern_DigelSoil", "Modern_DeRuiterSoil",
                                "Modern_EcoWeb", "Modern_Coachella", 
                                "Modern_Ythan", "Modern_TuesLake",
                                "Messel_all", "Messel_high_certainty",
                                "Rhynie_complete", "Rhynie_lumped", 
                                "Rhynie_terr", "Rhynie_aqu"),
                     labels = c("Modern: Digel Soil", "Modern_DeRuiterSoil",
                                "Modern: EcoWeb", "Modern: Coachella",
                                "Modern: Ythan Estuary",  "Modern: Tuesday Lake",
                                "Messel: complete", "Messel: high certainty links",
                                "Rhynie: complete", "Rhynie: lumped",
                                "Rhynie: terrestrial", "Rhynie: aquatic")) +
  scale_shape_manual(values = analysis_shapes,
                     breaks = c("Modern_DigelSoil", "Modern_DeRuiterSoil",
                                "Modern_EcoWeb", "Modern_Coachella", 
                                "Modern_Ythan", "Modern_TuesLake",
                                "Messel_all", "Messel_high_certainty",
                                "Rhynie_complete", "Rhynie_lumped", 
                                "Rhynie_terr", "Rhynie_aqu"),
                     labels = c("Modern: Digel Soil", "Modern_DeRuiterSoil",
                                "Modern: EcoWeb", "Modern: Coachella",
                                "Modern: Ythan Estuary",  "Modern: Tuesday Lake",
                                "Messel: complete", "Messel: high certainty links",
                                "Rhynie: complete", "Rhynie: lumped",
                                "Rhynie: terrestrial", "Rhynie: aquatic")) +
  scale_x_continuous(name = "S (Species Richness)") +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = c(0.84,0.15),
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
) +
  guides(colour = guide_legend(override.aes = list(size=3)))

#-----------------------------------------------------------------------------#

# PANEL BOXPLOT FIGURE 1: Rhynie (lumped + unlumped) vs. Niche Model results ####

metrics = c("C", "InDegree",
            "NTP_mean", "NTP_max", 
            "diameter", "max_chain_len", 
            "char_path_len", "std_path_len", "loop",
            "TrOmniv", "q",
            "priConsumers", "herbivores", "secConsumers", "Top")

## Boxplot 1 filter to only full/lumped Rhynie and Niche model, rename metrics and analyses #### 
df_rhynie <- df %>%
  filter(Analysis %in% c("Rhynie_all_TS", "Rhynie_all_lumped", "Niche_unlumped", "Niche_lumped")) %>%
  mutate(Analysis = fct_recode(Analysis, "Rhynie_complete" = "Rhynie_all_TS", "Rhynie_lumped" = "Rhynie_all_lumped", 
                               "Niche_complete" = "Niche_unlumped", "Niche_lumped" = "Niche_lumped")) %>%
  rename(NTP_mean = mean_NTP, NTP_max = max_NTP, q = q_inCoherence,
         char_path_len = mean_path_len, InDegree = meanInDegree, 
         priConsumers = Herbiv, herbivores = Herbiv_true, secConsumers = Carniv)

# Reshape to long format
df_rhynie_long <- df_rhynie %>%
  pivot_longer(
    cols = all_of(metrics),
    names_to = "Metric",
    values_to = "Value"
  )

# re-code the levels in Analysis to make sure it plots in the right order
df_rhynie_long$Analysis <- factor(df_rhynie_long$Analysis,
                                  levels = c("Rhynie_complete", "Niche_complete", "Rhynie_lumped", "Niche_lumped"))
# re-code the levels in Metric to make sure the facets appear in the right order
df_rhynie_long$Metric <- factor(df_rhynie_long$Metric, levels = metrics)

## Boxplot 1 metric labels ####
metric_labels <- c(C = "Connectance (C)", 
                   InDegree = "In-degree", 
                   NTP_mean = "Mean ntp", 
                   NTP_max = "Max ntp", 
                   diameter = "Network diameter", 
                   max_chain_len = "Max chain length",
                   char_path_len = "Characteristic path length", 
                   std_path_len = "Path length std. dev.", 
                   loop = "Loop frequency",
                   TrOmniv = "Trophic omnivory", 
                   q = "Coherence (q)", 
                   priConsumers = "Primary consumers",
                   herbivores = "Herbivores", 
                   secConsumers= "Secondary consumers", 
                   Top = "Apex species")


## Boxplot 1 color palette ####
analysis_colors <- c(
  "Rhynie_complete" = "#DB90FF",
  "Rhynie_lumped" = "#D6016D",
  "Niche_complete" = "#093BC5",
  "Niche_lumped" = "#23D2E4"
)

metrics_active <- metrics[11:15]
df_rhynie_long_view <- df_rhynie_long %>%
  filter(Metric %in% metrics_active)

## Boxplot 1 GENERATE FIGURE (ggplot2 call) ####
ggplot(df_rhynie_long_view, aes(x = Analysis, y = Value, fill = Analysis)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3, labeller = labeller(Metric = metric_labels)) +
  scale_fill_manual(values = analysis_colors, 
                    breaks = c("Rhynie_complete", "Niche_complete",
                               "Rhynie_lumped", "Niche_lumped"),
                    labels = c("Rhynie: complete", "Niche model (S = Rhynie complete)",
                               "Rhynie: lumped", "Niche model (S = Rhynie lumped)")) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    legend.position = c(0.84,0.15),
    legend.title = element_blank(),
    legend.key.width = unit(0.75, "cm"),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "", x = NULL, y = NULL)


# PANEL BOXPLOT FIGURE 2: specific metrics of interest ####

# define metrics of interest and renamed versions
metrics <- c("Max_NTP", "q_inCoherence", "Mean_path_len", "Herbiv")
metrics2 <- c("NTP_max", "q", "char_path_len", "priConsumers")

## Boxplot 2 filter to only lumped Rhynie and modern, metrics of interest, rename metrics + analyses #### 
df_sigmetrics <- df %>%
  filter(Analysis %in% c("Rhynie_all_lumped", "DigelSoil_TS", "EcoWeb_TS")) %>%
  mutate(Analysis = fct_recode(Analysis, "Rhynie_lumped" = "Rhynie_all_lumped", 
                               "Modern_EcoWeb" = "EcoWeb_TS", "Modern_DigelSoil" = "DigelSoil_TS", )) %>%
  filter(S > 15) %>% # only keep webs with more than 15 species to mitigate sensitivity to scaling
  select(all_of(append("Analysis", metrics))) %>%
  rename(NTP_max = Max_NTP, q = q_inCoherence, char_path_len = Mean_path_len, priConsumers = Herbiv) %>%
  group_by(Analysis) %>%
  slice_head(n = 100)

# Reshape to long format
df_sigmetrics_long <- df_sigmetrics %>%
  pivot_longer(
    cols = all_of(metrics2),
    names_to = "Metric",
    values_to = "Value"
  )

# re-code the levels in Metric to make sure the facets appear in the right order
df_sigmetrics_long$Metric <- factor(df_sigmetrics_long$Metric, levels = metrics2)

## Boxplot 2 metric labels ####
metric_labels <- c(NTP_max = "Max ntp", 
                   q = "Coherence (q)",
                   char_path_len = "Characteristic path length", 
                   priConsumers = "Primary consumers")

## Boxplot 2 color palette ####
analysis_colors <- c(
  "Rhynie_lumped" = "#D6016D",
  "Modern_EcoWeb" = "#9CC104",
  "Modern_DigelSoil" = "#7D845E"
)

## Boxplot 2 GENERATE FIGURE (ggplot2 call) ####
ggplot(df_sigmetrics_long, aes(x = Analysis, y = Value, fill = Analysis)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 2, labeller = labeller(Metric = metric_labels)) +
  scale_fill_manual(values = analysis_colors, 
                    breaks = c("Rhynie_lumped", "Modern_EcoWeb", "Modern_DigelSoil")) +
  scale_x_discrete(breaks = c("Rhynie_lumped", "Modern_EcoWeb", "Modern_DigelSoil"),
                   labels = c("Rhynie", "Modern:\nEcoWeb\n(S > 15)", "Modern:\nDigelSoil")) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "", x = NULL, y = NULL)

# (deprecated?) INDIVIDUAL METRIC SCATTERPLOTS ####

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
ggplot(df_small, aes(x = S, y = mean_NTP, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Mean Trophic Position vs Species Richness",
       x = "Number of species (S)",
       y = "Mean Trophic Position")

# max trophic level (mean_NTP) vs S
ggplot(df_small, aes(x = S, y = max_NTP, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Max Trophic Position vs Species Richness",
       x = "Number of species (S)",
       y = "Max Trophic Position")

# generality (mean_InDegree) vs S
ggplot(df_small, aes(x = S, y = meanInDegree, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Mean In Degree (generality) vs Species Richness",
       x = "Number of species (S)",
       y = "Mean In Degree")

# incoherence (q) vs S
ggplot(df_small, aes(x = S, y = TrOmniv, color = Analysis)) +
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
ggplot(df_small, aes(x = S, y = Herbiv, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Primary consumers vs Species Richness",
       x = "Number of species (S)",
       y = "Percentage of primary consumers")

# % Secondary consumer taxa vs S
ggplot(df_small, aes(x = S, y = Carniv, color = Analysis)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Secondary consumers vs Species Richness",
       x = "Number of species (S)",
       y = "Percentage of secondary consumers")

# (deprecated?) Group comparisons - individual metric boxplots ####

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


# (deprecated) PANEL BOXPLOT FIGURE: Modern webs, Rhynie Complete, Rhynie Lumped ####

metrics = c("C", "NTP_mean_norm", "NTP_max", "InDegree", "TrOmniv", "q", "char_path_len", "std_path_len", "priConsumers", "secConsumers", "Top")

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
  "Modern_EcoWeb" = "#9CC104",
  "Modern_DigelSoil" = "#7D845E",
  "Rhynie_complete" = "#DB90FF",
  "Rhynie_lumped" = "#D6016D"
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

# (deprecated) PANEL BOXPLOT FIGURE: Rhynie Complete, Rhynie aquatic, Rhynie terrestrial ####

metrics = c("C", "NTP_mean_norm", "NTP_max", "InDegree", "TrOmniv", "q", "char_path_len", "Std_path_len", "priConsumers", "secConsumers", "Top")

# Filter to the Rhynie lumped variants and rename metrics and analyses
df_rhynie <- df %>%
  filter(Analysis %in% c("Rhynie_all_lumped", "Rhynie_terr_lumped", "Rhynie_aqu_lumped")) %>%
  mutate(Analysis = fct_recode(Analysis, "Rhynie_complete" = "Rhynie_all_lumped", "Rhynie_terr" = "Rhynie_terr_lumped", "Rhynie_aqu" = "Rhynie_aqu_lumped")) %>%
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
