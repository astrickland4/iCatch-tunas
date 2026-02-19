## Mapping metrics visualization ##


# DATA LOADING & PREP -----------------------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(viridis)
library(reshape2)

# Load mapping rate data and mapping metric data
mapping_metrics <- read.table("./data/mapping_metrics.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
mapping_rate <- read.table("./data/alignment_by_species.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Map species codes to full names
species_names <- c(
  "ALB" = "Albacore",
  "BET" = "Bigeye",
  "BFT" = "Atlantic Bluefin",
  "BKT" = "Blackfin",
  "PBT" = "Pacific Bluefin",
  "SBT" = "Southern Bluefin",
  "SKJ" = "Skipjack",
  "YFT" = "Yellowfin"
)

mapping_rate$Species_Full <- species_names[mapping_rate$Species]
mapping_metrics$Species_Full <- species_names[mapping_metrics$Species]

# Standardize & fix some species codes
mapping_metrics$Species <- toupper(mapping_metrics$Species)
mapping_metrics$Species <- gsub("^BE2$", "BET", mapping_metrics$Species)  # BE2 -> BET (Bigeye)
mapping_metrics$Species <- gsub("^BKF$", "BKT", mapping_metrics$Species)  # BKF -> BKT (Blackfin)
mapping_metrics$Species <- gsub("^PBF$", "PBT", mapping_metrics$Species)  # PBF -> PBT (Pacific Bluefin)
mapping_metrics$Species <- gsub("^SBF$", "SBT", mapping_metrics$Species)  # SBF -> SBT (Southern Bluefin)

# Clean up reference names
mapping_metrics <- mapping_metrics %>%
  mutate(
    Reference_Clean = gsub("_ref$", "", Reference),
    Reference_Clean = gsub("thunnus_albacares", "T. albacares", Reference_Clean),
    Reference_Clean = gsub("thunnus_maccoyii", "T. maccoyii", Reference_Clean),
    Reference_Clean = gsub("thunnus_thynnus", "T. thynnus", Reference_Clean),
    Reference_Clean = gsub("katsuwonus_pelamis", "K. pelamis", Reference_Clean)
  )

mapping_rate <- mapping_rate %>%
  mutate(
    Reference_Clean = gsub("_ref$", "", Reference),
    Reference_Clean = gsub("thunnus_albacares", "T. albacares", Reference_Clean),
    Reference_Clean = gsub("thunnus_maccoyii", "T. maccoyii", Reference_Clean),
    Reference_Clean = gsub("thunnus_thynnus", "T. thynnus", Reference_Clean),
    Reference_Clean = gsub("katsuwonus_pelamis", "K. pelamis", Reference_Clean)
  )


# 1. Mean Mapping Rate by Species and Reference ---------------------------

p1 <- ggplot(mapping_rate, aes(x = Reference_Clean, y = Mean_Alignment_Rate, fill = Species_Full)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = Min_Alignment_Rate, ymax = Max_Alignment_Rate),
                position = position_dodge(0.9), width = 0.3, alpha = 0.6) +
  labs(title = "Mean Mapping Rate by Reference Genome and Species",
       subtitle = "Error bars show min-max range",
       x = "Reference Genome",
       y = "Mean Mapping Rate (0-1)",
       fill = "Species") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 1)
p1
ggsave("plots/mapping_rate_by_species_bars.png", p1, width = 12, height = 7, dpi = 300)


# 2. Mapping rates faceted by reference --------------------------------------


p2 <- ggplot(mapping_rate, aes(x = Species_Full, y = Mean_Alignment_Rate, fill = Species_Full)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = Min_Alignment_Rate, ymax = Max_Alignment_Rate),
                width = 0.3, alpha = 0.6) +
  facet_wrap(~Reference_Clean, ncol = 2, scales = "free_x") +
  labs(title = "Mapping Rates by Reference Genome",
       x = "Species",
       y = "Mean Mapping Rate (0-1)") +
  theme_classic(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.background = element_rect(fill = "lightgray")) +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 1)
p2
ggsave("plots/mapping_rate_faceted_by_reference.png", p3, width = 12, height = 10, dpi = 300)


# 3. Heatmap of Mean Mapping Rates ----------------------------------------


p3 <- ggplot(mapping_rate, aes(x = Reference_Clean, y = Species_Full, fill = Mean_Alignment_Rate)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f", Mean_Alignment_Rate)), 
            color = "black", size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                       midpoint = 0.5, limits = c(0, 1),
                       name = "Mapping\nRate") +
  labs(title = "Mapping Rate Heatmap",
       subtitle = "Shows mean alignment rate for each species-reference combination",
       x = "Reference Genome",
       y = "Species") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())
p3
ggsave("plots/mapping_rate_heatmap.png", p2, width = 10, height = 8, dpi = 300)


# 4. Best Reference per Species -------------------------------------------


best_ref <- mapping_rate %>%
  group_by(Species_Full) %>%
  slice_max(order_by = Mean_Alignment_Rate, n = 1) %>%
  ungroup()

p4 <- ggplot(best_ref, aes(x = reorder(Species_Full, Mean_Alignment_Rate), 
                            y = Mean_Alignment_Rate, fill = Reference_Clean)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f", Mean_Alignment_Rate)),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Best Reference Genome for Each Species",
       subtitle = "Showing highest mean mapping rate",
       x = "Species",
       y = "Mean Mapping Rate (0-1)",
       fill = "Best Reference") +
  theme_classic(base_size = 12) +
  scale_fill_brewer(palette = "Set1") +
  ylim(0, 1)
p4
ggsave("plots/best_reference_per_species.png", p4, width = 10, height = 7, dpi = 300)


# 5. Mean depth - grouped by species --------------------------------------


p5 <- ggplot(mapping_metrics, aes(x = Reference_Clean, y = Mean_Depth, fill = Species_Full)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  labs(title = "Mean Coverage Depth by Reference Genome and Species",
       x = "Reference Genome",
       y = "Mean Depth (X)",
       fill = "Species") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "Set2")
p5
ggsave("plots/mean_depth_by_species.png", p3, width = 12, height = 7, dpi = 300)


# 6. Mean depth - average ----------------------------------------------------


depth_summary <- mapping_metrics %>%
  group_by(Reference_Clean) %>%
  summarise(Mean_Depth_Overall = mean(Mean_Depth, na.rm = TRUE),
            SD_Depth = sd(Mean_Depth, na.rm = TRUE),
            N = n())

p6 <- ggplot(depth_summary, aes(x = Reference_Clean, y = Mean_Depth_Overall, fill=Reference_Clean)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = pmax(0, Mean_Depth_Overall - SD_Depth),
                    ymax = Mean_Depth_Overall + SD_Depth),
                width = 0.3, linewidth = 0.8) +
  geom_text(aes(label = paste0("n=", N)), vjust = -0.5, size = 3) +
  labs(title = "Average Coverage Depth by Reference Genome",
       x = "Reference Genome",
       y = "Mean Depth (X)",
       fill = "Reference") +
  theme_classic(base_size = 12) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6
ggsave("plots/mean_depth_overall.png", p4, width = 8, height = 6, dpi = 300)


# 7. Breadth (1X) - grouped by species ------------------------------------


p7 <- ggplot(mapping_metrics, aes(x = Reference_Clean, y = Breadth_1x, fill = Species_Full)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  labs(title = "Genome Breadth (≥1X) by Reference and Species",
       x = "Reference Genome",
       y = "Breadth of Coverage (%)",
       fill = "Species") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 105)
p7
ggsave("plots/breadth_1x_by_species.png", p5, width = 12, height = 7, dpi = 300)


# 8. Breadth (1X) - average -----------------------------------------------


breadth_summary <- mapping_metrics %>%
  group_by(Reference_Clean) %>%
  summarise(Mean_Breadth_1x = mean(Breadth_1x, na.rm = TRUE),
            SD_Breadth_1x = sd(Breadth_1x, na.rm = TRUE),
            N = n())

p8 <- ggplot(breadth_summary, aes(x = Reference_Clean, y = Mean_Breadth_1x, fill=Reference_Clean)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = pmax(0, Mean_Breadth_1x - SD_Breadth_1x),
                    ymax = pmin(100, Mean_Breadth_1x + SD_Breadth_1x)),
                width = 0.3, linewidth = 0.8) +
  geom_text(aes(label = paste0("n=", N)), vjust = -0.5, size = 3) +
  labs(title = "Average Genome Breadth (≥1X) by Reference",
       x = "Reference Genome",
       y = "Mean Breadth (%)", 
       fill = "Reference") +
  theme_classic(base_size = 12) +
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 105)
p8
ggsave("plots/breadth_1x_overall.png", p6, width = 8, height = 6, dpi = 300)


# 9. Mean mapping quality - grouped by species ----------------------------


# Filter out NA values for mapping quality
mapq_data <- mapping_metrics %>% filter(!is.na(Mean_MapQ) & Mean_MapQ > 0)

p9 <- ggplot(mapq_data, aes(x = Reference_Clean, y = Mean_MapQ, fill = Species_Full)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  labs(title = "Mean Mapping Quality by Reference and Species",
       x = "Reference Genome",
       y = "Mean MAPQ",
       fill = "Species") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red", alpha = 0.5)
p9
ggsave("plots/mean_mapq_by_species.png", p7, width = 12, height = 7, dpi = 300)


# 10. Mean mapping quality - average --------------------------------------


mapq_summary <- mapq_data %>%
  group_by(Reference_Clean) %>%
  summarise(Mean_MapQ_Overall = mean(Mean_MapQ, na.rm = TRUE),
            SD_MapQ = sd(Mean_MapQ, na.rm = TRUE),
            N = n())

p10 <- ggplot(mapq_summary, aes(x = Reference_Clean, y = Mean_MapQ_Overall, fill=Reference_Clean)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = pmax(0, Mean_MapQ_Overall - SD_MapQ),
                    ymax = Mean_MapQ_Overall + SD_MapQ),
                width = 0.3, linewidth = 0.8) +
  geom_text(aes(label = paste0("n=", N)), vjust = -0.5, size = 3) +
  labs(title = "Average Mapping Quality by Reference",
       x = "Reference Genome",
       y = "Mean MAPQ") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette="Set1")+
  geom_hline(yintercept = 30, linetype = "dashed", color = "red", alpha = 0.5)
p10
ggsave("plots/mean_mapq_overall.png", p8, width = 8, height = 6, dpi = 300)


# 11. Multi-panel summary plot --------------------------------------------

summary_plot <- plot_grid(p4, p6, p8, p10, 
                          ncol = 2, nrow = 2,
                          labels = c("A", "B", "C", "D"),
                          label_size = 14)
summary_plot
ggsave("plots/summary_all_metrics.png", summary_plot, width = 14, height = 12, dpi = 300)

