# Set up environment
library(leaflet)
library(sf)
library(htmlwidgets)
library(dplyr)
library(tidyr)
library(leaflet.minicharts)


# Import data
tuna_data = read.csv("data/metadata.csv", header = TRUE)
tuna_data_clean <- tuna_data %>%
  filter(!is.na(Species_code) & Species_code != "" & trimws(Species_code) != "")

# Summarize species counts by site
site_summary <- tuna_data %>%
  group_by(Location_lat, Location_long, Location_sample) %>%
  summarise(
    total_count = n(),
    species_counts = paste(
      paste(names(table(Species_code)), table(Species_code), sep = ": "), 
      collapse = "<br>"
    ),
    .groups = 'drop'
  )

# Create popup text with species information
site_summary <- site_summary %>%
  mutate(
    popup_text = paste0(
      "<b>Site:</b> ", Location_sample, "<br>",
      "<b>Location:</b> ", round(Location_lat, 2), ", ", round(Location_long, 2), "<br>",
      "<b>Total Fish:</b> ", total_count, "<br>",
      "<b>Species Breakdown:</b><br>", species_counts
    )
  )

# Create the leaflet map
map <- leaflet(site_summary) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    lng = ~Location_long,
    lat = ~Location_lat,
    radius = 10,
    popup = ~popup_text,
    color = "#2E86AB",
    fillColor = "#A23B72",
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2
  )

species_summary <- tuna_data_clean %>%
  group_by(Location_lat, Location_long, Species_code) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = Species_code, 
    values_from = count, 
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., -Location_lat, -Location_long)))


species_colors_vector <- c("#000000", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")

total_species_counts <- species_summary %>%
  summarise(across(c("PBT", "ALB", "SKJ", "SBT", "BET", "YFT", "BFT", "BKT"), sum)) %>%
  pivot_longer(everything(), names_to = "Species", values_to = "Count") %>%
  filter(Count > 0)  # Only show species that were found

# Create the text for the box
total_text <- paste0(
  "<div style='background-color: white; padding: 10px; border: 2px solid #333; border-radius: 5px;'>",
  "<b>Total Species Counts</b><br>",
  paste(total_species_counts$Species, ": ", total_species_counts$Count, collapse = "<br>"),
  "<br><b>Grand Total: </b>", sum(total_species_counts$Count),
  "</div>"
)

map2 <- map %>% 
  addMinicharts(
    species_summary$Location_long, species_summary$Location_lat,
    chartdata = species_summary[, c("PBT", "ALB", "SKJ", "SBT", "BET", "YFT", "BFT", "BKT")],
    colorPalette = species_colors_vector,
    width = 90, height = 90,
    type="bar"
  ) %>%
  addControl(
    html = total_text,
    position = "bottomleft"  # You can use "topleft", "topright", "bottomleft", or "bottomright"
  )

# Display the map
map2
saveWidget(map2, file = "tuna_counts_map_20251010_2.html")
