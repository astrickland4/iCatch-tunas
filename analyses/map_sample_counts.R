# Set up environment
library(leaflet)
library(sf)
library(htmlwidgets)
library(dplyr)
library(tidyr)



# Import data
tuna_data = read.csv("metadata.csv", header = TRUE)


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

# Display the map
map
saveWidget(map, file = "tuna_counts_map.html")

