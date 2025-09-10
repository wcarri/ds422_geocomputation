#https://geoportal.hawaii.gov/datasets/tsunami-evacuation-zones/explore?showTable=true

library(tidyverse)
library(sf)
library(here)
library(mapgl)
library(tidycensus)
#library(rsocrata)

evac_zones <- st_read(here("data/Tsunami_Evacuation_Zones.geojson"))



# 2) Simple style (semi-transparent fill + outline)
fill_col <- "#ef4444"# red
line_col <- "#7f1d1d"

mapboxgl(bounds = evac_zones) |>
  add_fill_layer(
    id = "tsunami_evac",
    source = evac_zones,
    fill_color = fill_col,
    fill_opacity = 0.55,
    tooltip = "mapname"
  ) |>
  add_line_layer(
    id = "tsunami_evac",
    source = evac_zones,
    line_color = line_col,
    line_width = 1
  ) |>
   add_legend(
    legend_title = "Tsunami Evacuation Zones",
    values = "Evacuation Area",
    colors = fill_col,
    type = "categorical"
  )



# Question 1: How many hawaii residents live in evacuation zones

# --- 1) Get ACS population with geometry (block groups) ---
bg <- get_acs(
  geography = "block group",
  variables = "B01003_001",   # total population
  state = "HI",
  year = 2023, survey = "acs5",
  geometry = TRUE, cache_table = TRUE
) |>
  st_transform(4326) |>
  st_make_valid() |>
  rename(pop = estimate) |>
  select(GEOID, pop)



block_groups <- bg |>
  mutate(blockgroup_area = st_area(geometry))

evac_zones <- evac_zones |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())



evac_zones <- evac_zones |>
  select(evac_id, island, zone_type, zone_desc)


intersections <- st_intersection(block_groups, evac_zones)


intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))


intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))


intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)


sum(intersections$pop_in_zone)


## Challenge 
# Use the Tsunami_Evacuation_All_Zones.geojson (https://geoportal.hawaii.gov/datasets/437b77f132ed416294b45bde6aef23f4_11/explore?location=20.546870%2C-157.491600%2C7.83) 
# 1. Using mapgl, make a map of colored zone types
# 2. Can you tell me how many people are in Tsunami Safe Zones vs Tsunami Evacuation Zones vs Extreme Tsunami Evacuation Zones
# 3. What is the key assumption our analysis makes? Is this acceptable? How can it be improved?

