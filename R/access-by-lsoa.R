# Investigating Park Access by LSOA
library(BSol.mapR)
library(dplyr)
library(readxl)

dist_m = 1000

# Load Birmingham LSOAs
brum_lsoas <- LSOA21@data %>%
  filter(
    Area == "Birmingham"
  ) %>%
  select(
    LSOA21, LONG, LAT
  ) %>%
  mutate(
    num_parks_1km = NA,
    num_play_areas_1km = NA,
    dist_to_nearest_park = NA,
    dist_to_nearest_big_park = NA,
    total_park_size_1km = NA
  )


# Load park info
park_info <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  "Park Info"
) %>%
  select(
    Old_Site_Ref, Site_Name, Square_Meters
  ) %>%
  inner_join(
    read_excel(
      "data/park_multi_access_info_2024.xlsx",
      "Play Parks"
    ),
    by = join_by("Old_Site_Ref")
  ) 

# Load Parks data
park_coords <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  "Park Locations"
) %>%
  filter(
    # Filter for 376
    Site_Name %in% park_info$Site_Name
  )


# Loop over all LSOAs
for (i in 1:nrow(brum_lsoas)) {
  # Find all parks within 1km
  parks_i <- purrr::map2_dfr(
    brum_lsoas$LAT[i],
    brum_lsoas$LONG[i],
    ~spatialrisk::points_in_circle(park_coords, .y, .x,
                                   lon = Longitude,
                                   lat = Latitude,
                                   radius = 5000)) %>%
    group_by(Site_Name) %>%
    summarise(
      distance = min(distance_m)
    ) %>%
    left_join(
      park_info,
      by = "Site_Name"
    )
  
  parks_i_1km <- parks_i %>%
    filter(
      distance < 1000
    )
  
  parks_i_big <- parks_i %>%
    filter(
      Square_Meters > 25000
    )
  
  brum_lsoas$num_parks_1km[i] = nrow(parks_i_1km)
  brum_lsoas$num_play_areas_1km[i] = sum(parks_i_1km$Play_Park)
  brum_lsoas$dist_to_nearest_park[i] = min(parks_i$distance)/1000
  brum_lsoas$dist_to_nearest_big_park[i] = min(parks_i_big$distance)/1000
  brum_lsoas$total_park_size_1km[i] = sum(parks_i_1km$Square_Meters)/1e6
}

num_parks <- plot_map(
  brum_lsoas,
  value_header = "num_parks_1km",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Number of Parks within 1km",
  style = "cont"
)
num_parks
save_map(num_parks, "output/park-access/num_parks_1km.png")

play_parks <- plot_map(
  brum_lsoas,
  value_header = "num_play_areas_1km",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Number of Parks with a Play Park within 1km",
  style = "cont"
) 
play_parks
save_map(play_parks, "output/park-access/num_play_parks_1km.png")


nearest_park <- plot_map(
  brum_lsoas,
  value_header = "dist_to_nearest_park",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Distance to Nearest Park (km) - All sizes",
  style = "cont"
) 
nearest_park
save_map(nearest_park, "output/park-access/nearest_park.png")

nearest_big_park <- plot_map(
  brum_lsoas,
  value_header = "dist_to_nearest_big_park",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Distance to Nearest Big Park (km) - 25,000 Square Meters or More",
  style = "cont"
) 
nearest_big_park
save_map(nearest_big_park, "output/park-access/nearest_big_park.png")


# Need to fix BSol.mapR breaks to get this to work
total_park_size_1km <- plot_map(
  brum_lsoas,
  value_header = "total_park_size_1km",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Total Park Space Sccessible within 1km (square meters)",
  style = "cont",
  breaks = c(0, 2e5, 4e5, 6e5, 8e5)
) 
total_park_size_1km
save_map(total_park_size_1km, "output/park-access/park_size_1km.png")