library(readxl)
library(BSol.mapR)
source("functions.R")

park_postcodes <- read_excel(
  "../data/PARKS-WHITE-BOOK-2021.xlsx",
  sheet = "PARKS 2021"
  ) %>%
  mutate(
    Park_Name = `SITE  NAME`,
    Postcode = POSTCODE
  )

# Assuming walking speed of 5 km/hr
dist_10_min_walk_km = 10 * 5 / 60

#############################################################
#             Plot Park 10 Minute Walking Radii             #
#############################################################

map <- plot_empty_map(
  area_name = "Birmingham"
)

map <- add_radii(
  map,
  park_postcodes,
  radii = dist_10_min_walk_km,
  alpha = 0.1,
  color = "darkgreen"
)

map <- add_points(
  map,
  park_postcodes,
  color = "black"
)

map

############################################################################
#               Estimate percentage LSOA coverage from each park           #
############################################################################

park_coords <- park_postcodes %>%
  left_join(
    wm_postcodes,
    by = join_by("Postcode")
  )

# Park index
i = 2

# Get all postcodes in each LSOA within 10 minutes walking distance
LSOA_coverage <- get_LSOA_coverage(
  park_coords$Latitude[i],
  park_coords$Longitude[i],
  dist_m = dist_10_min_walk_km*1000
  )

# Plot postcode coverage percentage
plot_map(
  LSOA_coverage,
  value_header = "overlap_perc",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = park_coords$Park_Name,
  fill_missing = 0,
  style = "cont"
)

############################################################################
#               Estimate park local population demographics                #
############################################################################
park_info <- get_park_info(
  park_coords,
  index = i,
  dist_10_min_walk_km*1000)

print(park_info)
