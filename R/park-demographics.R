library(readxl)
library(BSol.mapR)
library(writexl)
source("functions.R")

park_postcodes <- read_excel(
  "../data/PARKS-WHITE-BOOK-2021.xlsx",
  sheet = "PARKS 2021"
  ) %>%
  mutate(
    Park_Name = `SITE  NAME`,
    Postcode = POSTCODE
  )

park_coords <- park_postcodes %>%
  left_join(
    wm_postcodes,
    by = join_by("Postcode")
  )

# Assuming walking speed of 5 km/hr
dist_10_min_walk_km = 10 * 5 / 60

# Example park index
example_index = 2


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
#      Estimate percentage LSOA coverage from each park  (Example)         #
############################################################################

# Get all postcodes in each LSOA within 10 minutes walking distance
LSOA_coverage <- get_LSOA_coverage(
  park_coords$Latitude[example_index],
  park_coords$Longitude[example_index],
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
#                Estimate park local demographics (Example)                #
############################################################################

park_info <- get_park_info(
  park_coords,
  index = example_index,
  dist_10_min_walk_km*1000)

print(park_info)

############################################################################
#             Estimate all park local population demographics              #
############################################################################

# Restrict to parks with valid postcodes
valid_park_postcodes <- park_coords %>%
  filter(
    !is.na(Longitude)
  )

valid_park_info <- get_all_park_info(
  valid_park_postcodes,
  dist_10_min_walk_km*1000
  )

# Create NA dataframe for parks with invalid postcodes
invalid_park_info <- park_coords %>%
  filter(
    is.na(Longitude)
  ) %>%
  select(
    Park_Name, Postcode
  ) %>%
  mutate(
    !!!setNames(
      rep(list(NA),
          length(colnames(all_park_info)[2:11])),
      colnames(all_park_info)[2:11])
    )

# combine valid and invalid park data
all_park_info <- rbind(
  valid_park_info,
  invalid_park_info
  )

head(all_park_info)

# Save output
write_xlsx(all_park_info, "../output/park_demographics.xlsx")
