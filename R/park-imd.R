library(readxl)
library(BSol.mapR)

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
#    Estimate number of people within 10 minutes of each park postcode     #
############################################################################

wm_postcodes <- read.csv(
  "../data/West Midlands postcodes.csv"
) %>%
  mutate(
    LSOA21 = LSOA21.Code
  ) %>%
  select(
    Postcode, Longitude, Latitude, LSOA21
  )

LSOA21_postcode_counts <- wm_postcodes %>%
  select(LSOA21, Postcode) %>%
  distinct() %>%
  count(LSOA21) %>%
  rename(Total_Postcodes = n)


park_coords <- park_postcodes %>%
  left_join(
    wm_postcodes,
    by = join_by("Postcode")
  )

# Park index
i = 13

# Get all postcodes in each LSOA within 10 minutes walking distance
LSOA_coverage <- purrr::map2_dfr(
  park_coords$Latitude[i],
  park_coords$Longitude[i],
  ~spatialrisk::points_in_circle(wm_postcodes, .y, .x,
                                 lon = Longitude,
                                 lat = Latitude,
                                 radius = dist_10_min_walk_km*1000)) %>%
  # count number of postcodes within each LSOA
  count(LSOA21) %>%
  # Join to total number of postcodes in each LSOA
  left_join(
    LSOA21_postcode_counts,
    by = join_by("LSOA21")
  ) %>%
  # Calculate percentage of LSOA postcodes within 10 minutes walk
  mutate(
    overlap_frac = n / Total_Postcodes,
    overlap_perc = 100 * overlap_frac
  )

# Plot postcode coverage percentage
plot_map(
  LSOA_coverage,
  value_header = "overlap_perc",
  map_type = "LSOA21",
  area_name = "Birmingham",
  fill_missing = 0,
  style = "cont"
)
