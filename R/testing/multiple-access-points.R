library(dplyr)
library(readxl)
library(writexl)
library(tmap)

############################################################################
#  Create new data file with seperate park info and access point coords    #
############################################################################

park <- read_excel("../../data/PARKS-WHITE-BOOK-2021 - multi access.xlsx",
                           sheet = "Park Info") %>%
  mutate(
    POSTCODE = gsub("\\s+", " ", POSTCODE)
  ) 
colnames(park) <- gsub("/", "", 
                       stringr::str_to_title(colnames(park)))
colnames(park) <- gsub("\\s{1,}", "_", 
                       stringr::str_to_title(colnames(park)))

wm_postcodes <- read.csv(
  "../../data/West Midlands postcodes.csv"
) %>%
  mutate(
    LSOA21 = LSOA21.Code,
    IMD_rank = Index.of.Multiple.Deprivation
  ) %>%
  select(
    Postcode, Longitude, Latitude
  )

# Assuming walking speed of 5 km/hr
dist_10_min_walk_km = 10 * 5 / 60

park_locs <- park %>% 
  select(Site_Name, Postcode, Area_Acres, Square_Meters) %>%
  left_join(
    wm_postcodes,
    by = join_by("Postcode")
  ) %>%
  arrange(desc(Area_Acres)) %>%
  mutate(
    crude_radius_km = sqrt(Square_Meters/pi)/1e3,
    Multiple_Points = crude_radius_km > dist_10_min_walk_km / 4
  ) %>%
  select(
    Site_Name, Postcode, Multiple_Points, Longitude, Latitude
  )

output_list <- list(
  "Park Info" = park,
  "Park Locations" = park_locs
)


# write_xlsx(output_list, "../../data/park_multi_access_info_2024_raw.xlsx")

############################################################################
#             Show multiple access points for Sutton Park                  #
############################################################################

park_coords <- read_excel(
  "../../data/park_multi_access_info_2024.xlsx",
  "Park Locations"
)

multi_site_parks <- park_coords %>%
  filter(Access_Point == 2) %>%
  pull(Site_Name)

for (site_i in multi_site_parks) {
  
  park_i <- park_coords%>%
    filter(Site_Name == site_i) %>%
    select(-Postcode) %>%
    rename(LONG = Longitude,
           LAT = Latitude)
  
  map <- plot_empty_map(
    area_name = "Birmingham",
    const_lines = F,
    const_names = F
  )
  
  map <- add_radii(
    map,
    park_i,
    radii = dist_10_min_walk_km,
    alpha = 0.1,
    color = "orange"
  )
  
  map <- add_points(
    map,
    park_i,
    color = "black",
    size = 0.05
  )
  
  map
  
  save_name <- paste0(
    "../../output/figures/park-access-points/",
    gsub("\\s{1,}", "_", tolower(site_i)),
    ".html"
  )
  save_map(map, save_name)
}
