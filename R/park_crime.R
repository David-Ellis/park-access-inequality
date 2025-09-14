# Collating park crime reporting data from 3 sources:
#   1 - crime data processed by GIS team for 223 parks with polygon data 
#      (237 objects relating to 223 parks)
#   2 - crime data for 134 parks without polygon data that can be approximated
#     using a fixed radius
#   3 - crime data for the 19 parks with multiple access points that we create
#     our own polygons for

# TODO: Step 2 includes Aston Buffer P.O.S which I couldn't find. This needs to
#   be checked later.

################################################################################
#                            0 - Load Crime Data                               #
################################################################################

# Get Birmingham LSOAs using BSol.mapR 
# (https://github.com/Birmingham-and-Solihull-ICS/BSol.mapR)
Birmingham_LSOAs <- BSol.mapR::LSOA11@data %>%
  filter(Area == "Birmingham") %>%
  pull(LSOA11)

crime_data <- read.csv(
  "data/all-crime-reporting/west-mids-reported-crimes.csv"
) %>%
  # Filter for crimes reported in Birmingham
  filter(
    LSOA11 %in% Birmingham_LSOAs
  ) %>%
  group_by(
    Year, LSOA11, Longitude, Latitude, CrimeType
  ) %>%
  summarise(
    Crime_Reports = n(),
    .groups = "drop"
  )

################################################################################
#         1 - Load and Aggregate GIS team's Crime dData (237 -> 223)           #
################################################################################

gis_park_lookup <- read_excel(
  "data/gis-park-lookup.xlsx"
  ) %>%
  select(
    c(Name_GIS,	Name_Park,	Old_Site_Ref)
    ) %>%
  # Need distinct() for 'Woodview Pocket Park' since it has two different 
  # popi IDs in the GIS data
  distinct()

gis_crime <- read_excel(
  "data/BirminghamCrime__Parks_Data.xlsx",
  sheet = "BirminghamCrime__Parks"
) %>%
  # Fix the site names so they can be joined to the park-GIS lookup
  mutate(
    # Convert to title case
    Site_Name = str_to_title(SITE_NAME),
    # manual name changes
    Site_Name = case_when(
      Site_Name == "Walkers Heath Playing Fields" ~ "Walkers Heath Sports Centre",
      Site_Name == "Cannon Hill Park And Queens Drive" ~ "Cannon Hill Park",
      Site_Name == "Queen Mother Plantation" ~ "Moor Green Sports Ground",
      Site_Name == "Formans Road Nature Area Shire Country Park" ~ "Burbury Brickworks River Walk",
      Site_Name == "Hay Barn Recreation Ground" ~ "Berkeley Rd Recreation Ground",
      Site_Name == "Farnborough Road Playing Fields" ~ "Castle Vale Playing Field",
      Site_Name == "Selcroft Avenue Park" ~ "West Boulevard/Selcroft P.o.s.",
      TRUE ~ Site_Name
    )
  ) %>%
  # Remove things that aren't parks (see gis-park-lookup.R)
  filter(
    !(Site_Name %in% c("Batchelors Farm Park", "St Pauls Closed Burial Ground",
      "Wake Green Public Playing Fields", "Ackers Trust", "Blakesley Sports Ground",
      "Callowbrook Recreation Ground", "Long Nuke Road Sports Ground", 
      "Hilltop And Manwood Sports Pitch", "Park Approach", "Laurel Road Sports Ground" )
      )
    ) %>%
  # Join park-gis lookup
  left_join(
    gis_park_lookup,
    by = join_by("Site_Name" == "Name_GIS")
  ) %>%
  # Count 
  group_by(
    Year, Name_Park, Old_Site_Ref, CrimeType
  ) %>%
  rename(
    Site_Name = Name_Park
  ) %>%
  summarise(
    Crime_Reports = n(),
    .groups = "drop"
  )

# Check that all crime reporting data has an Old_Site_Ref. If not, kill code.
if (any(is.na(gis_crime$Old_Site_Ref))) {
  stop("One or more crime reporting doesn't have a matching Old_Site_Ref code.")
}

################################################################################
#         2 - Estimating crimes reported in smaller/simpler parks (134)        #
################################################################################

# Load Old_Site_Refs for the 376 parks in the study
keep_list_376 <- read_excel(
  "data/376parks_regression1_Change_cost.xlsx"
) %>%
  select(Old_Site_Ref) %>%
  distinct() %>%
  pull(Old_Site_Ref)

park_locs <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  sheet = "Park Locations"
)

small_parks <- park_locs %>%
  # Filter for parks with one one recorded access point
  count(`Site_Name`) %>%
  filter(n == 1) %>%
  # rejoin to self to get coordinates
  left_join(
    park_locs, 
    by = join_by("Site_Name")
    ) %>%
  # Join park info to get park size
  inner_join(
    read_excel(
      "data/park_multi_access_info_2024.xlsx",
      sheet = "Park Info"
    ) %>%
      select(c("Site_Name", "Old_Site_Ref", "Square_Meters")),
    by = join_by("Site_Name")
  ) %>%
  # Estimate park radius
  mutate(
    Radius_m = sqrt(Square_Meters/pi)
  ) %>%
  # Filter for parks with no GIS data
  filter(
    !(Old_Site_Ref %in% gis_park_lookup$Old_Site_Ref)
  )

small_park_crime_list <- list()

for (i in 1:nrow(small_parks)) { 
  crimes_i <- purrr::map2_dfr(
    small_parks$Latitude[i],
    small_parks$Longitude[i],
    ~spatialrisk::points_in_circle(crime_data, .y, .x,
                                   lon = Longitude,
                                   lat = Latitude,
                                   radius = small_parks$Radius_m[i])) %>%
    group_by(
      Year, CrimeType
    ) %>%
    summarise(
      Crime_Reports = sum(Crime_Reports),
      .groups = "drop"
    ) %>%
    mutate(
      Site_Name = small_parks$Site_Name[i],
      Old_Site_Ref = small_parks$Old_Site_Ref[i]
    ) %>%
    select(
      c(Year, Site_Name, Old_Site_Ref, CrimeType, Crime_Reports)
    )
  
  small_park_crime_list[[i]] <- crimes_i
}

small_park_crime <- data.table::rbindlist(small_park_crime_list)


################################################################################
#           3 - Estimating crimes reported multi-access parks (19)             #
################################################################################


################################################################################
#                            Combine data and save                             #
################################################################################ 