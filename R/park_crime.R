# Collating park crime reporting data from 3 sources:
#   1 - crime data processed by GIS team for 223 parks with polygon data 
#      (237 objects relating to 223 parks)
#   2 - crime data for 134 parks without polygon data that can be approximated
#     using a fixed radius
#   3 - crime data for the 19 parks with multiple access points that we create
#     our own polygons for

################################################################################
#       1 - Loading and aggregating GIS team's crime data (237 -> 223)         #
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


################################################################################
#           3 - Estimating crimes reported multi-access parks (19)             #
################################################################################


################################################################################
#                            Combine data and save                             #
################################################################################