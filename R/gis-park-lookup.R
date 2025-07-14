# Code to build GIS popi to Old_Site_Ref lookup file

library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(fuzzyjoin)
library(geosphere)

################################################################################
#                      Load GIS Park Info (From Stephen)                       #
################################################################################

gis_sheets = c(
  "Parks and Gardens",
  "Natural Green Spaces",
  "Recreation Grounds"
)

# Define site names that need to be removed from lookup
remove_names <- c(
  "Ackers Trust",
  "Batchelors Farm Park",
  "Wake Green Public Playing Fields",
  "Hazelwell Road Open Space River Rea Walkway",
  "River Rea Walkway Cartland Road To Hazelwell Road",
  "Dads Lane Recreation Ground",
  "Walkers Heath Recreation Ground",
  "Sunset Park",
  "Sutton Town Hall Gardens",
  "Park Approach",
  "Holders Lane Playing Fields"
)

gis_data_list <- list()
for (sheet_i in gis_sheets) {
  data_i <- read_excel(
    "data/BCC Parks Data_08072025.xlsx",
    sheet = sheet_i
  ) %>%
    mutate(
      Site_Name_GIS = str_to_title(SITE_NAME),
      GIS_Other_Name = str_to_title(OTHER_NAMES),
      GIS_Park_Type = sheet_i,
      GIS_Long = Long,
      GIS_Lat = Lat,
      GIS_Other_Name = case_when(
        GIS_Other_Name == "<Null>" ~ NA,
        TRUE ~ GIS_Other_Name
      ),
      POPI_UID = case_when(
        POPI_UID == "<Null>" ~ NA,
        TRUE ~ POPI_UID
      )
    ) %>%
    select(
      POPI_UID, Site_Name_GIS, GIS_Other_Name, GIS_Lat, GIS_Long, GIS_Park_Type
    )
  
  gis_data_list[[sheet_i]] <- data_i
}

gis_data <- rbindlist(gis_data_list) %>%
  # Impute missing popi IDs
  mutate(
    POPI_UID = if_else(
      is.na(POPI_UID),
      paste0("no-popi-", cumsum(is.na(POPI_UID))),
      POPI_UID,
    ),
    # Fix Canon Hill Popi
    POPI_UID = case_when(
      Site_Name_GIS == "Cannon Hill Park And Queens Drive" ~ "SI/001004676",
      TRUE ~ POPI_UID
    ),
    # Fix some site names
    Site_Name_GIS = case_when(
      Site_Name_GIS == "Walkers Heath Playing Fields" ~ "Walkers Heath Sports Centre",
      Site_Name_GIS == "Cannon Hill Park And Queens Drive" ~ "Cannon Hill Park",
      Site_Name_GIS == "Queen Mother Plantation" ~ "Moor Green Sports Ground",
      TRUE ~ Site_Name_GIS
    )
  ) %>%
  # Get rid of doubles
  filter(
    !(Site_Name_GIS %in% remove_names)
  )

double_popi_ids <- gis_data %>% 
  count(POPI_UID) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>%
  pull(POPI_UID)

print(
  paste0(
    "Any doubles in GIS data: ",
    length(double_popi_ids) > 0
  )
)

# doubles<-gis_data %>%
#   filter(
#     POPI_UID %in% double_popi_ids
#   ) %>% 
#   arrange(POPI_UID) 
# 
# writexl::write_xlsx(doubles, "doubles_tmp.xlsx")
  
################################################################################
#                      Load Park Info (From Keiron)                            #
################################################################################
# Join coordinates to park postcodes and calculate distances
wm_postcodes <- read.csv("data/West Midlands postcodes.csv") %>%
  select(Postcode, Latitude, Longitude) %>%
  rename(Park_Long = Longitude,
         Park_Lat = Latitude)

# Load Old_Site_Refs for the 376 parks in the study
keep_list_376 <- read_excel(
  "data/376parks_regression1_Change_cost.xlsx"
) %>%
  select(Old_Site_Ref) %>%
  distinct() %>%
  pull(Old_Site_Ref)

park_info <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  sheet = "Park Info"
) %>%
  filter(
    Old_Site_Ref %in% keep_list_376
  ) %>%
  rename(Site_Name_Parks = Site_Name) %>%
  select(
    c(Popi_Site_Ref, Old_Site_Ref, Site_Name_Parks, Postcode, Square_Meters)
  ) %>%
  # Standardise site name casing
  mutate(
    Site_Name_Parks = str_to_title(Site_Name_Parks)
  ) %>%
  left_join(
    wm_postcodes,
    by = join_by("Postcode")
  )

################################################################################
#                             Join by POPI                                     #
################################################################################

joined_by_popi <- gis_data %>%
  inner_join(
    park_info,
    by = join_by(POPI_UID == Popi_Site_Ref)
  ) %>%
  mutate(
    matched_on = "Popi ID"
  )

################################################################################
#                             Join by Name                                     #
################################################################################

joined_by_name <- park_info %>% 
  select(-Popi_Site_Ref) %>% 
  mutate(
    Join_Name_Park = str_remove(
      Site_Name_Parks, 
      "Recreation Ground|Rec Ground|Public Open Space|Pos|P.o.s"
    )
  ) %>%
  stringdist_join(
    gis_data %>% 
      mutate(
        Join_Name_GIS = str_remove(
          Site_Name_GIS, 
          "Recreation Ground|Rec Ground|Public Open Space|Pos|P.o.s"
        )
      ), 
    by=join_by("Join_Name_Park" == "Join_Name_GIS"), #match based on team
    mode='inner', #use left join
    method = "jw", #use jw distance metric
    max_dist=0.2, 
    distance_col='dist') %>%
  group_by(Join_Name_Park) %>%
  slice_min(order_by=dist, n=1) %>%
  filter(
    !(Old_Site_Ref %in% joined_by_popi)
  ) %>%
  ungroup() %>% 
  mutate(
    matched_on = "Fuzzy name"
  ) %>%
  select(colnames(joined_by_popi))

lookup1 <- rbind(joined_by_popi, joined_by_name)

################################################################################
#                           Join by other name                                 #
################################################################################

joined_by_other <- park_info %>% 
  select(-Popi_Site_Ref) %>% 
  mutate(
    Join_Name_Park = str_remove(
      Site_Name_Parks, 
      "Recreation Ground|Rec Ground|Public Open Space|Pos|P.o.s"
    )
  ) %>%
  stringdist_join(
    gis_data %>% 
      mutate(
        Join_Name_GIS = str_remove(
          GIS_Other_Name, 
          "Recreation Ground|Rec Ground|Public Open Space|Pos|P.o.s"
        )
      ), 
    by=join_by("Join_Name_Park" == "Join_Name_GIS"), #match based on team
    mode='inner', #use left join
    method = "jw", #use jw distance metric
    max_dist=0.2, 
    distance_col='dist') %>%
  group_by(Join_Name_Park) %>%
  slice_min(order_by=dist, n=1) %>%
  filter(
    !(Old_Site_Ref %in% joined_by_popi)
  ) %>%
  ungroup() %>% 
  mutate(
    matched_on = "Fuzzy name"
  ) %>%
  select(colnames(joined_by_popi)) 

lookup2 <- rbind(lookup1, joined_by_other)

################################################################################
#                              Check join                                      #
################################################################################

matched_osfs <- unique(lookup2$Old_Site_Ref)

matched_popis <- unique(lookup2$POPI_UID)

print(
  paste(
    100*round(mean(park_info$Old_Site_Ref %in% matched_osfs),4), "% of Old_Site_Ref's matched"
  )
)

print(
  paste(
    100*round(mean(gis_data$POPI_UID %in% matched_popis),4), "% of Stephen's Popis matched"
  )
)



lookup2$distance_m <- distHaversine(
  p1 = cbind(lookup2$GIS_Long, lookup2$GIS_Lat),
  p2 = cbind(lookup2$Park_Long, lookup2$Park_Lat)
)

lookup3 <- lookup2 %>%
  mutate(
    `dist:radius ratio` = distance_m / (Square_Meters/pi)**(1/2)
  )

# Get GIS parks still not matched
still_missing <- gis_data %>%
  filter(!(POPI_UID %in% lookup2$POPI_UID))

# Extract parks that are maybe too far away

too_far <- lookup3 %>%
  filter(`dist:radius ratio` > 2) %>%
  select(colnames(still_missing))

lookup_withdist <- lookup3 %>%
  filter(`dist:radius ratio` <= 2) 

################################################################################
#         Find nearest park for each missing and too far GIS object            #
################################################################################

park_coords <- park_info %>%
  rename(
    Park_Postcode = Postcode,
    Nearest_Park = Site_Name_Parks) %>%
  select(
    Old_Site_Ref, Nearest_Park, Park_Postcode, Park_Lat, Park_Long
  )

missing_cross <- tidyr::crossing(
  POPI_UID = still_missing$POPI_UID,
  Old_Site_Ref = park_coords$Old_Site_Ref
) %>%
  left_join(still_missing, by = "POPI_UID") %>%
  bind_cols(park_coords[rep(1:nrow(park_coords), times = nrow(still_missing)), ])

missing_cross <- missing_cross %>%
  mutate(distance_m = distHaversine(
    cbind(GIS_Long, GIS_Lat),
    cbind(Park_Long, Park_Lat)
  ))

# For each park in still_missing, find the closest park_coords park
closest_parks_to_missing <- missing_cross %>%
  group_by(POPI_UID) %>%
  slice_min(distance_m, n = 1) %>%
  ungroup()

# If you want to merge back into still_missing:
nearest_to_missing <- still_missing %>%
  left_join(closest_parks_to_missing, by = "POPI_UID")


## Too far ## 

too_far_cross <- tidyr::crossing(
  POPI_UID = too_far$POPI_UID,
  Old_Site_Ref = park_coords$Old_Site_Ref
) %>%
  left_join(too_far, by = "POPI_UID") %>%
  bind_cols(park_coords[rep(1:nrow(park_coords), times = nrow(too_far)), ])

too_far_cross <- too_far_cross %>%
  mutate(distance_m = distHaversine(
    cbind(GIS_Long, GIS_Lat),
    cbind(Park_Long, Park_Lat)
  ))

# For each park in still_missing, find the closest park_coords park
closest_parks_too_far <- too_far_cross %>%
  group_by(POPI_UID) %>%
  slice_min(distance_m, n = 1) %>%
  ungroup()

# If you want to merge back into still_missing:
nearest_too_far <- too_far %>%
  left_join(closest_parks_too_far, by = "POPI_UID")


################################################################################
#                                 Save output                                  #
################################################################################


output <- list(
  "Draft lookup" = lookup3,
  "GIS still missing" = nearest_to_missing,
  "Maybe too far" = too_far
)

writexl::write_xlsx(output, "data/gis-park-lookup-v2.xlsx")