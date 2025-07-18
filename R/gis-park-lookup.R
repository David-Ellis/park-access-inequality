# Code to build GIS popi to Old_Site_Ref lookup file

library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(fuzzyjoin)
library(geosphere)

################################################################################
#                           Manual lookup list                                 #
################################################################################
	

# Site name not strictly needed, but makes the data frame more human-readable
manual_lookup <- data.frame(
  Site_Name_GIS = c(
    "Glebe Farm Recreation Ground",
    "Hill Hook Local Nature Reserve",
    "Sheldon Country Park",
    "Shard End Public Open Space",
    "Weoley Castle Walkway",
    "Perry Park And Alexander Stadium",
    "Hollymoor Park",
    "Cocks Moors Woods Open Space",
    "Rea Road Open Space River Rea Walkway",
    "Sommerfield Road Open Space",
    "Hill Hook Adjacent Woodland",
    "Rover Park",
    "New Hall Valley Country Park",
    "Highcroft Park",
    "Monyhull Village Green"
    
    ),
  POPI_UID = c(
    "SI/001002374", 
    "SI/001003517", # Park Popi is SI/001003516 so just 1 off
    "SI/001000869", # Park Popi is SI/001000859 (one digit different)
    "no-popi-18",
    "no-popi-20",
    "SI/001001136",
    "SI/001001308", # Park popi is SI/001001301
    "SI/001002900", # Park popi is SI/001002884
    "no-popi-13",
    "no-popi-19",   # Park popi is SI/001000066
    "no-popi-9",    # Park popi is SI/001003516
    "SI/001001302", # Park popi is SI/001006045
    "SI/001003989", # Park popi is SI/001003860
    "SI/001004210", # Park popi is SI/001004782
    "no-popi-26"    # Park popi is SI/001002830
    ),
  Old_Site_Ref = c(
    "1488POA", 
    "1000POB", # Hill Hook Nature Reserve
    "1584CPA", 
    "1587POA", # Gressel Lane Recreation Ground
    "0282POA", # Castle Walkway
    "0994POB", # Perry Park
    "0078PKA", # Birmingham Great Park
    "0879POA",
    "0178POC", # Rea Road Public Open Space
    "0083CPA", # Woodgate Valley Country Park
    "1000POB", # Hill Hook Nature Reserve
    "0078POA", # Rover Park (But same postcode as Long Saw Drive Pos)
    "1394CPA",
    "0991POA", # Highcroft Public Open Space
    "0779POA"  # The Dell P.o.s
    )
  
)

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
  # ^ These two share a popi ID with ANOTHER shape file so have to be removed
  "Dads Lane Recreation Ground",
  "Walkers Heath Recreation Ground",
  "Sunset Park",
  "Sutton Town Hall Gardens",
  "Park Approach",
  "Holders Lane Playing Fields",
  "Callowbrook Recreation Ground",
  "Vesey Memorial Gardens", # Not a Park
  "Egghill Park", # Not in 376
  "Rubery Cutting", # Looks like small patch of grass
  "Corisande Walkway", # Looks like a random alleyway
  "Hope Gardens", # Doesn't appear to be a BCC park
  "Woodgate Valley Country Park Lye Close Lane-Kitwell Lane",
  # Can't tell if it's a park, but it doesn't connect to Woodgate Vally CP
  "St Thomas Closed Burial Ground", # graveyard
  "St Pauls Closed Burial Ground", # graveyard
  "Long Nuke Road Sports Ground", # Looks fenced up on Google Maps
  "Blakesley Sports Ground", # Possible private sports ground
  "Hilltop And Manwood Public Open Space", # Coords look like private golf course
  "Laurel Road Sports Ground", # private sports ground
  "The Fields Millennium Village Green", # Doesn't seem to be in park data
  "Lifford Lane Woodland", # Not accessible to public
  "Moseley Village Green", # Patch of grass
  "Highfield Hall Playing Fields", # Connected to community centre - not BCC park
  "Masefield Hall And Square", # Not a park
  "Lifford Reservoir", # Not in White Book
  "Oakwood Road Coppice", # Small wooded area - not park
  "Maney Hill Gardens", # Nice patch of grass - not a park
  "Erdington Playing Fields", # Not in 376
  "Five Ways Gardens", # Not a park
  "Hilltop And Manwood Sports Pitch", # Someone's mowing the grass - apparently not us
  "Priory Fields", # Nature reserve
  "Reaside Crescent East", # Small patch of grass
  "Monyhull Grange POS", # Not in 376
  "Oaklands Sports And Social Club", # Looks privately managed
  "Park Way Sports Pitch", # Private artificial turf pitch
  "Bromwich Wood", # Not a maintained park
  #"Mill Lane Open Space River Rea Walkway", # Doesn't appear to be a park
  "Bell Lane Recreation Ground", # Parkish looking grass but not in White Book
  "Brandwood Pocket Park And Pool", # Shares name with "Brandwood Pocket Park"
  # But looks like nature reserve/pond 
  "Ley Hill Road Coppice", # Wooded area
  "Centre Park", # Not in 376
  "Woodgate Valley Country Park Stanmore Grove", # Walkway - not a park
  "Ormond Road Public Open Space", # In Rubery not Birmingham
  "Birmingham Great Park Reservoir", # Doesn't seem to be connected to a park
  "River Rea Walkway Frankley" # Center of the shape file is in someones garden?
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
    # Add no-popi ID BEFORE filtering (so they don't change)
    POPI_UID = if_else(
      is.na(POPI_UID),
      paste0("no-popi-", cumsum(is.na(POPI_UID))),
      POPI_UID,
    ),
    # Fix Canon Hill Popi
    POPI_UID = case_when(
      Site_Name_GIS == "Cannon Hill Park And Queens Drive" ~ "SI/001004676",
      Site_Name_GIS == "The Dell" ~ "SI/001002830",
      Site_Name_GIS == "The Dell" & GIS_Other_Name == "Bells Farm Pos" ~ "SI/001002830",
      TRUE ~ POPI_UID
    ),
    # Fix some site names
    Site_Name_GIS = case_when(
      Site_Name_GIS == "Walkers Heath Playing Fields" ~ "Walkers Heath Sports Centre",
      Site_Name_GIS == "Cannon Hill Park And Queens Drive" ~ "Cannon Hill Park",
      Site_Name_GIS == "Queen Mother Plantation" ~ "Moor Green Sports Ground",
      Site_Name_GIS == "Formans Road Nature Area Shire Country Park" ~ "Burbury Brickworks River Walk",
      Site_Name_GIS == "Hay Barn Recreation Ground" ~ "Berkeley Rd Recreation Ground",
      Site_Name_GIS == "Farnborough Road Playing Fields" ~ "Castle Vale Playing Field",
      Site_Name_GIS == "Selcroft Avenue Park" ~ "West Boulevard/Selcroft P.o.s.",
      TRUE ~ Site_Name_GIS
    )
  ) %>%
  # Get rid of doubles
  filter(
    !(Site_Name_GIS %in% remove_names) & 
      !(Site_Name_GIS == "The Dell" & 
      GIS_Other_Name == "The Shire Country Park")
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

print("Loaded park data.")

################################################################################
#                             Join by POPI                                     #
################################################################################

joined_by_popi <- gis_data %>%
  # Don't join GIS parks in the manual lookup
  filter(
    !(POPI_UID %in% manual_lookup$POPI_UID)
  ) %>%
  inner_join(
    park_info,
    by = join_by(POPI_UID == Popi_Site_Ref)
  ) %>%
  mutate(
    matched_on = "Popi ID"
  )

print("Joined by POPI.")

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
      # Don't join GIS parks in the manual lookup
      filter(
        !(POPI_UID %in% manual_lookup$POPI_UID)
      ) %>%
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
  group_by(POPI_UID) %>%
  slice_min(order_by=dist, n=1) %>%
  filter(
    !(POPI_UID %in% joined_by_popi$POPI_UID)
  ) %>%
  ungroup() %>% 
  mutate(
    matched_on = "Fuzzy name"
  ) %>%
  select(colnames(joined_by_popi))

lookup1 <- rbind(joined_by_popi, joined_by_name)

print("Joined by Name.")
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
      # Don't join GIS parks in the manual lookup
      filter(
        !(POPI_UID %in% manual_lookup$POPI_UID)
      ) %>%
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
    !(POPI_UID %in% lookup1$POPI_UID)
  ) %>%
  ungroup() %>% 
  mutate(
    matched_on = "Fuzzy name"
  ) %>%
  select(colnames(joined_by_popi))  %>%
  filter(
    !(POPI_UID == "SI/001003262" & Old_Site_Ref == "1591POB")
  )

lookup2 <- rbind(lookup1, joined_by_other)

print("Joined by Other Name.")

################################################################################
#                         Join from manual list                                #
################################################################################

manual_lookup_joined <- manual_lookup %>%
  left_join(
    gis_data %>% select(-c(Site_Name_GIS)),
    by = join_by(POPI_UID)
    ) %>%
  left_join(
    park_info,
    by = join_by(Old_Site_Ref)
  ) %>%
  mutate(
    matched_on = "Manual"
  ) %>%
  select(
    colnames(lookup2)
  )

print("Joined from manual list.")

lookup3 <- rbind(lookup2, manual_lookup_joined)

################################################################################
#                              Check join                                      #
################################################################################

matched_osfs <- unique(lookup3$Old_Site_Ref)

matched_popis <- unique(lookup3$POPI_UID)

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

################################################################################
#                          Calculate Distances                                 #
################################################################################


lookup3$distance_m <- distHaversine(
  p1 = cbind(lookup3$GIS_Long, lookup3$GIS_Lat),
  p2 = cbind(lookup3$Park_Long, lookup3$Park_Lat)
)

lookup4 <- lookup3 %>%
  mutate(
    `dist:radius ratio` = distance_m / (Square_Meters/pi)**(1/2)
  )

# Get GIS parks still not matched
still_missing <- gis_data %>%
  filter(!(POPI_UID %in% lookup3$POPI_UID))

# Extract parks that are maybe too far away

too_far <- lookup4 %>%
  filter(
    # Only include parks that are too far apart
    `dist:radius ratio` > 2,
    # Remove parks with the same names
    Site_Name_GIS != Site_Name_Parks
    ) %>%
  rename(Matched_Park = Site_Name_Parks) %>%
  select("POPI_UID", "Site_Name_GIS", "GIS_Other_Name","GIS_Lat", "GIS_Long", 
         "Matched_Park", "distance_m","dist:radius ratio")

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

if (nrow(still_missing) > 0) {
  
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
} else {
  # Return empty data frame since none are missing
  closest_parks_to_missing <- data.frame()
}


################################################################################
#                  Find nearest parks too far from their match                 #
################################################################################


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
  ungroup() %>%
  rename(Nearest_Park_Postcode = Park_Postcode) %>%
  select(POPI_UID, Nearest_Park, Nearest_Park_Postcode)

# If you want to merge back into still_missing:
nearest_too_far <- too_far %>%
  left_join(closest_parks_too_far, by = "POPI_UID")

################################################################################
#                              Save output                                     #
################################################################################

output <- list(
  "Draft lookup" = lookup4,
  "GIS still missing" = closest_parks_to_missing,
  "Maybe too far" = nearest_too_far
)

writexl::write_xlsx(output, "data/gis-park-lookup-v6.xlsx")

################################################################################
#                      Create and save final lookup                            #
################################################################################

final_lookup <- lookup4 %>%
  select(Site_Name_GIS, POPI_UID, Site_Name_Parks, Old_Site_Ref) %>%
  # Add back in the Rea Vally shape files that were deleted
  rbind(
    rbindlist(gis_data_list) %>% 
      filter(
        Site_Name_GIS %in% c(
          "Hazelwell Road Open Space River Rea Walkway",
          "River Rea Walkway Cartland Road To Hazelwell Road"
          )
      ) %>%
      mutate(
        Site_Name_Parks = "Rea Valley (Hazelwell Rec)",
        Old_Site_Ref = "0581POB"
      ) %>%
      select(Site_Name_GIS, POPI_UID, Site_Name_Parks, Old_Site_Ref)
  ) %>%
  left_join(
    park_info %>% 
      select(Old_Site_Ref, Popi_Site_Ref),
    by = join_by("Old_Site_Ref")
  ) %>%
    rename(
      Name_GIS = Site_Name_GIS,
      POPI_GIS = POPI_UID,
      Name_Park = Site_Name_Parks,
      POPI_Park = Popi_Site_Ref
    ) %>%
  select(Name_GIS, POPI_GIS, Name_Park, POPI_Park, Old_Site_Ref) %>%
  mutate(
    Matching_POPI = POPI_GIS == POPI_Park
  )

writexl::write_xlsx(final_lookup, "data/gis-park-lookup.xlsx")