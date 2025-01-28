# Park work totals
#
# Calculating total costs from maintenance and fly tipping since 2013-14

library(readxl)
library(dplyr)

##############################################################
#                        Load data                           #
##############################################################

maint_data <- read_excel(
  "data/park-work-records/maintenance-all-years.xlsx"
  ) %>%
  mutate(
    Ward = `WARD DESCR`,
    #Asset_Type = `ASSET TYPE DESC`,
    Postcode = POSTCODE,
    # Standardise name
    Site_Name = sub(",.*", "", `SITE NAME`),
    Site_Name = stringr::str_to_title(Site_Name)
  ) %>%
  group_by(Site_Name, Postcode, Ward) %>%
  summarise(
    Total_Maint_Cost = sum(`TOTAL COST INC ON COST`)
  )

flytip_data <- read_excel(
  "data/park-work-records/fly-tipping-all-years.xlsx"
) %>%
  mutate(
    Site_Name = sub(",.*", "", FULLADDR), 
    Ward = WARDDESC,
    #Asset_Type = `ASSET TYPE DESC`,
    Postcode = POSTCD,
  ) %>%
  group_by(Site_Name, Postcode, Ward) %>%
  summarise(
    Total_Flytip_Cost = sum(`COST`)
  )

parks <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  sheet = "Park Info"
) %>%
  select(
    Site_Name, Postcode
  )

################################################################
#    Make lookup for park names in maintenance and ref data    #                        #
################################################################

# Find matches based on the name
maint_name_match <- maint_data %>%
  ungroup() %>%
  select(Site_Name) %>%
  distinct() %>%
  fuzzyjoin::stringdist_join(
    parks %>% select(Site_Name),
    by='Site_Name', #match based on team
    mode='left', #use left join
    method = "jw", #use jw distance metric
    max_dist=99, 
    distance_col='dist') %>%
  group_by(Site_Name.x) %>%
  slice_min(order_by=dist, n=1) %>%
  mutate(
    Site_Name.y = case_when(
      dist < 0.123 ~ Site_Name.y,
      TRUE ~ NA
    )
  ) %>%
  select(
    Site_Name.x, Site_Name.y
  )

# Find matches based on postcode
maint_pc_match <- maint_data %>%
  ungroup() %>%
  # Select only first row for each postcode
  select(Site_Name, Postcode) %>%
  group_by(Postcode) %>%
  mutate(
    nrow = row_number()
  ) %>%
  filter(nrow == 1) %>%
  left_join(
    parks,
    by = join_by("Postcode")
    ) %>%
  ungroup() %>%
  select(
    Site_Name.x, Site_Name.y
  )

maint_name_match_lookup <- rbind(
  maint_name_match,
  maint_pc_match
  ) %>%
  distinct() %>%
  # Remove matchings that are clearly false 
  filter(
    !((Site_Name.x == "All Saints Park") & Site_Name.y == "Bacchus Road Public Open Space"),
    !((Site_Name.x == "Belchers Brook") & Site_Name.y == "Maybank Play Area"),
    !((Site_Name.x == "Cole Bank Playing Field") & Site_Name.y == "John Morris Jones Walkway"),
    !((Site_Name.x == "Conolly Drive Play Area") & Site_Name.y == "Rubery Hill Hospital Burial Ground"),
    !((Site_Name.x == "Finchley Road Rec Ground") & Site_Name.y == "Warren Farm P.o.s. H/a-1209"),
    !((Site_Name.x == "Long Saw Drive Pos") & Site_Name.y == "Rover Park"),
    !((Site_Name.x == "Finchley Road Rec Ground") & Site_Name.y == "Warren Farm P.o.s. H/a-1209"),
    !((Site_Name.x == "Parklands Pos") & Site_Name.y == "Scholars Close Play Area")
  ) %>%
  # Find all cases where Site_Name.x matched to more than one Site_Name.y
  group_by(Site_Name.x) %>%
  mutate(
    doubles = max(row_number())
  ) %>%
  # Remove NAs on Site_Name.y that aren't in a double
  filter(
    !(is.na(Site_Name.y) & doubles > 1)
  ) %>%
  select(
    -doubles
  ) %>%
  # Recalculate doubles
  mutate(
    doubles = max(row_number())
  ) %>%
  rename(
    maintenance_name = Site_Name.x,
    park_name = Site_Name.y
  )%>%
  arrange(
    desc(doubles), park_name
  )

################################################################
#    Make lookup for park names in fly tipping and ref data     #                        #
################################################################

flytip_name_match <- flytip_data %>%
  ungroup() %>%
  select(Site_Name) %>%
  distinct() %>%
  fuzzyjoin::stringdist_join(
    parks %>% select(Site_Name),
    by='Site_Name', #match based on team
    mode='left', #use left join
    method = "jw", #use jw distance metric
    max_dist=99, 
    distance_col='dist') %>%
  group_by(Site_Name.x) %>%
  slice_min(order_by=dist, n=1) %>%
  mutate(
    Site_Name.y = case_when(
      dist < 0.213 ~ Site_Name.y,
      TRUE ~ NA
    )
  ) %>%
  select(
    Site_Name.x, Site_Name.y
  )

# Find matches based on postcode
flytip_pc_match <- flytip_data %>%
  ungroup() %>%
  # Select only first row for each postcode
  select(Site_Name, Postcode) %>%
  group_by(Postcode) %>%
  mutate(
    nrow = row_number()
  ) %>%
  filter(nrow == 1) %>%
  left_join(
    parks,
    by = join_by("Postcode")
  ) %>%
  ungroup() %>%
  select(
    Site_Name.x, Site_Name.y
  )




flytip_name_match_lookup <- rbind(
  flytip_name_match,
  flytip_pc_match
) %>%
  distinct() %>%
  # Remove matchings that are clearly false 
  filter(
    !((Site_Name.x == "Chadbrook Walkway") & Site_Name.y == "Harborne Walkway"),
    !((Site_Name.x == "Daffodil Park") & Site_Name.y == "Rea Road Public Open Space"),
    !((Site_Name.x == "John Morris Jones Walkway") & Site_Name.y ==  "Sarehole Mill Recreation Ground"),
    !((Site_Name.x == "Maybank Play Area") & Site_Name.y == "Belchers Brook"),
    !((Site_Name.x == "Rover Park") & Site_Name.y == "Long Saw Drive Pos"),
    !((Site_Name.x == "Warren Farm P.o.s.") & Site_Name.y == "Finchley Road Rec Ground"),
    !((Site_Name.x == "Rea Valley (cartland To Dogpool Ln)") & Site_Name.y == "Rea Valley (dogpool To Cannon Hl)")
  ) %>%
  # Find all cases where Site_Name.x matched to more than one Site_Name.y
  group_by(Site_Name.x) %>%
  mutate(
    doubles = max(row_number())
  ) %>%
  # Remove NAs on Site_Name.y that aren't in a double
  filter(
    !(is.na(Site_Name.y) & doubles > 1)
  ) %>%
  select(
    -doubles
  ) %>%
  # Recalculate doubles
  mutate(
    doubles = max(row_number())
  ) %>%
  rename(
    flytip_name = Site_Name.x,
    park_name = Site_Name.y
  ) %>%
  arrange(
    desc(doubles), park_name
  )

#######################################################################
#                    Save name matching lookups                       #
#######################################################################

lookup_check <- list(
  "maintenance" = maint_name_match_lookup,
  "fly-tipping" = flytip_name_match_lookup
)

writexl::write_xlsx(
  lookup_check,
  "data/park-name-match.xlsx"
  )