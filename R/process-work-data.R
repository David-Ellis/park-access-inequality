library(readxl)
library(writexl)
library(data.table)
library(dplyr)

# Get data lists for fly tipping
flytip_files <- list.files(pattern = "FLY TIP AWO TOTALS INC OLDSITEREF.*.xlsx", recursive = TRUE)

# Get data lists for maintenance
maintenance_files <- list.files(pattern = "BoQ.*.xlsx", recursive = TRUE)

# Load parks white book to filter maintenance and fly tipping data
parks <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  sheet = "Park Info"
)

##################################################################
#                       Fly tipping data                         #
##################################################################

# Create empty list to store all the fly tipping tables in
fly_tip_list <- list()

# Loop over all fly tipping data files
for (flytip_i in flytip_files) {
  # Get financial year from file name
  year_i <- strsplit(flytip_i, "/")[[1]][3]
  
  # Load next spreadsheet in list
  data_i <- read_excel(
    flytip_i,
    sheet = "DATA"
  ) %>%
    # Only take rows with a valid work ID (to remove totals at the bottom of the sheet)
    filter(!is.na(WORKID)) %>%
    mutate(
      # Create financial year column
      Year = year_i,
      # Extract the numerical start of the year e.g. "2021-22" -> 2021
      YearStart = as.numeric(strsplit(year_i, "-")[[1]][1]),
      # Convert any cost data stored as characters to numeric
      Cost = as.numeric(COST),
      # Update Old site ref column title
      Old_Site_Ref  = OLDSITEREF
    ) %>%
    # Select only needed columns
    # (Not keeping name in case different names are used for the same site)
    select(
      Old_Site_Ref, Year, YearStart, Cost
    )
  
  # Store this data frame in a list to combine later
  fly_tip_list[[year_i]] <- data_i
}


# combine all tables in the list
flytip_data <- rbindlist(fly_tip_list) %>%
  # Calculate total yearly cost for each Site
  group_by(
    Old_Site_Ref, Year, YearStart
  ) %>%
  summarise(
    Cost = sum(Cost)
  ) %>%
  # Filter data to only include data in the 2023 White Book 
  filter(
    Old_Site_Ref %in% parks$Old_Site_Ref
  )

# Save fly tipping data to an excel file
write_xlsx(flytip_data, "data/park-work-records/fly-tipping-all-years.xlsx")

##################################################################
#                       Maintenance data                         #
##################################################################

# Create empty list to store all the maintenance tables in
maint_list <- list()

# Loop over all maintenance data files
for (maint_i in maintenance_files) {
  # Get financial year from file name
  year_i <- strsplit(maint_i, "/")[[1]][3]
  
  # Load next spreadsheet in list
  data_i <- read_excel(
    maint_i,
    sheet = "DATA"
  ) %>%
    # Only take rows with a valid CONTRACT ID (to remove totals at the bottom of the sheet)
    filter(!is.na(CONTRACT)) %>%
    mutate(
      # Create financial year column
      Year = year_i,
      # Extract the numerical start of the year e.g. "2021-22" -> 2021
      YearStart = as.numeric(strsplit(year_i, "-")[[1]][1]),
      # Convert any cost data stored as characters to numeric
      Cost = as.numeric(`TOTAL COST INC ON COST`),
      # Update Old site ref column title
      Old_Site_Ref  = `OLD SITE REF`,
      # Is it a play park
      Play_Park = grepl("play", tolower(`WORK SCHD DESCR`))
    ) %>%
    # Select only needed columns
    # (Not keeping name in case different names are used for the same site)
    select(
      Old_Site_Ref, Year, YearStart, Cost, Play_Park
    )
  
  # Store this data frame in a list to combine later
  maint_list[[year_i]] <- data_i
}

# combine all tables in the list
maintenance_data <- rbindlist(maint_list) %>%
  # Calculate total yearly cost for each Site
  group_by(
    Old_Site_Ref, Year, YearStart
  ) %>%
  summarise(
    Cost = sum(Cost),
    Play_Park = any(Play_Park)
  ) %>%
  # Filter data to only include data in the 2023 White Book 
  filter(
    Old_Site_Ref %in% parks$Old_Site_Ref
  )

# Save maintenance data to an excel file
write_xlsx(maintenance_data, "data/park-work-records/maintenance-all-years.xlsx")


