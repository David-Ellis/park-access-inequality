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
      Old_Site_Ref, Year, Cost, Play_Park
    )
  
  # Store this data frame in a list to combine later
  maint_list[[year_i]] <- data_i
  }

# combine all tables in the list
maintenance_data <- rbindlist(maint_list) %>%
  # Calculate total yearly cost for each Site
  group_by(
    Old_Site_Ref, Year
  ) %>%
  summarise(
    Cost = sum(Cost),
    Play_Park = any(Play_Park)
  ) %>%
  # Filter data to only include data in the 2023 White Book 
  filter(
    Old_Site_Ref %in% parks$Old_Site_Ref
  )

## Filter List ##
# Get list of parks in 2013-14 and 2023-24
parks1 <- maintenance_data %>%
  filter(Year == "2013-14") %>%
  pull(Old_Site_Ref)

parks2 <- maintenance_data %>%
  filter(Year == "2023-24") %>%
  pull(Old_Site_Ref)

filter_list <- parks1[parks1 %in% parks2]
# Removes "0079POA" "0482POB" "0685POC" "0689POC" "0692PKB" 
#         "0785POB" "0889POC" "1295POC" "1298POA" "1491POG"
#         "1391POA" "1490POD" "1491POC" "1491POF" 

# Make data frame with all possible Year Old_Site_Ref combinations
maintenance_data <- expand.grid(
  unique(maintenance_data$Year),
  unique(maintenance_data$Old_Site_Ref)
) %>%
  rename(
    Year = Var1,
    Old_Site_Ref = Var2
  ) %>%
  filter(Old_Site_Ref %in% filter_list) %>%
  left_join(maintenance_data,
            by = join_by(Year, Old_Site_Ref)
  ) %>%
  mutate(Year2 = Year) %>%
  tidyr::separate(Year2, c("YearStart", NA)) %>%
  mutate(
    Cost = ifelse(is.na(Cost), 0, Cost),
    # Extract the numerical start of the year e.g. "2021-22" -> 2021
  ) %>%
  arrange(
    Old_Site_Ref, YearStart
  ) %>%
  select(Old_Site_Ref, Year, YearStart, Cost)

# Save maintenance data to an excel file
write_xlsx(maintenance_data, "data/park-work-records/maintenance-all-years.xlsx")


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
      # Convert any cost data stored as characters to numeric
      Cost = as.numeric(COST),
      # Update Old site ref column title
      Old_Site_Ref  = OLDSITEREF
    ) %>%
    # Select only needed columns
    # (Not keeping name in case different names are used for the same site)
    select(
      Old_Site_Ref, Year, Cost
    )
  
  # Store this data frame in a list to combine later
  fly_tip_list[[year_i]] <- data_i
}


# combine all tables in the list
flytip_data <- rbindlist(fly_tip_list) %>%
  # Calculate total yearly cost for each Site
  group_by(
    Old_Site_Ref, Year
  ) %>%
  summarise(
    Cost = sum(Cost),
    Flytipping_callouts = n()
  ) %>%
  # Filter data to only include data in the 2023 White Book 
  filter(
    Old_Site_Ref %in% parks$Old_Site_Ref
  )

# Make data frame with all possible Year Old_Site_Ref combinations
flytip_data <- expand.grid(
  unique(flytip_data$Year),
  unique(maintenance_data$Old_Site_Ref)
  ) %>%
  rename(
    Year = Var1,
    Old_Site_Ref = Var2
  ) %>%
  filter(Old_Site_Ref %in% filter_list) %>%
  left_join(flytip_data,
            by = join_by(Year, Old_Site_Ref)
  ) %>%
  mutate(Year2 = Year) %>%
  tidyr::separate(Year2, c("YearStart", NA)) %>%
  mutate(
    Cost = ifelse(is.na(Cost), 0, Cost),
    Flytipping_callouts = ifelse(is.na(Flytipping_callouts), 0, Flytipping_callouts),
    # Extract the numerical start of the year e.g. "2021-22" -> 2021
  ) %>%
  arrange(
    Old_Site_Ref, YearStart
  ) %>%
  select(Old_Site_Ref, Year, YearStart, Cost, Flytipping_callouts)

# Save fly tipping data to an excel file
write_xlsx(flytip_data, "data/park-work-records/fly-tipping-all-years.xlsx")

library(ggplot2)
ggplot(flytip_data, aes(Flytipping_callouts, Cost)) +
  geom_point() +
  theme_bw() +
  xlab("Number of Flytip Callouts") +
  ylab("Yearly Flytipping Cost (£)")

  
