library(readxl)
library(dplyr)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

wm_postcodes <- read.csv(
  "../data/West Midlands postcodes.csv"
) %>%
  mutate(
    LSOA21 = LSOA21.Code,
    IMD_rank = Index.of.Multiple.Deprivation
  ) %>%
  select(
    Postcode, Longitude, Latitude, LSOA21, IMD_rank
  )

LSOA21_IMDs <- wm_postcodes %>%
  group_by(LSOA21) %>%
  summarise(
    IMD_rank = Mode(IMD_rank)
  )

# Number of each broad ethnic
wm_eths <- read.csv(
  "../data/Census21_WestMids_LSOA_ethnicity.csv"
) %>%
  rename(
    LSOA21 = Lower.layer.Super.Output.Areas.Code,
    LSOA_Name = Lower.layer.Super.Output.Areas,
    Ethnic_Group = Ethnic.group..6.categories.
  ) %>%
  select(
    LSOA21, LSOA_Name, Ethnic_Group, Observation
  ) %>%
  # Remove `Does not apply` ethnic group since all zero
  filter(Ethnic_Group != "Does not apply" )
wm_eths <- wm_eths%>%
  # Calculate population total
  rbind(
    wm_eths %>%
      group_by(LSOA21, LSOA_Name) %>%
      summarise(Observation = sum(Observation)) %>%
      mutate(Ethnic_Group = "Total_Population")
  ) %>%
  arrange(LSOA21)


LSOA21_postcode_counts <- wm_postcodes %>%
  select(LSOA21, Postcode) %>%
  distinct() %>%
  count(LSOA21) %>%
  rename(Total_Postcodes = n)

get_LSOA_coverage <- function(
  # Data frame for all parks including longitude and latitude of each
  # entrance point
  all_park_data, 
  park_name,
  dist_m
) {
  
  # Filter for selected park
  park_data <- all_park_data %>%
    filter(Site_Name == park_name)
  
  n_points <- nrow(park_data)
  
  all_postcodes <- list()
  # Look over all coordinates to get all overlapping postcodes
  for (i in 1:n_points) {
    # Get all postcodes in each LSOA within 10 minutes walking distance
    postcodes_i <- purrr::map2_dfr(
      park_data$Latitude[i],
      park_data$Longitude[i],
      ~spatialrisk::points_in_circle(wm_postcodes, .y, .x,
                                     lon = Longitude,
                                     lat = Latitude,
                                     radius = dist_m))
    all_postcodes[[i]] <- postcodes_i %>%
      select(Postcode, LSOA21)
  }
  LSOA_coverage <- data.table::rbindlist(all_postcodes) %>%
    # Remove double counted postcodes
    distinct() %>%
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

  # Check for unmatched rows
  if (any(is.na(LSOA_coverage$Total_Postcodes))) {
    stop("Error: Not all rows in the left table have a match in the right table.")
  }

  return(LSOA_coverage)
}


get_park_LSOA_pops <- function(
    LSOA_coverage
    ) {

  LSOA_pops <- LSOA_coverage %>%
    left_join(
      wm_eths,
      by = join_by("LSOA21"),
      relationship = "one-to-many"
    ) %>% mutate(
      Est_Num = overlap_frac * Observation
    ) %>%
    select(-Observation)

  # Check for unmatched rows
  if (any(is.na(LSOA_pops$Est_Num))) {
    stop("Error: Not all rows in the left table have a match in the right table.")
  }


  return(LSOA_pops)
}

get_park_pops <- function(
    LSOA_pops
) {

  park_pops <- LSOA_pops %>%
    group_by(Ethnic_Group) %>%
    summarise(
      Est_Num = sum(Est_Num)
      )

  return(park_pops)

}

get_park_imd <- function(LSOA_pops) {
  park_LSOA_IMDs <- LSOA_pops %>%
    filter(Ethnic_Group == "Total_Population") %>%
    left_join(
      LSOA21_IMDs,
      by = join_by("LSOA21")
    )
  # Check for unmatched rows
  if (any(is.na(park_LSOA_IMDs$IMD_rank))) {
    stop("Error: Not all rows in the left table have a match in the right table.")
  }
  park_IMD <- park_LSOA_IMDs %>%
    summarise(
      IMD_rank = sum(IMD_rank * Est_Num ) / sum(Est_Num)
    ) %>%
    mutate(
      IMD_quintile =  floor(5 * IMD_rank/32844 + 1),
      IMD_decile =  floor(10 * IMD_rank/32844 + 1)
    ) %>%
    tidyr::pivot_longer(
      c(IMD_rank, IMD_quintile, IMD_decile),
      names_to = "IMD_Metric",
      values_to = "Value"
    )

  return(park_IMD)
}

get_park_info <- function(
    park_df,
    park_name,
    dist_m
) {
  
  LSOA_coverage <- get_LSOA_coverage(
    park_df,
    park_name,
    dist_m = dist_m
  )

  park_LSOA_pops <- get_park_LSOA_pops(LSOA_coverage)

  park_imd <- get_park_imd(park_LSOA_pops) %>%
    rename(
      Metric = IMD_Metric,
      Value = Value
    ) %>%
    mutate(
      Value = round(Value, 1)
    )

  park_pops <- get_park_pops(park_LSOA_pops) %>%
    rename(
      Metric = Ethnic_Group,
      Value = Est_Num
      ) %>%
    mutate(
      Value = round(Value, 1)
    )

  output_df <- data.frame(
    Metric = c("Site_Name"),
    Value = c(park_name)
    ) %>%
    rbind(park_imd) %>%
    rbind(park_pops)

    return(output_df)
}

get_all_park_info <- function(
    park_df,
    dist_m,
    verbose = F
) {
  df_list <- list()

  
  park_names <- unique(park_df$Site_Name)
  
  for (name_i in park_names) {


    df_list[[name_i]] <- get_park_info(
      park_df,
      name_i,
      dist_m
    ) %>%
      tidyr::pivot_wider(
        names_from = Metric,
        values_from = Value
      )
    
    if (verbose) {
      print(paste(
        name_i, ncol(df_list[[name_i]]))
      )
    }
  }
  
  
  output_df <- data.table::rbindlist(df_list) %>%
    # Convert numeric data back to numeric
    mutate(
      across(c(IMD_rank,IMD_quintile,IMD_decile,
               `Total_Population`,
               `Asian, Asian British or Asian Welsh`,
               `Black, Black British, Black Welsh, Caribbean or African`,
               `Mixed or Multiple ethnic groups`,
               `Other ethnic group`,
               White),
             as.numeric
             )
    ) %>%
    select(
      Site_Name, 
      IMD_rank, IMD_quintile, IMD_decile, 
      Total_Population,
      `Asian, Asian British or Asian Welsh`,
      `Black, Black British, Black Welsh, Caribbean or African`,
      `Mixed or Multiple ethnic groups`,
      `Other ethnic group`,
      White
    )
  return(output_df)
}
