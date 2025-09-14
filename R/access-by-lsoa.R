# Investigating Park Access by LSOA
library(BSol.mapR)
library(dplyr)
library(readxl)
library(ggplot2)

lsoa_access_path <- "output/data/park-access-by-lsoa.xlsx"

# Load Birmingham postcodes
brum_postcodes <- read.csv("data/West Midlands postcodes.csv") %>%
  filter(
    In.Use. == "Yes",
    District == "Birmingham"
  ) %>%
  rename(LSOA21 = LSOA21.Code) %>%
  select(
    Postcode,
    Latitude,
    Longitude,
    LSOA21
  ) %>%
  mutate(
    num_parks_1km = NA,
    num_play_areas_1km = NA,
    dist_to_nearest_park = NA,
    dist_to_nearest_big_park = NA,
    total_park_size_1km = NA
  )


# Load park info
park_info <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  "Park Info"
) %>%
  select(
    Old_Site_Ref, Site_Name, Square_Meters
  ) %>%
  inner_join(
    read_excel(
      "data/park_multi_access_info_2024.xlsx",
      "Play Parks"
    ),
    by = join_by("Old_Site_Ref")
  ) 

# Load Parks data
park_coords <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  "Park Locations"
) %>%
  filter(
    # Filter for 376
    Site_Name %in% park_info$Site_Name
  )

################################################################################
#                     Get park info for each postcode                          #
################################################################################

if (!file.exists(lsoa_access_path)) {
  
  
  # Loop over all LSOAs
  for (i in 1:nrow(brum_postcodes)) { 
    # Find all parks within 1km
    parks_i <- purrr::map2_dfr(
      brum_postcodes$Latitude[i],
      brum_postcodes$Longitude[i],
      ~spatialrisk::points_in_circle(park_coords, .y, .x,
                                     lon = Longitude,
                                     lat = Latitude,
                                     radius = 10000)) %>%
      group_by(Site_Name) %>%
      summarise(
        distance = min(distance_m)
      ) %>%
      left_join(
        park_info,
        by = "Site_Name"
      )
    
    parks_i_1km <- parks_i %>%
      filter(
        distance < 1000
      )
    
    parks_i_big <- parks_i %>%
      filter(
        Square_Meters > 100000
      )
    
    brum_postcodes$num_parks_1km[i] = nrow(parks_i_1km)
    brum_postcodes$num_play_areas_1km[i] = sum(parks_i_1km$Play_Park)
    brum_postcodes$dist_to_nearest_park[i] = min(parks_i$distance)/1000
    brum_postcodes$dist_to_nearest_big_park[i] = min(parks_i_big$distance)/1000
    brum_postcodes$total_park_size_1km[i] = sum(parks_i_1km$Square_Meters)/1e6
    
    if (i %% 1000 == 0) {
      print(paste(
        i, "of", nrow(brum_postcodes), "complete"
      ))
    }
  }
  
  ########################################################
  #                Aggregate to LSOA                     #
  ########################################################
  
  # Average all postcode-level values
  brum_lsoas <- brum_postcodes %>%
    group_by(LSOA21) %>%
    summarise(
      num_parks_1km = mean(num_parks_1km),
      num_play_areas_1km = mean(num_play_areas_1km),
      dist_to_nearest_park = mean(dist_to_nearest_park),
      dist_to_nearest_big_park = mean(dist_to_nearest_big_park),
      total_park_size_1km = mean(total_park_size_1km),
    )
  
  writexl::write_xlsx(brum_lsoas, lsoa_access_path)

} else {
  brum_lsoas <- read_excel(lsoa_access_path)
}


################################################################################
#                                Mapping                                       #
################################################################################

num_parks <- plot_map(
  brum_lsoas,
  value_header = "num_parks_1km",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Average Number of Parks within 1km",
  style = "cont"
)
num_parks
save_map(num_parks, "output/park-access/num_parks_1km.png")

play_parks <- plot_map(
  brum_lsoas,
  value_header = "num_play_areas_1km",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Average Number of Parks with a Play Park within 1km",
  style = "cont"
)
play_parks
save_map(play_parks, "output/park-access/num_play_parks_1km.png")


nearest_park <- plot_map(
  brum_lsoas,
  value_header = "dist_to_nearest_park",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Average Distance to Nearest Park (km) - All sizes",
  style = "cont"
)
nearest_park
save_map(nearest_park, "output/park-access/nearest_park.png")

nearest_big_park <- plot_map(
  brum_lsoas,
  value_header = "dist_to_nearest_big_park",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Average Distance to Nearest Big Park (km) - 10 hectares or More",
  style = "cont"
)
nearest_big_park
save_map(nearest_big_park, "output/park-access/nearest_big_park.png")


# Need to fix BSol.mapR breaks to get this to work
total_park_size_1km <- plot_map(
  brum_lsoas,
  value_header = "total_park_size_1km",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = "Average Combined Park Space Accessible within 1km (square meters)",
  style = "cont",
  breaks = c(0, 2e5, 4e5, 6e5, 8e5)
)
total_park_size_1km
save_map(total_park_size_1km, "output/park-access/park_size_1km.png")


################################################################################
#                             Analyse by IMD                                   #
################################################################################

brum_lsoas <- brum_lsoas %>%
  left_join(
    read_excel("data/demographics/IMD by LSOA 2021.xlsx"),
    by = join_by("LSOA21")
  )
  
access_by_IMD <- brum_lsoas %>%
  group_by(
    IMD_Quintile
  ) %>%
  summarize(
    `Average Number of Parks within 1km` = mean(num_parks_1km),
    `Average Number of Play Parks within 1km` = mean(num_play_areas_1km),
    `Average Distance to Nearest Park (km)` = mean(dist_to_nearest_park),
    `Average Distance to Nearest Big Park (km) (10+ hectares)` = mean(dist_to_nearest_big_park),
    `Average Combined Park Space Accessible within 1km (million square meters)` = mean(total_park_size_1km)
  ) %>%
  tidyr::pivot_longer(
    !IMD_Quintile,
    names_to = "Metric",
    values_to = "Value"
  )

imd_access_plot <- ggplot(access_by_IMD,
       aes(
         x = IMD_Quintile,
         y = Value
         )
       ) +
  geom_col(fill = "#006D7D") +
  theme_bw() +
  facet_wrap(~Metric, scale = "free_y", ncol = 1) +
  labs(
    y = "",
    x = "IMD Quintile"
  ) +
  scale_x_continuous(
    breaks = c(1,2,3,4,5),
    labels = c("1\n(Most deprived)", "2", "3", "4", "5\n(Least deprived)")
  )

imd_access_plot

ggsave(
  "output/park-access/access_by_IMD.png",
  plot = imd_access_plot, 
  width = 5,
  height = 7
)


################################################################################
#                          Analyse by ethnicity                                #
################################################################################

# Calculate global majority % in each LSOA
brum_gm <- read_excel("data/demographics/brum-eths-lsoa21.xlsx") %>%
  filter(`Ethnic group` != "Does not apply") %>%
  mutate(
    globalMajority = `Ethnic group` != "White"
  ) %>%
  group_by(
    LSOA21
  ) %>%
  summarise(
    global_majority_perc = 100 * sum(globalMajority * Observation)/sum(Observation)
  )

brum_lsoas <- brum_lsoas %>%
  left_join(
    brum_gm,
    by = join_by("LSOA21")
  ) %>%
  mutate(
    `Global Majority Resident %` = case_when(
      global_majority_perc < 20 ~ "0-20",
      global_majority_perc < 40 ~ "20-40",
      global_majority_perc < 60 ~ "40-60",
      global_majority_perc < 80 ~ "60-80",
      global_majority_perc < 100 ~ "80-100",
      TRUE ~ "Error in percentage labelling"
    )
  )


access_by_ethnicity <- brum_lsoas %>%
  group_by(
    `Global Majority Resident %`
  ) %>%
  summarize(
    `Average Number of Parks within 1km` = mean(num_parks_1km),
    `Average Number of Play Parks within 1km` = mean(num_play_areas_1km),
    `Average Distance to Nearest Park (km)` = mean(dist_to_nearest_park),
    `Average Distance to Nearest Big Park (km) (10+ hectares)` = mean(dist_to_nearest_big_park),
    `Average Combined Park Space Accessible within 1km (million square meters)` = mean(total_park_size_1km)
  ) %>%
  tidyr::pivot_longer(
    !`Global Majority Resident %`,
    names_to = "Metric",
    values_to = "Value"
  )


eth_access_plot <- ggplot(access_by_ethnicity,
                          aes(
                            x = `Global Majority Resident %`,
                            y = Value
                          )
) +
  geom_col(fill = "#006D7D") +
  theme_bw() +
  facet_wrap(~Metric, scale = "free_y", ncol = 1) +
  labs(
    y = "",
    x = "Global Majority Resident %"
  ) 

eth_access_plot

ggsave(
  "output/park-access/access_by_ethnicity.png",
  plot = eth_access_plot, 
  width = 5,
  height = 7
)
