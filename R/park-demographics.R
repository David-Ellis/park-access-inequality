library(readxl)
library(BSol.mapR)
library(writexl)
library(ggplot2)
source("R/functions.R")

park_coords <- read_excel(
  "data/park_multi_access_info_2024.xlsx",
  "Park Locations"
)

# Using 1km reference distance
distance = 1

# Example park index
example_index = 2

#############################################################
#             Plot Park 10 Minute Walking Radii             #
#############################################################

map <- plot_empty_map(
  area_name = "Birmingham"
)

# map <- add_radii(
#   map,
#   park_coords,
#   radii = distance,
#   alpha = 0.1,
#   color = "darkgreen"
# )

map <- add_points(
  map,
  park_coords,
  color = "black"
)

map

save_map(map, "output/figures/park_spheres.png")
############################################################################
#      Estimate percentage LSOA coverage from each park  (Example)         #
############################################################################



# Get all postcodes in each LSOA within 10 minutes walking distance
LSOA_coverage <- get_LSOA_coverage(
  park_coords,
  "Sutton Park",
  dist_m = distance*1000
)

# Plot postcode coverage percentage
map <- plot_map(
  LSOA_coverage,
  value_header = "overlap_perc",
  map_type = "LSOA21",
  area_name = "Birmingham",
  map_title = park_coords$Site_Name,
  fill_missing = 0,
  style = "cont"
)
map
############################################################################
#                Estimate park local demographics (Example)                #
############################################################################

park_info <- get_park_info(
  park_coords,
  park_name = "Sutton Park",
  distance*1000)

print(park_info)

############################################################################
#             Estimate all park local population demographics              #
############################################################################

# Restrict to parks with valid postcodes
valid_park_coords <- park_coords %>%
  filter(
    !is.na(Longitude)
  )
source("R/functions.R")
valid_park_info <- get_all_park_info(
  valid_park_coords,
  distance*1000
  )


get_park_info(
  valid_park_coords,
  "Rectory Park",
  distance*1000
)
# Create NA dataframe for parks with invalid postcodes
invalid_park_info <- park_coords %>%
  filter(
    is.na(Longitude)
  ) %>%
  select(
    Site_Name, 
  ) %>%
  mutate(
    !!!setNames(
      rep(list(NA),
          length(colnames(valid_park_info)[2:10])),
      colnames(valid_park_info)[2:10])
    )

# combine valid and invalid park data
all_park_info <- rbind(
  valid_park_info,
  invalid_park_info
  )

head(all_park_info)

# Add Old_Site_Ref back in
all_park_info <- all_park_info %>%
  left_join(
    read_excel(
      "data/park_multi_access_info_2024.xlsx",
      "Park Info"
    ) %>%
      select(Site_Name, Old_Site_Ref),
    by = join_by("Site_Name")
  ) %>%
  select(
    Site_Name, Old_Site_Ref, everything()
  )
  
  
  


# Save output
write_xlsx(all_park_info, "output/park_demographics.xlsx")

############################################################################
#                          Basic Visualisations                            #
############################################################################

# Population distribution
dist_distrib <- ggplot(all_park_info,
              aes(
                x = Total_Population
              )) +
  geom_histogram(bins = 30, fill = "#1f77b4") +
  labs(
    y = "Number of Parks",
    x = "Estimated Population within 1km"
  ) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 60),
    expand = c(0, 0)
  )
dist_distrib
ggsave("output/figures/pop_dist.png", plot = dist_distrib,
       width = 5, height = 3, dpi = 300)

# IMD distribution
IMD_dist_plt <- ggplot(all_park_info,
                  aes(
                    x = IMD_decile
                  )) +
  geom_bar(fill = "#1f77b4") +
  labs(
    y = "Number of Parks",
    x = "IMD Decile"
  ) +
  theme_bw() +
  scale_x_continuous(
    breaks = 1:10,
    labels = c("1\n(Most Deprived)", "2", "3", "4", "5",
               "6", "7", "8", "9", "10\n(Least Deprived)"),
    limits = c(0.5,10.5),
    expand  = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0, 150),
    expand = c(0, 0)
  )
IMD_dist_plt
ggsave("output/figures/imd_dist.png", plot = IMD_dist_plt,
       width = 5, height = 3, dpi = 300)
