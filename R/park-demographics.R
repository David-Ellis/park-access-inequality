library(readxl)
library(BSol.mapR)
library(writexl)
library(ggplot2)
source("functions.R")

park_coords <- read_excel(
  "../data/park_multi_access_info_2024.xlsx",
  "Park Locations"
)

# Assuming walking speed of 5 km/hr
dist_10_min_walk_km = 10 * 5 / 60

# Example park index
example_index = 2


#############################################################
#             Plot Park 10 Minute Walking Radii             #
#############################################################

map <- plot_empty_map(
  area_name = "Birmingham"
)

map <- add_radii(
  map,
  park_postcodes,
  radii = dist_10_min_walk_km,
  alpha = 0.1,
  color = "darkgreen"
)

map <- add_points(
  map,
  park_postcodes,
  color = "black"
)

map

save_map(map, "../output/figures/park_spheres.png")
############################################################################
#      Estimate percentage LSOA coverage from each park  (Example)         #
############################################################################



# Get all postcodes in each LSOA within 10 minutes walking distance
LSOA_coverage <- get_LSOA_coverage(
  park_coords,
  "Sutton Park",
  dist_m = dist_10_min_walk_km*1000
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
  index = example_index,
  dist_10_min_walk_km*1000)

print(park_info)

############################################################################
#             Estimate all park local population demographics              #
############################################################################
source("functions.R")
# Restrict to parks with valid postcodes
valid_park_coords <- park_coords %>%
  filter(
    !is.na(Longitude)
  )

valid_park_info <- get_all_park_info(
  valid_park_coords,
  dist_10_min_walk_km*1000
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

# Save output
write_xlsx(all_park_info, "../output/park_demographics.xlsx")

############################################################################
#                          Basic Visualisations                            #
############################################################################

# Population distribution
pop_plt <- ggplot(all_park_info,
              aes(
                x = Total_Population
              )) +
  geom_histogram(bins = 30, fill = "#1f77b4") +
  labs(
    y = "Number of Parks",
    x = "Estimated Population in 10-Minute Walking Distance"
  ) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 60),
    expand = c(0, 0)
  )
pop_plt
ggsave("../output/figures/pop_dist.png", plot = pop_plt,
       width = 5, height = 3, dpi = 300)

# IMD distribution
pop_plt <- ggplot(all_park_info,
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
pop_plt
ggsave("../output/figures/imd_dist.png", plot = pop_plt,
       width = 5, height = 3, dpi = 300)
