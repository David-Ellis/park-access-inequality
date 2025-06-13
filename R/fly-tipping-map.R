library(BSol.mapR)
library(readxl)
library(dplyr)

park_locs <- read_excel("data/park_multi_access_info_2024.xlsx") %>%
  select(c(Old_Site_Ref, Postcode))

ft_data <- read_excel(
  "data/park-work-records/fly-tipping-all-years.xlsx"
  ) %>%
  left_join(
    park_locs,
    by = join_by("Old_Site_Ref")
  )

for (year_i in 2013:2023) {
  ft_data_i <- ft_data %>%
    filter(YearStart == year_i)
  
  map_i <- plot_empty_map(
    map_title = paste0("Fly-tipping cost (", year_i,")"),
    area_name = "Birmingham"
  )
  map_i <- add_points(
    map_i,
    ft_data_i,
    size = "Cost",
  
  )
  
  save_name <- paste0(
    "output/figures/ft_maps/ft_map_",
    year_i,
    ".png"
  )
  
  save_map(map_i, save_name)
  
}