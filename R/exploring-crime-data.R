# Basic look at crime reporting data 
library(dplyr)
library(ggplot2)
library(BSol.mapR)

files <- list.files(path = "data/westmid-crime-2223-2324", 
           pattern = "\\.csv$", 
           recursive = TRUE, 
           full.names = TRUE)

data_list <- list()
for (file_i in files) {
  data_list[[file_i]] <- read.csv(file_i)
}

data <- data.table::rbindlist(data_list)

crime_type_count <- data %>%
  count(Crime.type) %>%
  arrange(desc(n)) %>%
  mutate(
    per_month = n / 24
  )
crime_type_count

LSOA_count <- data %>%
  rename(
    LSOA21 = LSOA.code
  ) %>%
  count(LSOA21) %>%
  mutate(
    monthly_crime = n / 24
  )

plot_map(
  LSOA_count,
  value_header = "monthly_crime",
  map_type = "LSOA21",
  area_name = "Birmingham",
  style = "cont",
  fill_title = "Average monthly number of crimes reported (22/23 - 23/24)"
)