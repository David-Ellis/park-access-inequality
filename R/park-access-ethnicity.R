library("readxl")
library("dplyr")
library("ggplot2")

park_sizes <- read_excel(
  "../data/park_multi_access_info_2024.xlsx",
  "Park Info"
) %>%
  select(
    Site_Name,
    Area_Acres
  ) 

park_access_by_eth <- read_excel("../output/park_demographics.xlsx") %>%
  left_join(
    park_sizes,
    by = join_by("Site_Name")) %>%
  # White pop only has greatest access by area when including Sutton Park
  # filter(Park_Name != "Sutton Park") %>%
  mutate(
    Park = case_when(
      Site_Name == "Sutton Park" ~ "Sutton Park",
      TRUE ~ "Other Parks"
    )
  ) %>%
  select(
    `Asian, Asian British or Asian Welsh`,
    `Black, Black British, Black Welsh, Caribbean or African`,
    `Mixed or Multiple ethnic groups`,
    `Other ethnic group`, 
    `White`,
    #Park,
    Area_Acres
    ) %>%
  tidyr::pivot_longer(
    cols = !c(#Park, 
      Area_Acres),
    names_to = "Ethnic_Group",
    values_to = "Park_access_count") %>%
  group_by(
    Ethnic_Group#, 
    #Park
    ) %>%
  summarise(
    Total_Park_access_count = sum(
      Park_access_count, na.rm=TRUE
      ),
    Area_Access_Acres = sum(
      Park_access_count * Area_Acres, na.rm=TRUE
    )
  )

brum_eth_counts <- read.csv(
  "../data/Census21_WestMids_LSOA_ethnicity.csv"
) %>% rename(
    Ethnic_Group = Ethnic.group..6.categories.
  ) %>%
  filter(
    grepl("Birmingham", Lower.layer.Super.Output.Areas),
    Ethnic_Group != "Does not apply"
  ) %>%
  group_by(Ethnic_Group) %>%
  summarise(
    Total_Count = sum(Observation)
  )
    
avg_parks_accessible <- park_access_by_eth %>%
  left_join(
    brum_eth_counts,
    by = join_by("Ethnic_Group")
    ) %>%
  mutate(
    Ethnic_Group = stringr::str_wrap(Ethnic_Group, 18),
    Avg_Parks_Accessible = Total_Park_access_count / Total_Count,
    Avg_Area_Access_Acres = Area_Access_Acres / Total_Count,
    
  )
  


count_plt <- ggplot(
  avg_parks_accessible, 
  aes(x = Ethnic_Group, y = Avg_Parks_Accessible)
  ) +
  geom_col(fill = "#1f77b4", color = "black") +
  theme_bw() +
  labs(
    x = "",
    y = "Average Number of Parks\nwithin 10 minutes walking distance"
  ) +
  scale_y_continuous(
    limits = c(0, 6),
    expand  = c(0,0)
    )
count_plt
ggsave("../output/figures/park_count_access.png", plot = count_plt, 
       width = 6, height = 4, dpi = 300)

area_plt <- ggplot(
  avg_parks_accessible, 
  aes(x = Ethnic_Group, y = Avg_Area_Access_Acres)
) +
  geom_col(fill = "#1f77b4", color = "black") +
  theme_bw() +
  labs(
    x = "",
    y = "Average Area of Parks (Acres)\nwithin 10 minutes walking distance"
  ) +
  scale_y_continuous(
    limits = c(0, 200),
    expand  = c(0,0)
  )
area_plt
ggsave("../output/figures/park_area_access.png", plot = area_plt, 
       width = 6, height = 4, dpi = 300)