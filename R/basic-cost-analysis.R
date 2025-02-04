# Park work totals
#
# Calculating total costs from maintenance and fly tipping since 2013-14

library(readxl)
library(dplyr)
library(ggplot2)
##############################################################
#                        Load data                           #
##############################################################

maint_data <- read_excel(
  "data/park-work-records/maintenance-all-years.xlsx"
  ) %>%
  group_by(
    YearStart, Year
  ) %>%
  summarise(
    Cost = sum(Cost)
  ) %>%
  mutate(
    Type = "Maintenance"
  )

ft_data <- read_excel(
  "data/park-work-records/fly-tipping-all-years.xlsx"
) %>%
  group_by(
    YearStart, Year
  ) %>%
  summarise(
    Cost = sum(Cost)
  ) %>%
  mutate(
    Type = "Fly Tipping"
  )

comb_work_data <- rbind(maint_data, ft_data)

ggplot(comb_work_data, aes(x = YearStart, y = Cost/1e6, color = Type)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ylim(0, 10) +
  labs(
    y = "Yearly Cost (Millions)",
    x = "Financial Year"
    ) +
  scale_x_continuous(
    breaks = seq(2013, 2023, 2), 
    labels = paste(seq(13, 23, 2), seq(14, 24, 2), sep = "/")
      )

