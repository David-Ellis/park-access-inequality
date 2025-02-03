library(readxl)
library(writexl)
library(data.table)
library(dplyr)

# Get data lists
flytip_files <- list.files(pattern = "FLY TIP AWO TOTALS INC OLDSITEREF.*.xlsx", recursive = TRUE)
maintenance_files <- list.files(pattern = "BoQ.*.xlsx", recursive = TRUE)

# Load and combine fly-tipping data
fly_tip_list <- list()
for (flytip_i in flytip_files) {
  year_i <- strsplit(flytip_i, "/")[[1]][3]
  data_i <- read_excel(
    flytip_i,
    sheet = "DATA"
  ) %>%
    filter(!is.na(WORKID)) %>%
    mutate(
      Year = year_i,
      YearStart = as.numeric(strsplit(year_i, "-")[[1]][1]),
      COST = as.numeric(COST),
    ) %>%
    select(
      # Removing dates for now because I can't figure out how to convert
      # them properly
      -any_of(c("...13", "DTCREATE", "ACTCMPLTD"))
    )
  fly_tip_list[[year_i]] <- data_i
}
# combine
flytip_data <- rbindlist(fly_tip_list)
# save
write_xlsx(flytip_data, "data/park-work-records/fly-tipping-all-years.xlsx")

# Load and combine maintenance data
maint_list <- list()
for (maint_i in maintenance_files) {
  year_i <- strsplit(maint_i, "/")[[1]][3]
  data_i <- read_excel(
    maint_i,
    sheet = "DATA"
  ) %>%
    filter(!is.na(CONTRACT)) %>%
    mutate(
      Year = year_i,
      YearStart = as.numeric(strsplit(year_i, "-")[[1]][1])
    ) %>%
    select(
     c(
       "Year", "YearStart",
       "CONTRACT", "CUST TYPE","BUDGET CODE",
       "BUDGET GROUP", "BUDGET HEAD", "BH DESCR",
       "PORTFOLIO", "COST CENTRE", "SUBJCODE",
       "OLD SITE REF", "SITE NAME", "POSTCODE",
       "CONSTITUENCY", "WARD", "WARD DESCR",
       "ASSET TYPE", "ASSET TYPE DESC", "SOR", "SOR DESCR",
       "WSKEY", "WORK SCHD", "WORK SCHD DESCR", "WORK SCHD SIMPLE",
       "RATE PER OCCASION INC ON COST", "TOTAL COST INC ON COST"
       
     )
    )
  maint_list[[year_i]] <- data_i
}

maintenance_data <- rbindlist(maint_list)
write_xlsx(maintenance_data, "data/park-work-records/maintenance-all-years.xlsx")