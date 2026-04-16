library(dplyr)
library(tmap)
library(readxl)
library(janitor)
library(BSol.mapR)
library(sf)

haversine_km <- function(lon1, lat1, lon2, lat2) {
  # Convert degrees to radians
  to_rad <- pi / 180
  lon1 <- lon1 * to_rad
  lat1 <- lat1 * to_rad
  lon2 <- lon2 * to_rad
  lat2 <- lat2 * to_rad
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Earth radius in km
  R <- 6371
  
  distance <- R * c
  return(distance)
}

################################################################################
#                         Single Access Point Diagram                          #                              
################################################################################


park_long <- -1.9009912012445835 
park_lat <- 52.490261341639744

park_coords <- data.frame(
  LONG = c(park_long),
  LAT = c(park_lat)
)

park_shape <- st_as_sf(get_points_shape(park_coords))

# Tower Street Recreation Ground
postcodes <- read.csv("data/West Midlands postcodes.csv") %>%
  clean_names() %>%
  filter(
    in_use == "Yes",
    grepl("Birmingham", constituency) | constituency == "Sutton Coldfield",
    # Remove Aston main building and the prison
    !(postcode %in% c("B4 7ET", "B18 4AS")) 
  ) %>%
  rename(LAT = latitude, LONG = longitude, Postcode = postcode) %>%
  select(Postcode, LONG, LAT, lsoa21_code, population) %>%
  mutate(
    population = tidyr::replace_na(population, 0),
    distance = haversine_km(LONG, LAT, park_long, park_lat),
    inside = distance < 1
  ) 

postcode_shape <- st_as_sf(get_points_shape(postcodes))

lsoas <- postcodes %>%
  group_by(lsoa21_code) %>%
  summarise(
    num_postcodes = n(),
    pcs_inside = sum(inside),
    pop_inside = sum(inside * population),
    pop =  sum(population),
    perc_pcs_inside = 100*mean(inside),
    perc_pop_inside = 100* pop_inside / pop
    ) %>%
  mutate(
    plot_text = 
      case_when( perc_pop_inside > 0.05 ~ paste0(
        round(perc_pop_inside, 1), "%"
      ),
      TRUE ~ ""
      )
  )

radii_shape <- st_buffer(
  park_shape,
  units::set_units(
    1,
    "km"
  )
)

park_x <- 406721.9
park_y <- 288084.1

plot_radius <- 1300

bbox <- st_bbox(
  c(
    xmin = park_x - plot_radius,
    ymin = park_y - plot_radius,
    xmax = park_x + plot_radius,
    ymax = park_y + plot_radius
  ),
  crs = st_crs(postcode_shape)
)

LSOA_shape <- st_read("data/shape-files/lsoas/LSOA_2021_EW_BFC_V10.shp") %>%
  #filter(LSOA21 %in% lsoas$lsoa_code) %>%
  left_join(
    lsoas,
    by = join_by("LSOA21CD" == "lsoa21_code")
  )

postcode_shape$pop_alpha <- pmin(postcode_shape$population, 100) / 100

map <- tm_shape(
  postcode_shape, 
  bbox = bbox
  ) +
  tm_basemap("CartoDB.Positron") +
  tm_dots(
    col = NA,
    lwd = 0,
    fill = "black",
    fill_alpha = "pop_alpha",
    fill_alpha.scale = tm_scale(
      values = c(0, 1),
      label.na = NA
    )
  ) +
  tm_shape(LSOA_shape) +
  tm_fill(
   "perc_pop_inside",
    fill.scale = tm_scale_continuous(
      values = "-hcl.greens2",
      label.na = NA
    ),
   fill_alpha = 0.3 
  ) +
  tm_borders(
    col = "gray20"
  ) +
  tm_text("plot_text",
          size = 1.3,
          col = "black") +
  tm_shape(park_shape) +
  tm_dots(
    size = 0.7,
    col = "red",
    lwd = 2,
    shape = 4
  ) +
  tm_shape(radii_shape) +
  tm_polygons(fill_alpha = 0, lwd = 2.5) +
  tm_layout(legend.show = FALSE) +
  tm_credits(
    "© OpenStreetMap contributors © CARTO",
    position = c("RIGHT", "BOTTOM"),
    size = 0.6,
    frame = TRUE,
    frame.lwd = 0,
    bg = TRUE,
    bg.color = "white",
    bg.alpha = 0.5
  ) +
  tm_credits(
    "Tower Street Recreation Ground",
    position = c("LEFT", "TOP"),
    size = 1.5,
    frame = TRUE,
    frame.lwd = 0,
    bg = TRUE,
    bg.color = "white",
    bg.alpha = 0.6
  )

map

tmap_save(map, "output/figures/single-park-example.png")

################################################################################
#                        Multiple Access Point Diagram                         #
################################################################################

selly_park <- data.frame(
  LAT = c(52.4434731371218, 52.4446031654576, 52.4441565023113,
               52.4428966587723, 52.4414306136648, 52.4407510241351),
  LONG = c(-1.94928879719874, -1.94899442545806, -1.94414668660001,
           -1.94290656736681, -1.94616970941418, -1.94735345960541)
)


selly_park_shape <- st_as_sf(get_points_shape(selly_park))

sp_radii_shape <- st_buffer(
  selly_park_shape,
  units::set_units(
    1,
    "km"
  )
)

sp_outline_shape <- st_union(sp_radii_shape)

s_park_x <- (403446.5 + 403880.3) / 2
s_park_y <- (282574.2 + 283002.6) / 2

plot_radius2 <- 1500

bbox2 <- st_bbox(
  c(
    xmin = s_park_x - plot_radius2,
    ymin = s_park_y - plot_radius2,
    xmax = s_park_x + plot_radius2,
    ymax = s_park_y + plot_radius2
  ),
  crs = st_crs(postcode_shape)
)


postcodes2 <- read.csv("data/West Midlands postcodes.csv") %>%
  clean_names() %>%
  filter(
    in_use == "Yes",
    grepl("Birmingham", constituency) | constituency == "Sutton Coldfield",
    # Remove Aston main building and the prison
    !(postcode %in% c("B4 7ET", "B18 4AS")) 
  ) %>%
  rename(LAT = latitude, LONG = longitude, Postcode = postcode) %>%
  select(Postcode, LONG, LAT, lsoa21_code, population) %>%
  mutate(
    population = tidyr::replace_na(population, 0),
    d1 = haversine_km(LONG, LAT, selly_park$LONG[1], selly_park$LAT[1]),
    d2 = haversine_km(LONG, LAT, selly_park$LONG[2], selly_park$LAT[2]),
    d3 = haversine_km(LONG, LAT, selly_park$LONG[3], selly_park$LAT[3]),
    d4 = haversine_km(LONG, LAT, selly_park$LONG[4], selly_park$LAT[4]),
    d5 = haversine_km(LONG, LAT, selly_park$LONG[5], selly_park$LAT[5]),
    d6 = haversine_km(LONG, LAT, selly_park$LONG[6], selly_park$LAT[6])
  ) %>%
  rowwise() %>%
  mutate(
    distance =  min(d1,d2,d3,d4,d5,d6),
    inside = distance < 1
  ) 


lsoas2 <- postcodes2 %>%
  group_by(lsoa21_code) %>%
  summarise(
    num_postcodes = n(),
    pcs_inside = sum(inside),
    pop_inside = sum(inside * population),
    pop =  sum(population),
    perc_pcs_inside = 100*mean(inside),
    perc_pop_inside = 100* pop_inside / pop
  ) %>%
  mutate(
    plot_text = 
      case_when( perc_pop_inside > 0.05 ~ paste0(
        round(perc_pop_inside, 1), "%"
        ),
        TRUE ~ ""
        )
  )

LSOA2_shape <- st_read("data/shape-files/lsoas/LSOA_2021_EW_BFC_V10.shp") %>%
  #filter(LSOA21 %in% lsoas$lsoa_code) %>%
  left_join(
    lsoas2,
    by = join_by("LSOA21CD" == "lsoa21_code")
  )

LSOA2_shape$perc_alpha <- LSOA2_shape$perc_pop_inside/100

postcode2_shape <- st_as_sf(get_points_shape(postcodes2))

postcode2_shape$pop_alpha <- pmin(postcode2_shape$population, 100) / 100

map2 <- tm_shape(
  postcode2_shape,
  bbox = bbox2
) +
  tm_basemap("CartoDB.Positron") +
  tm_dots(
    col = NA,
    lwd = 0,
    fill = "black",
    fill_alpha = "pop_alpha",
    fill_alpha.scale = tm_scale(
      values = c(0, 1),
      label.na = NA
    )
  ) +
  tm_shape(LSOA2_shape) +
  tm_fill(
    "perc_pop_inside",
    fill.scale = tm_scale_continuous(
      values = "-hcl.greens2",
      label.na = NA
    ),
    fill_alpha = 0.3 
  ) +
  tm_borders(
    col = "gray20"
  ) +
  tm_text(
    "plot_text",
    size = 1.3,
    col = "black"
  )+
  tm_shape(selly_park_shape) + 
  tm_dots(
    size = 0.5,
    col = "red",
    lwd = 2,
    shape = 4
  ) +
  tm_shape(sp_radii_shape) +
  tm_polygons(
    fill_alpha = 0, 
    lwd = 1,
    lty = "dotted",
    col = "gray30"
    ) +
  tm_shape(sp_outline_shape) +
  tm_polygons(fill_alpha = 0, lwd = 2.5) +
  tm_layout(legend.show = FALSE) +
  tm_credits(
    "© OpenStreetMap contributors © CARTO",
    position = c("RIGHT", "BOTTOM"),
    size = 0.6,
    frame = TRUE,
    frame.lwd = 0,
    bg = TRUE,
    bg.color = "white",
    bg.alpha = 0.5
  ) +
  tm_credits(
    "Selly Park",
    position = c("LEFT", "TOP"),
    size = 1.5,
    frame = TRUE,
    frame.lwd = 0,
    bg = TRUE,
    bg.color = "white",
    bg.alpha = 0.6
  )
  
map2
tmap_save(map2, "output/figures/selly-park-example.png")