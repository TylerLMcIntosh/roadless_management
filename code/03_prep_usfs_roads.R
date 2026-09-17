
library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)


dir_dats <- here("data/raw")
dir_derived <- here("data/derived")

mngmt_natl_fl <- here(dir_derived, "roadless_management_national_simplified.gpkg")

management_national <- sf::st_read(mngmt_natl_fl)

# States
states <- tigris::states() |>
  filter(! STUSPS %in% c("CO", "ID")) #remove CO and ID

# counties - for pulling TIGRIS road data
counties <- tigris::counties()

fs_road_file <- here(dir_dats, "S_USA.RoadCore_FS/S_USA.RoadCore_FS.shp")
#confirmed - fs road file is same CRS as roadless data

tiger_crs <- tigris::roads(state = "WY", county = "Teton", year = 2024) |>
  sf::st_crs()


pull_relevant_state_roads <- function(state) {
  
  # We need to operate in the crs of the roads, since transforming them
  # takes a long time (a very large number of vertices). Using tiger crs
  
  dir_roads <- here(dir_derived, "roads")
  dir.create(dir_roads, showWarnings = FALSE)
  
  geo_state <- states |>
    filter(STUSPS == state) |>
    sf::st_transform(tiger_crs)
  
  sf::sf_use_s2(FALSE)
  
  # state management
  management_state <- management_national |>
    sf::st_transform(tiger_crs) |>
    sf::st_intersection(geo_state) |>
    sf::st_union()
  
  # tiger
  print("Tiger ops")
  counties_state_list <- counties |>
    filter(STATEFP == geo_state$STATEFP) |>
    sf::st_transform(tiger_crs) |>
    sf::st_filter(management_state) |>
    pull(NAME)
  
  mtfcc_drop <- c("S1710", "S1720", "S1820", "S1830") #walkways, stairways, bike paths, bridle paths, etc - things you can't take a vehicle down
  
  tiger_roads_state <- tigris::roads(state, county = counties_state_list, year = 2024) |>
    filter(! MTFCC %in% mtfcc_drop) |>
    sf::st_filter(management_state) |>
    sf::st_intersection(management_state)
  
  # FS roads
  fs_roads_state <- st_read(fs_road_file,
                            wkt_filter = aoi_bbox_roadless) |>
    st_transform(tiger_crs) |>
    st_filter(geo_state) |>
    filter(ROUTE_STAT == "EX - EXISTING")
  
  
  # Merge roads
  print(glue("All roads pulled for {state}; binding and writing")) 
  
  roads_all_state <- rbind(sf::st_geometry(fs_roads_state),
                           sf::st_geometry(tiger_roads_state)) |>
    sf::st_sf()
  
  flnm <- paste0(state, "_usfs_roads.gpkg")
  sf::st_write(roads_all_state, here(dir_roads, flnm))
  return(flnm)
}

road_files <- c("WY") |>
  #road_files <- states$STUSPS |>
  purrr::set_names() |>
  purrr::map(.f = pull_relevant_state_roads)






# TESTING

state <- "WY"
dir_roads <- here(dir_derived, "roads")
dir.create(dir_roads, showWarnings = FALSE)

geo_state <- states |>
  dplyr::filter(STUSPS == state) |>
  sf::st_transform(tiger_crs)

sf::sf_use_s2(FALSE)

# state management
management_state <- management_national |>
  sf::st_transform(tiger_crs) |>
  sf::st_intersection(geo_state)

# tiger
print("Tiger ops")
counties_state_list <- counties |>
  filter(STATEFP == geo_state$STATEFP) |>
  sf::st_transform(tiger_crs) |>
  sf::st_filter(management_state) |>
  pull(NAME)

mtfcc_drop <- c("S1710", "S1720", "S1820", "S1830") #walkways, stairways, bike paths, bridle paths, etc - things you can't take a vehicle down

tiger_roads_state <- tigris::roads(state, county = counties_state_list, year = 2024) |>
  filter(! MTFCC %in% mtfcc_drop) |>
  sf::st_filter(management_state)

tiger_roads_state_5070 <- tiger_roads_state |>
  sf::st_transform(5070)

# FS roads
fs_roads_state <- st_read(fs_road_file) |>
  st_transform(tiger_crs) |>
  st_filter(geo_state) |>
  filter(ROUTE_STAT == "EX - EXISTING")

fs_roads_state_5070 <- fs_roads_state |>
  sf::st_transform(5070)

# Merge roads
print(glue("All roads pulled for {state}; binding and writing")) 

geo_state_5070 <- geo_state |>
  sf::st_transform(5070)

roads_all_state <- rbind(fs_roads_state_5070 |> select(geometry),
                         tiger_roads_state_5070 |> select(geometry)) |>
  sf::st_intersection(geo_state_5070)

flnm <- paste0(state, "_allusfs_roads.gpkg")
sf::st_write(roads_all_state, here(dir_roads, flnm))
sf::st_write(tiger_roads_state_5070, here(dir_roads, paste0(state, "_fs_only_roads.gpkg")))
sf::st_write(fs_roads_state_5070, here(dir_roads, paste0(state, "_tiger_only_roads.gpkg")))

return(flnm)


