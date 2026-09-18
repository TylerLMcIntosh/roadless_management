
library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)
library(glue)


dir_dats <- here("data/raw")
dir_derived <- here("data/derived")



# Examine road data ----

# # LOOK AT ROAD DATA
# # test case - WY
# 
# state <- "WY"
# 
# geo_state <- states |>
#   filter(STUSPS == state) |>
#   sf::st_transform(twig_crs)
# 
# 
# aoi_bbox_roadless <- geo_state |>
#   st_transform(roadless_crs) |>
#   st_geometry() |>
#   st_as_text()
# 
# sma_state <- usfs |>
#   st_filter(geo_state) |>
#   st_intersection(geo_state) |>
#   st_union()
# 
# counties_state_list <- counties |>
#   filter(STATEFP == geo_state$STATEFP) |>
#   sf::st_filter(sma_state) |>
#   pull(NAME)
# 
# fs_roads_state <- st_read(fs_road_file,
#                           wkt_filter = aoi_bbox_roadless) |>
#   st_transform(twig_crs) |>
#   st_intersection(geo_state) |>
#   filter(ROUTE_STAT == "EX - EXISTING") |>
# 
# tiger_roads_state <- tigris::roads(state, county = counties_state_list, year = 2024) |>
#   st_transform(twig_crs) |>
#   filter(! MTFCC %in% mtfcc_drop) |>
#   sf::st_filter(sma_state) |>
#   sf::st_intersection(sma_state)
# 
# twig_filt_centroids_state <- twig_filt_centroids |>
#   st_filter(geo_state) 
# 
# 
# # get nearest feature for both sets; keep closest distance
# roads_all_state <- dplyr::bind_rows(sf::st_geometry(fs_roads_state), sf::st_geometry(tiger_roads_state)) |>
#   sf::st_sf()
# 
# nearest_idx <- sf::st_nearest_feature(twig_filt_centroids_state, roads_all_state)
# twig_filt_centroids_state$dist_to_road <- sf::st_distance(twig_filt_centroids_state, roads_all_state[nearest_idx, ], by_element = TRUE)
# 
# state_dist_summary <- twig_filt_centroids_state |>
#   dplyr::group_by(type)
# 
# # unique(fs_roads_state$ROUTE_STAT)
# # unique(fs_roads_state$FUNCTIONAL)
# # unique(fs_roads_state$SURFACE_TY)
# # unique(fs_roads_state$LANES)
# # unique(fs_roads_state$LEVEL_OF_S)
# # unique(fs_roads_state$LOC_ERROR)
# # unique(fs_roads_state$OPENFORUSE)
# # unique(fs_roads_state$SYMBOL_NAM)
# 
# #TIGER metadata information is located here: https://www.arcgis.com/sharing/rest/content/items/2f3dd04e86e64388becba0a0e6e4e5e9/info/metadata/metadata.xml?format=default&output=html
# ## this page directs to https://doi.org/10.21949/1529082 for a full list of codes
# 
# # MTFCC values are as follow:
# # S1100	Primary Road
# # S1200	Secondary Road
# # S1400	Local Neighborhood Road, Rural Road, City Street
# # S1500	Vehicular Trail (4WD)
# # S1630	Ramp
# # S1640	Service Drive usually along a lined access highway
# # S1710	Walkway/Pedestrian Trail
# # S1720	Stairway
# # S1730	Alley
# # S1740	Private Road for service vehicles (logging, oil fields, ranches, etc.)
# # S1750	Private Driveway
# # S1780	Parking Lot Road
# # S1820	Bike Path or Trail
# # S1830	Bridle Path
# # S2000	Road Median
# 
# mtfcc_drop <- c("S1710", "S1720", "S1820", "S1830") #walkways, stairways, bike paths, bridle paths, etc - things you can't take a vehicle down
# 
# tig_roads_state <- tigris::roads("WY", county = c("Teton", "Sublette"), year = 2024) |>
#   filter(! MTFCC %in% mtfcc_drop)
# 
# mapview(sma_state) + mapview(tig_roads_state, color = "purple") + mapview(fs_roads_state, color = "orange")
# # It is clear that we need BOTH TIGER and USFS roads




# Operate on road data ----

mngmt_natl_fl <- here(dir_derived, "roadless_management_national_simplified_5070.gpkg")

management_national <- sf::st_read(mngmt_natl_fl)

# States
states <- tigris::states() |>
  filter(! STUSPS %in% c("CO", "ID")) #remove CO and ID

# counties - for pulling TIGRIS road data
counties <- tigris::counties()

fs_road_file <- here(dir_dats, "S_USA.RoadCore_FS/S_USA.RoadCore_FS.shp")
fs_road_crs <- sf::st_read(
  fs_road_file,
  query = "SELECT * FROM \"S_USA.RoadCore_FS\" LIMIT 0",
  quiet = TRUE
) |>
  sf::st_crs()


st_crs(fs_road_file)

tiger_crs <- tigris::roads(state = "WY", county = "Teton", year = 2024) |>
  sf::st_crs()


management_national_tiger <- management_national |>
  sf::st_transform(tiger_crs)



pull_relevant_state_roads <- function(state) {
  
  # We need to operate in the crs of the roads, since transforming them
  # takes a long time (a very large number of vertices). Using tiger crs
  
  dir_roads <- here(dir_derived, "roads")
  dir.create(dir_roads, showWarnings = FALSE)
  
  geo_state <- states |>
    dplyr::filter(STUSPS == state) |>
    sf::st_transform(tiger_crs)
  
  sf::sf_use_s2(FALSE)
  
  # state management
  management_state <- management_national_tiger |>
    sf::st_filter(geo_state) |>
    sf::st_intersection(geo_state)
  
  if(nrow(management_state) > 0 ) {
      
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
    
    sf::sf_use_s2(TRUE)
    
    tiger_roads_state_5070 <- tiger_roads_state |>
      sf::st_transform(5070)
    
    # FS roads
    
    aoi_bbox <- geo_state |>
      sf::st_transform(fs_rodd_crs) |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_text()
    
    fs_roads_state <- sf::st_read(
      fs_road_file,
      wkt_filter = aoi_bbox,
      quiet = TRUE
    ) |>
      sf::st_transform(tiger_crs) |>
      sf::st_filter(geo_state) |>
      dplyr::filter(ROUTE_STAT == "EX - EXISTING")
    
    # fs_roads_state <- st_read(fs_road_file) |>
    #   st_transform(tiger_crs) |>
    #   st_filter(geo_state) |>
    #   filter(ROUTE_STAT == "EX - EXISTING")
    
    fs_roads_state_5070 <- fs_roads_state |>
      sf::st_transform(5070)
    
    # Merge roads
    print(glue("All roads pulled for {state}; binding and writing")) 
    
    geo_state_5070 <- geo_state |>
      sf::st_transform(5070)
    
    roads_all_state <- rbind(fs_roads_state_5070 |> select(geometry),
                             tiger_roads_state_5070 |> select(geometry)) |>
      sf::st_intersection(geo_state_5070)
    
    flnm <- paste0(state, "_allusfs_roads_5070.gpkg")
    sf::st_write(roads_all_state, here(dir_roads, flnm))
    sf::st_write(tiger_roads_state_5070, here(dir_roads, paste0(state, "_tiger_only_roads_5070.gpkg")))
    sf::st_write(fs_roads_state_5070, here(dir_roads, paste0(state, "_fs_only_roads_5070.gpkg")))
    
    return(flnm)
  } else {
    return("")
  }
}

road_files <- c("WY") |>
#road_files <- states$STUSPS |>
  purrr::set_names() |>
  purrr::map(.f = pull_relevant_state_roads)






# TESTING
# 
# state <- "WY"
# dir_roads <- here(dir_derived, "roads")
# dir.create(dir_roads, showWarnings = FALSE)
# 
# geo_state <- states |>
#   dplyr::filter(STUSPS == state) |>
#   sf::st_transform(tiger_crs)
# 
# sf::sf_use_s2(FALSE)
# 
# # state management
# management_state <- management_national |>
#   sf::st_transform(tiger_crs) |>
#   sf::st_intersection(geo_state)
# 
# # tiger
# print("Tiger ops")
# counties_state_list <- counties |>
#   filter(STATEFP == geo_state$STATEFP) |>
#   sf::st_transform(tiger_crs) |>
#   sf::st_filter(management_state) |>
#   pull(NAME)
# 
# mtfcc_drop <- c("S1710", "S1720", "S1820", "S1830") #walkways, stairways, bike paths, bridle paths, etc - things you can't take a vehicle down
# 
# tiger_roads_state <- tigris::roads(state, county = counties_state_list, year = 2024) |>
#   filter(! MTFCC %in% mtfcc_drop) |>
#   sf::st_filter(management_state)
# 
# tiger_roads_state_5070 <- tiger_roads_state |>
#   sf::st_transform(5070)
# 
# # FS roads
# fs_roads_state <- st_read(fs_road_file) |>
#   st_transform(tiger_crs) |>
#   st_filter(geo_state) |>
#   filter(ROUTE_STAT == "EX - EXISTING")
# 
# fs_roads_state_5070 <- fs_roads_state |>
#   sf::st_transform(5070)
# 
# # Merge roads
# print(glue("All roads pulled for {state}; binding and writing")) 
# 
# geo_state_5070 <- geo_state |>
#   sf::st_transform(5070)
# 
# roads_all_state <- rbind(fs_roads_state_5070 |> select(geometry),
#                          tiger_roads_state_5070 |> select(geometry)) |>
#   sf::st_intersection(geo_state_5070)
# 
# flnm <- paste0(state, "_allusfs_roads.gpkg")
# sf::st_write(roads_all_state, here(dir_roads, flnm))
# sf::st_write(tiger_roads_state_5070, here(dir_roads, paste0(state, "_fs_only_roads.gpkg")))
# sf::st_write(fs_roads_state_5070, here(dir_roads, paste0(state, "_tiger_only_roads.gpkg")))
# 
# return(flnm)
# 

