

library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)


dir_dats <- here("data/raw")
dir_derived <- here("data/derived")

st_layers(here(dir_dats, "S_USA.RoadCore_FS/S_USA.RoadCore_FS.shp"))

# USFS roads and roadless
roadless_path <- here(dir_dats, "S_USA.RoadlessArea_2001/S_USA.RoadlessArea_2001.shp")

roadless_crs <- st_read(
  roadless_path) |>
  st_crs()

fs_road_file <- here(dir_dats, "S_USA.RoadCore_FS/S_USA.RoadCore_FS.shp")
#confirmed - fs road file is same CRS as roadless data


# USFS all
sma_fl <- here(dir_dats, "SMA_WM.gdb")
st_layers(sma_fl)
sma_crs <- st_read(
  here(sma_fl),
  query = "SELECT * FROM SurfaceMgtAgy_USFS LIMIT 0"
) |>
  st_crs()




# Twig
twig_path <- here(dir_dats, "treatment_index.gdb")
twig_crs <- st_read(
  twig_path,
  query = "SELECT * FROM treatment_index LIMIT 0"
) |>
  st_crs()

treatment_type_list <- c("Hand Pile Burn",
                         "Machine Pile Burn",
                         "Broadcast Burn",
                         #"Fire Use", FIRE USE REMOVED
                         "Jackpot Burn",
                         "Thinning",
                         "Crushing",
                         "Mowing",
                         "Mastication",
                         "Biomass Removal",
                         "Machine Pile",
                         "Chipping",
                         "Hand Pile",
                         "Preparation",
                         "Lop and Scatter",
                         "Mastication/Mowing")

twig <- sf::st_read(twig_path,
                    layer = "treatment_index")

# filter twig
dup_cols <- c('name', 'treatment_date', 'twig_category', 'acres')
twig_filt_ids <- twig |>
  sf::st_drop_geometry() |>
  dplyr::mutate(treatment_year = lubridate::year(actual_completion_date)) |>
  dplyr::filter(treatment_year %in% seq(2001, 2026)) %>%
  mutate(across(everything(), ~ ifelse(. %in% c('', ' ', 'N/A', 'NA', 'Not Applicable'), NA, .))) %>%
  filter(date_source %in% c('date_completed', 'act_comp_dt')) %>%   # needs to be either 'date_completed' or 'act_comp_dt' (different source codes for completion)
  distinct(unique_id, .keep_all = T) %>%
  filter(error %in% c(NA, 'DUPLICATE-KEEP')) %>%  #drop duplicates and high cost errors
  distinct(across(all_of(dup_cols)), .keep_all = T) %>%
  mutate(treatment_date = as.POSIXct(actual_completion_date, tz = 'UTC')) %>% # converted back to epoch time for some reason
  mutate(across(c(date_current, actual_completion_date), function(x) as.POSIXct(x, tz = 'UTC'))) %>%
  filter(treatment_date < as.Date("2026-01-01")) |>
  pull(unique_id)

twig_filt <- twig |>
  filter(unique_id %in% twig_filt_ids & type %in% treatment_type_list) |>
  dplyr::mutate(treatment_year = lubridate::year(actual_completion_date))

# centroids for distance calculations
twig_filt_centroids <- twig_filt |>
  sf::st_centroid(of_largest_polygon = TRUE)


# States
states <- tigris::states() |>
  st_transform(twig_crs) |>
  filter(! STUSPS %in% c("CO", "ID")) #remove CO and ID

# counties - for pulling TIGRIS road data
counties <- tigris::counties() |>
  st_transform(twig_crs)


usfs <- st_read(
  here(sma_fl),
  layer = "SurfaceMgtAgy_USFS") |>
  st_transform(twig_crs)


# prep management national data
usfs_dissolved <- usfs |> sf::st_union()

roadless_all <- sf::st_read(roadless_path) |>
  sf::st_transform(twig_crs) |>
  sf::st_union()

usfs_nonroadless_all <- sf::st_difference(usfs_dissolved, roadless_all)

management_national <- rbind(
  sf::st_sf(management = "roadless", geometry = roadless_all),
  sf::st_sf(management = "usfs non-roadless", geometry = usfs_nonroadless_all)
)


sf::st_write(management_national, here(dir_derived, "roadless_management_national_simplified.gpkg"))


# X acres treated in each state, making up X% of the fuel treatment accomplished within each state

roadless_analysis_1 <- function(state) {
  
  geo_state <- states |>
    filter(STUSPS == state) |>
    sf::st_transform(twig_crs)
  
  # aoi_bbox_roadless <- geo_state |>
  #   st_transform(roadless_crs) |>
  #   st_geometry() |>
  #   st_as_text()
  
  # twig_state_filt <- twig_filt |>
  #   st_filter(geo_state) |>
  #   st_intersection(geo_state)
  
  # sma_state <- usfs |>
  #   st_filter(geo_state) |>
  #   st_intersection(geo_state) |>
  #   st_union()
  # 
  # roadless_state <- st_read(roadless_path,
  #                           wkt_filter = aoi_bbox_roadless) |>
  #   st_transform(twig_crs) |>
  #   st_intersection(geo_state)
  # 
  
  management_state <- management_national |>
    sf::st_intersection(geo_state)
  
  # area of each management class within the state
  management_area_state <- management_state |>
    st_transform(5070) |>
    mutate(area_acres = units::set_units(st_area(geometry), "acres")) |>
    st_drop_geometry() |>
    group_by(management) |>
    summarise(
      area_acres = sum(area_acres),
      .groups = "drop"
    ) |>
    mutate(state = state,
           area_acres = units::drop_units(area_acres))
  
  
  # spatial join
  twig_state_centroids_filt_joined <- twig_filt_centroids |>
    st_filter(geo_state) |>
    sf::st_join(management_state) |>
    filter(!is.na(management))
  
  
  # summarize
  state_summary <- twig_state_centroids_filt_joined |>
    dplyr::group_by(type, treatment_year, state, management) |>
    dplyr::summarise(reported_acres = sum(acres, na.rm = TRUE),
                     reported_cost = sum(total_cost, na.rm = TRUE),
                     n_trt = n(),
                     n_trt_w_acres = sum(!is.na(acres)),
                     n_trt_w_cost = sum(!is.na(total_cost)),
                     .groups = "drop")
  
  # return results
  return(list("state_summary" = state_summary,
              "point_data" = twig_state_centroids_filt_joined,
              "management_area" = management_area_state))
  
  # # only perform for states with roadless areas
  # if(nrow(roadless_state) > 0) {
  #   
  #   #management data
  #   roadless_state <- roadless_state |>
  #     st_union()
  #   
  #   usfs_nonroadless_geom <- st_difference(sma_state, roadless_state)
  #   
  #   management_state <- rbind(
  #     st_sf(operating_state = state, management = "roadless", geometry = roadless_state),
  #     st_sf(operating_state = state, management = "usfs non-roadless", geometry = usfs_nonroadless_geom)
  #   )
  #   
  #   # spatial join
  #   twig_state_joined <- twig_state_filt |>
  #     sf::st_join(management_state, largest = TRUE) |>
  #     filter(!is.na(management))
  #   
  #   # summarize
  #   state_summary <- twig_state_joined |>
  #     dplyr::group_by(type, treatment_year, operating_state, management) |>
  #     dplyr::summarise(reported_acres = sum(acres, na.rm = TRUE),
  #                      reported_cost = sum(total_cost, na.rm = TRUE),
  #                      n_trt = n(),
  #                      n_trt_w_acres = sum(!is.na(acres)),
  #                      n_trt_w_cost = sum(!is.na(total_cost)),
  #                      .groups = "drop")
  #   
  #   # return results
  #   return(list("state_summary" = state_summary,
  #               "poly_data" = twig_state_joined))
  # }
}

all_results <- states$STUSPS |>
  purrr::set_names() |>
  purrr::map(.f = roadless_analysis_1)

all_results_bound <- all_results |>
  purrr::list_transpose() |>
  purrr::map(dplyr::bind_rows)

view(all_results_bound$state_summary)

all_results_summed <- all_results_bound$state_summary |>
  dplyr::group_by(state, management) |>
  dplyr::summarise(reported_acres = sum(reported_acres, na.rm = TRUE),
                   reported_cost = sum(reported_cost, na.rm = TRUE),
                   n_trt = sum(n_trt),
                   n_trt_w_acres = sum(n_trt_w_acres),
                   n_trt_w_cost = sum(n_trt_w_cost)) |>
  dplyr::left_join(all_results_bound$management_area)

all_results_summed <- all_results_summed |>
  dplyr::mutate((reported_acres / area_acres) * 100)



roadless_analysis_2 <- function() {
  
  # ADD STUFF AT THE BEGINNING TO MAKE WORK
  
  # deal with roads
  fs_roads_state <- st_read(fs_road_file,
                            wkt_filter = aoi_bbox_roadless) |>
    st_transform(twig_crs) |>
    st_intersection(geo_state) |>
    filter(ROUTE_STAT == "EX - EXISTING")
  
  counties_state_list <- counties |>
    filter(STATEFP == geo_state$STATEFP) |>
    sf::st_filter(sma_state) |>
    pull(NAME)
  
  mtfcc_drop <- c("S1710", "S1720", "S1820", "S1830") #walkways, stairways, bike paths, bridle paths, etc - things you can't take a vehicle down
  
  tiger_roads_state <- tigris::roads(state, county = counties_state_list, year = 2024) |>
    st_transform(twig_crs) |>
    filter(! MTFCC %in% mtfcc_drop) |>
    sf::st_filter(sma_state) |>
    sf::st_intersection(sma_state)
  
  twig_filt_centroids_state <- twig_filt_centroids |>
    st_filter(geo_state) 
  
  
  # get nearest feature for both sets; keep closest distance
  roads_all_state <- rbind(sf::st_geometry(fs_roads_state),
                           sf::st_geometry(tiger_roads_state)) |>
    sf::st_sf()
  
  nearest_idx <- sf::st_nearest_feature(twig_filt_centroids_state, roads_all_state)
  twig_filt_centroids_state$dist_to_road <- sf::st_distance(twig_filt_centroids_state, roads_all_state[nearest_idx, ], by_element = TRUE)
  twig_filt_centroids_state$operating_state <- state
  
  
  centroid_distances <- twig_filt_centroids_state |>
    dplyr::group_by("type, operating_state") |>
    dplyr::summarize(mean_road_dist = mean(dist_to_road, na.rm = TRUE),
                     median_road_dist = median(dist_to_road, na.rm = TRUE),
                     min_road_dist = min(dist_to_road, na.rm = TRUE),
                     max_road_dist = max(dist_to_road, na.rm = TRUE),
                     n_trt = n())
  
  return(list("distance_summary" = centroid_distances,
              "centroid_data" = twig_filt_centroids_state))
}





# 
# all_results_summed <- all_results |>
#   dplyr::group_by(operating_state, management) |>
#   dplyr::summarise(reported_acres = sum(reported_acres, na.rm = TRUE),
#                    reported_cost = sum(reported_cost, na.rm = TRUE))



# levels of fuel treatment have occurred in Roadless areas (XX%) when compared to non-roadless 
# USFS lands (XX%); this is the case across all states


#we estimate that XX% of roadless areas could be accessed from existing roads 
#(XX acres, which would cost $XX to perform). Of that area, only XX% has actually seen fuel 
#treatment activities, at an estimated cost of $XX. 
#(In comparison to XX% of road-accessible areas being treated on normal USFS lands)





# 
# 
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



# buffer SMA by one km
# filter and clip both road datasets to only overlapping segments

# distance from roads to centroids first; then 



##
# 
# state <- "MD"
# 
# 
# geo_state <- states |>
#   filter(STUSPS == state) |>
#   sf::st_transform(twig_crs)
# 
# # load each dataset clipped to just the state extent, then clip each to the actual state polygon
# 
# aoi_bbox_roadless <- geo_state |>
#   st_transform(roadless_crs) |>
#   st_geometry() |>
#   st_as_text()
# 
# 
# twig_state_filt <- twig_filt |>
#   st_filter(geo_state) |>
#   st_intersection(geo_state)
# 
# sma_state <- usfs |>
#   st_filter(geo_state) |>
#   st_intersection(geo_state) |>
#   st_union() #|>



state <- "WY"


geo_state <- states |>
  filter(STUSPS == state) |>
  sf::st_transform(twig_crs)

aoi_bbox_roadless <- geo_state |>
  st_transform(roadless_crs) |>
  st_geometry() |>
  st_as_text()

twig_state_filt <- twig_filt |>
  st_filter(geo_state) |>
  st_intersection(geo_state)

sma_state <- usfs |>
  st_filter(geo_state) |>
  st_intersection(geo_state) |>
  st_union()

roadless_state <- st_read(roadless_path,
                          wkt_filter = aoi_bbox_roadless) |>
  st_transform(twig_crs) |>
  st_intersection(geo_state)


# only perform for states with roadless areas
#if(nrow(roadless_state) > 0) {
  
  #management data
  roadless_state <- roadless_state |>
    st_union()
  
  usfs_nonroadless_geom <- st_difference(sma_state, roadless_state)
  
  management_state <- rbind(
    st_sf(operating_state = state, management = "roadless", geometry = roadless_state),
    st_sf(operating_state = state, management = "usfs non-roadless", geometry = usfs_nonroadless_geom)
  )
  
  # spatial join
  twig_state_joined <- twig_state_filt |>
    sf::st_join(management_state, largest = TRUE) |>
    filter(!is.na(management))
  
  # summarize
  state_summary <- twig_state_joined |>
    dplyr::group_by(type, treatment_year, operating_state, management) |>
    dplyr::summarise(reported_acres = sum(acres, na.rm = TRUE),
                     reported_cost = sum(total_cost, na.rm = TRUE),
                     n_trt = n(),
                     n_trt_w_acres = sum(!is.na(acres)),
                     n_trt_w_cost = sum(!is.na(total_cost)),
                     .groups = "drop")

  
    