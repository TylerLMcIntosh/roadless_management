library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)


dir_dats <- here("data/raw")
dir_derived <- here("data/derived")
dir_roads <- here(dir_derived, "roads")

twig_points_with_management <- st_read(here(dir_derived, "twig_points_management_added.gpkg")) # from script 02

states <- tigris::states() |>
  st_transform(5070) |>
  filter(! STUSPS %in% c("CO", "ID")) #remove CO and ID

distances_from_roads <- function(state) {
  
  twig_filt_centroids_state <- twig_points_with_management |>
    st_filter(geo_state) 
  
  roads_all_state <- sf::st_read(paste0(state, "_allusfs_roads_5070.gpkg"))
  
  # get nearest feature; keep closest distance

  nearest_idx <- sf::st_nearest_feature(twig_filt_centroids_state, roads_all_state)
  twig_filt_centroids_state$dist_to_road <- sf::st_distance(twig_filt_centroids_state, roads_all_state[nearest_idx, ], by_element = TRUE)

  centroid_distances <- twig_filt_centroids_state |>
    dplyr::group_by("type, state") |>
    dplyr::summarize(mean_road_dist = mean(dist_to_road, na.rm = TRUE),
                     median_road_dist = median(dist_to_road, na.rm = TRUE),
                     min_road_dist = min(dist_to_road, na.rm = TRUE),
                     max_road_dist = max(dist_to_road, na.rm = TRUE),
                     n_trt = n())
  
  return(list("distance_summary" = centroid_distances,
              "centroid_data" = twig_filt_centroids_state))
}


#road_files <- c("WY") |>
road_files <- states$STUSPS |>
  purrr::set_names() |>
  purrr::map(.f = pull_relevant_state_roads)


