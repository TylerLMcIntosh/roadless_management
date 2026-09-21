library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)
library(glue)

dir_dats <- here("data/raw")
dir_derived <- here("data/derived")
dir_roads <- here(dir_derived, "roads")

twig_points_with_management <- st_read(here(dir_derived, "twig_points_management_added_5070.gpkg")) # from script 02

states <- tigris::states() |>
  st_transform(5070) |>
  filter(! STUSPS %in% c("CO", "ID")) #remove CO and ID

distances_from_roads <- function(state) {
  
  state_roads_fl <- here(dir_roads, paste0(state, "_allusfs_roads_5070.gpkg"))
  
  if(file.exists(state_roads_fl)) {
    
    roads_all_state <- sf::st_read(state_roads_fl)
    
    geo_state <- states |>
      dplyr::filter(STUSPS == state)
    
    twig_filt_centroids_state <- twig_points_with_management |>
      st_filter(geo_state) 
    
    # get nearest feature; keep closest distance
  
    nearest_idx <- sf::st_nearest_feature(twig_filt_centroids_state, roads_all_state)
    twig_filt_centroids_state$dist_to_road <- sf::st_distance(twig_filt_centroids_state, roads_all_state[nearest_idx, ], by_element = TRUE)
  
    centroid_distances <- twig_filt_centroids_state |>
      dplyr::group_by(type, state) |>
      dplyr::summarize(mean_road_dist = mean(dist_to_road, na.rm = TRUE),
                       median_road_dist = median(dist_to_road, na.rm = TRUE),
                       min_road_dist = min(dist_to_road, na.rm = TRUE),
                       max_road_dist = max(dist_to_road, na.rm = TRUE),
                       n_trt = n())
    
    return(list("distance_summary" = centroid_distances,
                "centroid_data" = twig_filt_centroids_state))
  }
}


#road_files <- c("WY") |>
dist_roads_results <- states$STUSPS |>
  purrr::set_names() |>
  purrr::map(.f = distances_from_roads)


dist_roads_results_bound <- dist_roads_results |>
  purrr::compact() |>
  purrr::list_transpose() |>
  purrr::map(dplyr::bind_rows)

distance_summary <- dist_roads_results_bound$distance_summary |>
  sf::st_drop_geometry()
twig_points_with_management_and_distance <- dist_roads_results_bound$centroid_data

write_csv(distance_summary, here(dir_derived, "distance_summary.csv"))
sf::st_write(twig_points_with_management_and_distance, here(dir_derived, "twig_points_management_distance_added_5070.gpkg"))



