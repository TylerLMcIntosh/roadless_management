
library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)
library(glue)
library(future)
library(furrr)

dir_dats <- here("data/raw")
dir_derived <- here("data/derived")
dir_roads <- here("data/derived/roads")
dir_figs <- here("figs")
dir_buffers <- here("data/derived/buffers")

dir.create(dir_buffers)

MEDIAN_TREATMENT_AREA_RATIO <- 1.82 # from 05; ratio of computed geospatial area to reported area

agg_results <- read_csv(here(dir_derived, "state_perc_treated_results.csv"))

agg_results <- agg_results |>
  dplyr::group_by(state) |>
  dplyr::filter(
    all(c("roadless", "usfs non-roadless") %in% management)
  ) |>
  dplyr::ungroup()


top_10_roadless_area_states <- agg_results |>
  filter(management == "roadless") |>
  arrange(desc(area_acres)) |>
  slice_head(n = 10) |>
  pull(state)


# just do top ten
dist_stats <- read_csv(here(dir_derived, "distance_state_summary.csv"))
dist_stats <- dist_stats |>
  filter(state %in% top_10_roadless_area_states) |>
  mutate(mean_dist_doubled = mean_dist * 2,
         median_dist_doubled = median_dist * 2)

states <- tigris::states() |>
  filter(STUSPS %in% top_10_roadless_area_states) |>
  sf::st_transform(5070)

management_national <- sf::st_read(here(dir_derived, "roadless_management_national_simplified_5070.gpkg"))

#twig_filt <- sf::st_read(here(dir_derived, "twig_filt_poly_5070.gpkg")) # read in for each state instead
twig_filt_fl <- here(
  dir_derived,
  "twig_filt_poly_5070.gpkg"
)

get_buffered_areas <- function(state) {
  state_roads_fl <- here(dir_roads, paste0(state, "_allusfs_roads_5070.gpkg"))
  
  if(file.exists(state_roads_fl)) {
    
    dist_to_use <- dist_stats |>
      filter(.data$state == .env$state) |>
      pull(median_dist) # use median distance for first run
    
    stopifnot(
      length(dist_to_use) == 1,
      !is.na(dist_to_use)
    )
    
    roads_all_state <- sf::st_read(state_roads_fl) |>
      sf::st_union()
    
    geo_state <- states |>
      dplyr::filter(.data$STUSPS == .env$state)
    
    state_mgmt <- management_national |>
      sf::st_filter(geo_state) |>
      sf::st_intersection(geo_state)
    
    roads_buffered <- roads_all_state |>
      sf::st_buffer(dist = dist_to_use,
                    nQuadSegs = 4) # use 4 = 16 sides total for circle, reasonable approximation with much faster operation
  
    roadless_buffered <- roads_buffered |>
      sf::st_intersection(state_mgmt |> filter(management == "roadless"))
      
    usfsnonroadless_buffered <- roads_buffered |>
      sf::st_intersection(state_mgmt |> filter(management != "roadless"))
    
    sf::st_write(roadless_buffered,
                 here(dir_buffers, paste0(state, "_roadless_buffers.gpkg")),
                 append = FALSE)
    sf::st_write(usfsnonroadless_buffered,
                 here(dir_buffers, paste0(state, "_usfsnonroadless_buffers.gpkg")),
                 append = FALSE)
    
    # intersect w/ TWIG
    # state_twig <- twig_filt |>
    #   sf::st_filter(geo_state) |>
    #   sf::st_union()
    
    # read in with wkt
    twig_filter_wkt <- geo_state |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_text()
    
    state_twig <- sf::st_read(
      twig_filt_fl,
      wkt_filter = twig_filter_wkt,
      quiet = TRUE
    ) |>
      sf::st_filter(geo_state) |>
      sf::st_union()
    
    #operatef
    roadless_buff_twig <- state_twig |>
      sf::st_intersection(roadless_buffered)
    
    usfsnonroadless_buff_twig <- state_twig |>
      sf::st_intersection(usfsnonroadless_buffered)
    
    sf::st_write(roadless_buff_twig,
                 here(dir_buffers, paste0(state, "_roadless_buff_twig.gpkg")),
                 append = FALSE)
    sf::st_write(usfsnonroadless_buff_twig,
                 here(dir_buffers, paste0(state, "_usfsnonroadless_buff_twig.gpkg")),
                 append = FALSE)
    
    
    # get sums
    roadless_area <- roadless_buffered |>
      sf::st_area() |>
      sum()
    
    usfsnonroadless_area <- usfsnonroadless_buffered |>
      sf::st_area() |>
      sum()
    
    roadless_area_twig <- roadless_buff_twig |>
      sf::st_area() |>
      sum()
    
    usfsnonroadless_area_twig <- usfsnonroadless_buff_twig |>
      sf::st_area() |>
      sum()
    
    return(
      tibble::tibble(
        state = state,
        roadless_buffered_area_acres = units::drop_units(roadless_area) * 0.000247105,
        usfsnonroadless_buffered_area_acres = units::drop_units(usfsnonroadless_area) * 0.000247105,
        roadless_buff_twig_area_acres = units::drop_units(roadless_area_twig) * 0.000247105,
        usfsnonroadless_buff_twig_area_acres = units::drop_units(usfsnonroadless_area_twig) * 0.000247105,
        roadless_buff_twig_area_acres_corrected = roadless_buff_twig_area_acres / MEDIAN_TREATMENT_AREA_RATIO,
        usfsnonroadless_buff_twig_area_acres_corrected = usfsnonroadless_buff_twig_area_acres / MEDIAN_TREATMENT_AREA_RATIO
      )
    )
  }
}



#future::plan(future::multisession, workers = 10)

buffer_results <- top_10_roadless_area_states |>
  purrr::set_names() |>
  #furrr::future_map(
  purrr::map(
    .f = get_buffered_areas,
    .options = furrr::furrr_options(
      packages = c(
        "sf",
        "dplyr",
        "tibble",
        "units",
        "here"
      ),
      seed = TRUE
    )
  )

#future::plan(future::sequential)

buffer_results_bound <- buffer_results |>
  purrr::compact() |>
  dplyr::bind_rows()

write_csv(buffer_results_bound,
          here(dir_derived, "distance_aware_stats_by_state.csv"))



