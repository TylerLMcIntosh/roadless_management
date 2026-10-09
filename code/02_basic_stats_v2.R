# version using Kyle's data


library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)
library(furrr)
library(future)

dir_dats <- here("data/raw")
dir_derived <- here("data/derived")

twig_filt_fl <- here(dir_derived, "twig_filt_poly_5070.gpkg")
twig_filt_centr_fl <- here(dir_derived, "twig_filt_centroids_5070.gpkg")
land_status_fl <- here(dir_derived, "roadless_land_status_5070.gpkg")


# Roadless analysis #1 ----

land_status <- sf::st_read(land_status_fl)
twig_filt_centroids <- sf::st_read(twig_filt_centr_fl)

roadless_analysis_1 <- function(state) {
  
  state_land_status <- land_status |>
    filter(STATE == state)
    
  management_area_state <- state_land_status |>
    st_drop_geometry() |>
    group_by(STATE, exposure_group) |>
    summarise(
      MANAGE_AREA_ACRES = sum(ACRES, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # spatial join
  twig_state_centroids_filt_joined <- twig_filt_centroids |>
    st_filter(state_land_status) |>
    sf::st_join(
      state_land_status |>
        dplyr::select(STATE, STRATUM, exposure_group)
    )
  
  # summarize
  state_summary <- twig_state_centroids_filt_joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(type, treatment_year, STATE, exposure_group) |>
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
  # } else {
  #   return(list(
  #     state_summary = tibble::tibble(),
  #     point_data = twig_filt_centroids[0, ],
  #     management_area = tibble::tibble()
  #   ))
  # }
}

# Run analysis
# all_results <- states$STUSPS |>
#   purrr::set_names() |>
#   purrr::map(.f = roadless_analysis_1)

future::plan(future::multisession, workers = 8)

all_results <- unique(land_status$STATE) |>
  purrr::set_names() |>
  furrr::future_map(
    .f = roadless_analysis_1,
    .options = furrr::furrr_options(
      packages = c("sf", "dplyr", "units", "tibble")
    )
  )

future::plan(future::sequential)

#manage
all_results_bound <- all_results |>
  purrr::list_transpose() |>
  purrr::map(dplyr::bind_rows)

all_results_disag <- all_results_bound$state_summary# |>
  #filter(state != "ID" & state != "CO")

all_results_summed <- all_results_disag |>
  dplyr::group_by(STATE, exposure_group) |>
  dplyr::summarise(reported_acres = sum(reported_acres, na.rm = TRUE),
                   reported_cost = sum(reported_cost, na.rm = TRUE),
                   n_trt = sum(n_trt),
                   n_trt_w_acres = sum(n_trt_w_acres),
                   n_trt_w_cost = sum(n_trt_w_cost),
                   .groups = "drop") |>
  dplyr::left_join(all_results_bound$management_area,
                   by = c("STATE", "exposure_group"))

all_results_summed <- all_results_summed |>
  dplyr::mutate(perc_treated = (reported_acres / MANAGE_AREA_ACRES) * 100) |>
  sf::st_drop_geometry()

write_csv(all_results_disag, here(dir_derived, "state_perc_treated_results_disag.csv"))
write_csv(all_results_summed, here(dir_derived, "state_perc_treated_results.csv"))

twig_points_with_management <- all_results_bound$point_data |>
  dplyr::rename(twig_state = state)
sf::st_write(twig_points_with_management, here(dir_derived, "twig_points_management_added_5070_v2.gpkg"), append = FALSE)



ggplot(all_results_summed) +
  geom_col(aes(x = STATE, y = perc_treated, fill = exposure_group), position = "dodge")

