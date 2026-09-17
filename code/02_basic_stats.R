


library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)


dir_dats <- here("data/raw")
dir_derived <- here("data/derived")

twig_filt_fl <- here(dir_derived, "twig_filt_poly_5070.gpkg")
twig_filt_centr_fl <- here(dir_derived, "twig_filt_centroids_5070.gpkg")
mngmt_natl_fl <- here(dir_derived, "roadless_management_national_simplified_5070.gpkg")


# States
states <- tigris::states() |>
  st_transform(5070) |>
  filter(! STUSPS %in% c("CO", "ID")) #remove CO and ID


# Roadless analysis #1 ----

# X acres treated in each state, making up X% of the fuel treatment accomplished within each state

management_national <- sf::st_read(mngmt_natl_fl)
twig_filt_centroids <- sf::st_read(twig_filt_centr_fl)

roadless_analysis_1 <- function(state) {
  
  geo_state <- states |>
    filter(STUSPS == state)
  
  management_state <- management_national |>
    sf::st_intersection(geo_state)
  
  # area of each management class within the state
  management_area_state <- management_state |>
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
}



all_results <- states$STUSPS |>
  purrr::set_names() |>
  purrr::map(.f = roadless_analysis_1)

all_results_bound <- all_results |>
  purrr::list_transpose() |>
  purrr::map(dplyr::bind_rows)

view(all_results_bound$state_summary)

all_results_disag <- all_results_bound$state_summary |>
  filter(state != "ID" & state != "CO")

all_results_summed <- all_results_disag |>
  dplyr::group_by(state, management) |>
  dplyr::summarise(reported_acres = sum(reported_acres, na.rm = TRUE),
                   reported_cost = sum(reported_cost, na.rm = TRUE),
                   n_trt = sum(n_trt),
                   n_trt_w_acres = sum(n_trt_w_acres),
                   n_trt_w_cost = sum(n_trt_w_cost)) |>
  dplyr::left_join(all_results_bound$management_area)

all_results_summed <- all_results_summed |>
  dplyr::mutate(perc_treated = (reported_acres / area_acres) * 100) |>
  sf::st_drop_geometry()

write_csv(all_results_disag, here(dir_derived, "state_perc_treated_results_disag.csv"))
write_csv(all_results_summed, here(dir_derived, "state_perc_treated_results.csv"))

twig_points_with_management <- all_results_bound$point_data |>
  dplyr::rename(ST_NAME = NAME)
sf::st_write(twig_points_with_management, here(dir_derived, "twig_points_management_added.gpkg"))



ggplot(all_results_summed) +
  geom_col(aes(x = state, y = perc_treated, fill = management), position = "dodge")

