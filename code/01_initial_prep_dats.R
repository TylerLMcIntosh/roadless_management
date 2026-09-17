library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)


dir_dats <- here("data/raw")
dir_derived <- here("data/derived")

# Twig ----
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
  dplyr::filter(unique_id %in% twig_filt_ids & type %in% treatment_type_list) |>
  dplyr::mutate(treatment_year = lubridate::year(actual_completion_date)) |>
  sf::st_transform(5070)

# centroids for distance calculations
twig_filt_centroids <- twig_filt |>
  sf::st_centroid(of_largest_polygon = TRUE)

sf::st_write(twig_filt, here(dir_derived, "twig_filt_poly_5070.gpkg"))
sf::st_write(twig_filt_centroids, here(dir_derived, "twig_filt_centroids_5070.gpkg"))

# Management ----

st_layers(here(dir_dats, "S_USA.RoadCore_FS/S_USA.RoadCore_FS.shp"))

# USFS roads and roadless
roadless_path <- here(dir_dats, "S_USA.RoadlessArea_2001/S_USA.RoadlessArea_2001.shp")

roadless_crs <- st_read(
  roadless_path) |>
  st_crs()


# USFS all
sma_fl <- here(dir_dats, "SMA_WM.gdb")
st_layers(sma_fl)
sma_crs <- st_read(
  here(sma_fl),
  query = "SELECT * FROM SurfaceMgtAgy_USFS LIMIT 0"
) |>
  st_crs()

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
) |>
  sf::st_transform(5070)

sf::st_write(management_national, here(dir_derived, "roadless_management_national_simplified_5070.gpkg"))



