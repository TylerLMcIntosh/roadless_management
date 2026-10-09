
library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)
library(glue)
library(future)
library(furrr)
library(terra)
library(exactextractr)
library(tictoc)

dir_dats <- here("data/raw")
dir_derived <- here("data/derived")
dir_roads <- here("data/derived/roads")
dir_figs <- here("figs")
dir_buffers <- here("data/derived/buffers")

forest_raster_fl <- here(
  dir_derived,
  "NLCD_2024_forest_5070.tif"
)

if(!file.exists(forest_raster_fl)) {
  # nlcd <- terra::rast(here(dir_dats, "Annual_NLCD_LndCov_2024_CU_C1V2/Annual_NLCD_LndCov_2024_CU_C1V2.tif"))
  # nlcd_5070 <- nlcd |>
  #   terra::project('epsg:5070', method = "near")
  # terra::writeRaster(nlcd_5070, here(dir_derived, "NLCD_2024_5070.tif"), overwrite = TRUE)
  # 
  # nlcd_forestmask_5070 <- nlcd_5070 %in% c(41, 42, 43)
  # terra::writeRaster(nlcd_forestmask_5070,
  #                    forest_raster_fl,
  #                    datatype = "INT1U",
  #                    overwrite = TRUE)
  
  nlcd <- terra::rast(
    here(
      dir_dats,
      "Annual_NLCD_LndCov_2024_CU_C1V2",
      "Annual_NLCD_LndCov_2024_CU_C1V2.tif"
    )
  )
  
  nlcd_forestmask <- nlcd %in% c(41, 42, 43)
  
  nlcd_forestmask_5070 <- terra::project(
    nlcd_forestmask,
    "epsg:5070",
    method = "near"
  )
  
  terra::writeRaster(
    nlcd_forestmask_5070,
    forest_raster_fl,
    datatype = "INT1U",
    overwrite = TRUE
  )
}



dir.create(dir_buffers)

MEDIAN_TREATMENT_AREA_RATIO <- 1.82 # from 05; ratio of computed geospatial area to reported area

agg_results <- read_csv(here(dir_derived, "state_perc_treated_results.csv"))

# agg_results <- agg_results |>
#   dplyr::group_by(state) |>
#   dplyr::filter(
#     all(c("roadless", "usfs non-roadless") %in% management)
#   ) |>
#   dplyr::ungroup()


top_10_roadless_area_states <- agg_results |>
  filter(exposure_group == "Exposed roadless") |>
  arrange(desc(MANAGE_AREA_ACRES)) |>
  slice_head(n = 10) |>
  pull(STATE)


# just do top ten
dist_stats <- read_csv(here(dir_derived, "distance_state_summary.csv"))
dist_stats <- dist_stats |>
  filter(STATE %in% top_10_roadless_area_states) |>
  mutate(mean_dist_doubled = mean_dist * 2,
         median_dist_doubled = median_dist * 2)

# states <- tigris::states() |>
#   filter(STUSPS %in% top_10_roadless_area_states) |>
#   sf::st_transform(5070)

land_status <- sf::st_read(here(dir_derived, "roadless_land_status_5070.gpkg")) |>
  sf::st_make_valid()


#twig_filt <- sf::st_read(here(dir_derived, "twig_filt_poly_5070.gpkg")) # read in for each state instead
twig_filt_fl <- here(
  dir_derived,
  "twig_filt_poly_5070.gpkg"
)


forest_area_acres <- function(x, forest_raster) {
  
  if (length(x) == 0 || all(sf::st_is_empty(x))) {
    return(0)
  }
  
  # exact_extract(sum) accounts for partial-cell coverage
  forest_cells <- exactextractr::exact_extract(
    forest_raster,
    x,
    fun = "sum"
  )
  
  # EPSG:5070 units are meters, so cell area is m^2
  cell_area_m2 <- prod(terra::res(forest_raster))
  
  sum(forest_cells, na.rm = TRUE) *
    cell_area_m2 *
    0.000247105
}


get_buffered_areas <- function(state) {
  state_roads_fl <- here(dir_roads, paste0(state, "_allusfs_roads_5070.gpkg"))
  
  if(file.exists(state_roads_fl)) {
    
    dist_to_use <- dist_stats |>
      filter(.data$STATE == .env$state) |>
      pull(median_dist_doubled) # SELECT DISTANCE TO USE
    
    stopifnot(
      length(dist_to_use) == 1,
      !is.na(dist_to_use)
    )
    
    result_fl <- here(
      dir_buffers,
      paste0(state, "_buffer_results.rds")
    )
    
    # Skip completed states
    if (file.exists(result_fl)) {
      return(readRDS(result_fl))
    }
    
    roads_all_state <- sf::st_read(state_roads_fl)# |>
    #sf::st_union()
    
    # geo_state <- states |>
    #   dplyr::filter(.data$STUSPS == .env$state)
    
    state_mgmt <- land_status |>
      dplyr::filter(STATE == state)
    
    roads_buffered <- roads_all_state |>
      sf::st_buffer(dist = dist_to_use,
                    nQuadSegs = 4) # use 4 = 16 sides total for circle, reasonable approximation with much faster operation
    
    exposed_roadless_buffered <- roads_buffered |>
      sf::st_intersection(state_mgmt |> filter(exposure_group == "Exposed roadless")) |>
      sf::st_union()
    
    unexposed_roadless_buffered <- roads_buffered |>
      sf::st_intersection(state_mgmt |> filter(exposure_group == "Unexposed roadless")) |>
      sf::st_union()
    
    usfsnonroadless_buffered <- roads_buffered |>
      sf::st_intersection(state_mgmt |> filter(exposure_group == "Non-roadless")) |>
      sf::st_union()
    
    sf::st_write(exposed_roadless_buffered,
                 here(dir_buffers, paste0(state, "_exposed_roadless_buffers.gpkg")),
                 append = FALSE)
    sf::st_write(unexposed_roadless_buffered,
                 here(dir_buffers, paste0(state, "_unexposed_roadless_buffers.gpkg")),
                 append = FALSE)
    sf::st_write(usfsnonroadless_buffered,
                 here(dir_buffers, paste0(state, "_usfsnonroadless_buffers.gpkg")),
                 append = FALSE)
    
    
    # intersect w/ TWIG
    # state_twig <- twig_filt |>
    #   sf::st_filter(geo_state) |>
    #   sf::st_union()
    
    # read in with wkt
    twig_filter_wkt <- state_mgmt |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_text()
    
    state_twig <- sf::st_read(
      twig_filt_fl,
      wkt_filter = twig_filter_wkt,
      quiet = TRUE
    ) |>
      sf::st_filter(state_mgmt) |>
      sf::st_union()
    
    #operate
    exposed_roadless_buff_twig <- state_twig |>
      sf::st_intersection(exposed_roadless_buffered)
    
    unexposed_roadless_buff_twig <- state_twig |>
      sf::st_intersection(unexposed_roadless_buffered)
    
    usfsnonroadless_buff_twig <- state_twig |>
      sf::st_intersection(usfsnonroadless_buffered)
    
    sf::st_write(exposed_roadless_buff_twig,
                 here(dir_buffers, paste0(state, "_exposed_roadless_buff_twig.gpkg")),
                 append = FALSE)
    sf::st_write(unexposed_roadless_buff_twig,
                 here(dir_buffers, paste0(state, "_unexposed_roadless_buff_twig.gpkg")),
                 append = FALSE)
    sf::st_write(usfsnonroadless_buff_twig,
                 here(dir_buffers, paste0(state, "_usfsnonroadless_buff_twig.gpkg")),
                 append = FALSE)
    
    
    # get sums
    unexposed_roadless_area <- unexposed_roadless_buffered |>
      sf::st_area() |>
      sum()
    
    exposed_roadless_area <- exposed_roadless_buffered |>
      sf::st_area() |>
      sum()
    
    usfsnonroadless_area <- usfsnonroadless_buffered |>
      sf::st_area() |>
      sum()
    
    unexposed_roadless_area_twig <- unexposed_roadless_buff_twig |>
      sf::st_area() |>
      sum()
    
    exposed_roadless_area_twig <- exposed_roadless_buff_twig |>
      sf::st_area() |>
      sum()
    
    usfsnonroadless_area_twig <- usfsnonroadless_buff_twig |>
      sf::st_area() |>
      sum()
    
    
    # deal with forest overlap
    # crop forest raster once for this state
    if(state != "AK") {
      forest_state <- terra::rast(forest_raster_fl) |>
        terra::crop(terra::vect(state_mgmt |> sf::st_union()))
      
      # forest area within buffered road areas
      unexposed_roadless_forest_area <- forest_area_acres(
        unexposed_roadless_buffered,
        forest_state
      )
      
      exposed_roadless_forest_area <- forest_area_acres(
        exposed_roadless_buffered,
        forest_state
      )
      
      usfsnonroadless_forest_area <- forest_area_acres(
        usfsnonroadless_buffered,
        forest_state
      )
      
      # forest area within TWIG treatments intersecting buffered road areas
      unexposed_roadless_twig_forest_area <- forest_area_acres(
        unexposed_roadless_buff_twig,
        forest_state
      )
      
      exposed_roadless_twig_forest_area <- forest_area_acres(
        exposed_roadless_buff_twig,
        forest_state
      )
      
      usfsnonroadless_twig_forest_area <- forest_area_acres(
        usfsnonroadless_buff_twig,
        forest_state
      )
    } else {
      unexposed_roadless_forest_area <- NA
      exposed_roadless_forest_area <- NA
      usfsnonroadless_forest_area <- NA
      unexposed_roadless_twig_forest_area <- NA
      exposed_roadless_twig_forest_area <- NA
      usfsnonroadless_twig_forest_area <- NA
    }
    
    
    # return dats
    # return(
    #   tibble::tibble(
    #     state = state,
    #     roadless_buffered_area_acres = units::drop_units(roadless_area) * 0.000247105,
    #     usfsnonroadless_buffered_area_acres = units::drop_units(usfsnonroadless_area) * 0.000247105,
    #     roadless_buff_twig_area_acres = units::drop_units(roadless_area_twig) * 0.000247105,
    #     usfsnonroadless_buff_twig_area_acres = units::drop_units(usfsnonroadless_area_twig) * 0.000247105,
    #     roadless_buff_twig_area_acres_corrected = roadless_buff_twig_area_acres / MEDIAN_TREATMENT_AREA_RATIO,
    #     usfsnonroadless_buff_twig_area_acres_corrected = usfsnonroadless_buff_twig_area_acres / MEDIAN_TREATMENT_AREA_RATIO
    #   )
    # )
    
    results <- tibble::tibble(
      state = state,
      
      # Total buffered area
      unexposed_roadless_buffered_area_acres =
        units::drop_units(unexposed_roadless_area) * 0.000247105,
      
      exposed_roadless_buffered_area_acres =
        units::drop_units(exposed_roadless_area) * 0.000247105,
      
      usfsnonroadless_buffered_area_acres =
        units::drop_units(usfsnonroadless_area) * 0.000247105,
      
      # TWIG area within buffers
      unexposed_roadless_buff_twig_area_acres =
        units::drop_units(unexposed_roadless_area_twig) * 0.000247105,
      
      exposed_roadless_buff_twig_area_acres =
        units::drop_units(exposed_roadless_area_twig) * 0.000247105,
      
      usfsnonroadless_buff_twig_area_acres =
        units::drop_units(usfsnonroadless_area_twig) * 0.000247105,
      
      # Forest area within buffers
      unexposed_roadless_buffered_forest_area_acres =
        unexposed_roadless_forest_area,
      
      exposed_roadless_buffered_forest_area_acres =
        exposed_roadless_forest_area,
      
      usfsnonroadless_buffered_forest_area_acres =
        usfsnonroadless_forest_area,
      
      # Forest area within TWIG treatments inside buffers
      unexposed_roadless_buff_twig_forest_area_acres =
        unexposed_roadless_twig_forest_area,
      
      exposed_roadless_buff_twig_forest_area_acres =
        exposed_roadless_twig_forest_area,
      
      usfsnonroadless_buff_twig_forest_area_acres =
        usfsnonroadless_twig_forest_area,
      
      # Corrected TWIG areas
      unexposed_roadless_buff_twig_area_acres_corrected =
        unexposed_roadless_buff_twig_area_acres /
        MEDIAN_TREATMENT_AREA_RATIO,
      
      exposed_roadless_buff_twig_area_acres_corrected =
        exposed_roadless_buff_twig_area_acres /
        MEDIAN_TREATMENT_AREA_RATIO,
      
      usfsnonroadless_buff_twig_area_acres_corrected =
        usfsnonroadless_buff_twig_area_acres /
        MEDIAN_TREATMENT_AREA_RATIO,
      
      # Corrected forest TWIG areas
      unexposed_roadless_buff_twig_forest_area_acres_corrected =
        unexposed_roadless_buff_twig_forest_area_acres /
        MEDIAN_TREATMENT_AREA_RATIO,
      
      exposed_roadless_buff_twig_forest_area_acres_corrected =
        exposed_roadless_buff_twig_forest_area_acres /
        MEDIAN_TREATMENT_AREA_RATIO,
      
      usfsnonroadless_buff_twig_forest_area_acres_corrected =
        usfsnonroadless_buff_twig_forest_area_acres /
        MEDIAN_TREATMENT_AREA_RATIO
    )
    
    saveRDS(results, result_fl)
    
    return(results)
  }
}


tic("Operate on top 10 states")

future::plan(future::multisession, workers = 5)

buffer_results <- top_10_roadless_area_states |>
  #purrr::set_names() |>
  furrr::future_map(
    #purrr::map(
    .f = purrr::safely(get_buffered_areas),
    .options = furrr::furrr_options(
      packages = c(
        "sf",
        "dplyr",
        "tibble",
        "units",
        "here",
        "terra",
        "exactextractr"
      ),
      seed = TRUE
    )
  )

future::plan(future::sequential)

buffer_results_bound <- buffer_results |>
  purrr::compact() |>
  dplyr::bind_rows()

write_csv(buffer_results_bound,
          here(dir_derived, "distance_aware_stats_by_state.csv"))

toc()


