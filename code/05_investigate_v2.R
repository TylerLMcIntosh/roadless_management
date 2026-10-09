
library(tigris)
library(sf)
library(tidyverse)
library(here)
library(mapview)
library(glue)
library(ggridges)
library(patchwork)

dir_dats <- here("data/raw")
dir_derived <- here("data/derived")
dir_figs <- here("figs")

dats <- sf::st_read(here(dir_derived, "twig_points_management_distance_added_5070_v2.gpkg"))


centroid_distances <- dats |>
  dplyr::group_by(type, STATE) |>
  dplyr::summarize(mean_road_dist = mean(dist_to_road, na.rm = TRUE),
                   median_road_dist = median(dist_to_road, na.rm = TRUE),
                   min_road_dist = min(dist_to_road, na.rm = TRUE),
                   max_road_dist = max(dist_to_road, na.rm = TRUE),
                   n_trt = n())

ggplot(dats) +
  geom_density(aes(x = dist_to_road)) +
  xlim(c(0, 1000)) +
  geom_vline(xintercept = mean(dats$dist_to_road), color = "red") +
  geom_vline(xintercept = median(dats$dist_to_road), color = "blue")



ggplot(dats) +
  geom_density(aes(x = dist_to_road, color = type)) +
  coord_cartesian(xlim = c(0, 1000))

ggplot(dats) +
  geom_density(aes(x = dist_to_road, color = STATE)) +
  xlim(xlim = c(0, 1000)) +
  ylim(xlim = c(0, 0.008))



ggplot(dats) +
  geom_density(aes(x = dist_to_road)) +
  xlim(xlim = c(0, 1000)) +
  geom_vline(xintercept = mean(dats$dist_to_road), color = "red") +
  geom_vline(xintercept = median(dats$dist_to_road), color = "blue")



# By state ggridges
state_summary <- dats |>
  dplyr::group_by(STATE) |>
  dplyr::summarise(
    mean_dist = mean(dist_to_road, na.rm = TRUE),
    median_dist = median(dist_to_road, na.rm = TRUE)
  )


state_summary_long <- state_summary |>
  tidyr::pivot_longer(
    cols = c(mean_dist, median_dist),
    names_to = "stat",
    values_to = "dist"
  )

p_summary <- ggplot(state_summary_long, aes(x = dist, color = stat)) +
  geom_density(linewidth = 1) +
  scale_color_manual(
    values = c(
      mean_dist = "red",
      median_dist = "blue"
    ),
    labels = c(
      mean_dist = "Mean",
      median_dist = "Median"
    )
  ) +
  coord_cartesian(xlim = c(0, 750)) +
  labs(
    x = NULL,
    y = "Density",
    color = NULL
  ) +
  theme(legend.position = "top")

p_states <- ggplot(dats, aes(x = dist_to_road, y = state_summary_long)) +
  ggridges::geom_density_ridges(scale = 1.5) +
  geom_point(
    data = state_summary,
    aes(x = mean_dist, y = STATE),
    color = "red",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = state_summary,
    aes(x = median_dist, y = STATE),
    color = "blue",
    inherit.aes = FALSE
  ) +
  coord_cartesian(xlim = c(0, 750)) +
  labs(
    title = "Treatment distances from roads across states",
    subtitle = "(All types put together)"#,
    #caption = "Blue dot = median; red dot = mean"
  )

p1 <- p_summary / p_states +
  patchwork::plot_layout(heights = c(0.2, 4))
ggsave(filename = here(dir_figs, "dist_to_roads_by_state.png"),
       p1,
       units = "px",
       width = 1500,
       height = 3000)



# by type ggridges
# By state ggridges
type_summary <- dats |>
  dplyr::group_by(type) |>
  dplyr::summarise(
    mean_dist = mean(dist_to_road, na.rm = TRUE),
    median_dist = median(dist_to_road, na.rm = TRUE)
  )


type_summary_long <- type_summary |>
  tidyr::pivot_longer(
    cols = c(mean_dist, median_dist),
    names_to = "stat",
    values_to = "dist"
  )

p_type_summary <- ggplot(type_summary_long, aes(x = dist, color = stat)) +
  geom_density(linewidth = 1) +
  scale_color_manual(
    values = c(
      mean_dist = "red",
      median_dist = "blue"
    ),
    labels = c(
      mean_dist = "Mean",
      median_dist = "Median"
    )
  ) +
  coord_cartesian(xlim = c(0, 750)) +
  labs(
    x = NULL,
    y = "Density",
    color = NULL
  ) +
  theme(legend.position = "top")

p_types <- ggplot(dats, aes(x = dist_to_road, y = type)) +
  ggridges::geom_density_ridges(scale = 1.5) +
  geom_point(
    data = type_summary,
    aes(x = mean_dist, y = type),
    color = "red",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = type_summary,
    aes(x = median_dist, y = type),
    color = "blue",
    inherit.aes = FALSE
  ) +
  coord_cartesian(xlim = c(0, 750)) +
  labs(
    title = "Treatment distances from roads across types",
    subtitle = "(All states put together)"#,
    #caption = "Blue dot = median; red dot = mean"
  )

p2 <- p_type_summary / p_types +
  patchwork::plot_layout(heights = c(0.4, 4))
ggsave(filename = here(dir_figs, "dist_to_roads_by_trt.png"),
       p2,
       units = "px",
       width = 1500,
       height = 2000)

write_csv(state_summary |> sf::st_drop_geometry(),
          here(dir_derived, "distance_state_summary.csv"))

write_csv(type_summary |> sf::st_drop_geometry(),
          here(dir_derived, "distance_type_summary.csv"))





# summary stats
agg_results <- read_csv(here(dir_derived, "state_perc_treated_results.csv"))

# agg_results <- agg_results |>
#   dplyr::group_by(STATE) |>
#   dplyr::filter(
#     all(c("roadless", "usfs non-roadless") %in% management)
#   ) |>
#   dplyr::ungroup()

area_summary <- dats |>
  dplyr::filter(STATE %in% unique(agg_results$STATE)) |>
  dplyr::group_by(exposure_group) |>
  dplyr::summarise(reported_acres = sum(acres, na.rm = TRUE),
                   reported_cost = sum(total_cost, na.rm = TRUE),
                   n_trt = n(),
                   n_trt_w_acres = sum(!is.na(acres)),
                   n_trt_w_cost = sum(!is.na(total_cost)),
                   .groups = "drop") |>
  sf::st_drop_geometry()

man_area <- agg_results |>
  dplyr::group_by(exposure_group) |>
  dplyr::summarise(MANAGE_AREA_ACRES = sum(MANAGE_AREA_ACRES))

area_summary <- area_summary |>
  left_join(man_area) |>
  mutate(perc_treated = reported_acres / MANAGE_AREA_ACRES * 100,
         version = "All")


ggplot(agg_results) +
  geom_col(aes(x = STATE, y = MANAGE_AREA_ACRES, fill = exposure_group), position = "dodge")
ggsave(filename = here(dir_figs, "management_area_by_state.png"),
       units = "px",
       width = 2500,
       height = 1500)

ggplot(area_summary) +
  geom_col(aes(x = exposure_group, y = perc_treated))

# without AK

man_area_noak <- agg_results |>
  filter(STATE != "AK") |>
  dplyr::group_by(exposure_group) |>
  dplyr::summarise(MANAGE_AREA_ACRES = sum(MANAGE_AREA_ACRES))

area_summary_noak <- dats |>
  dplyr::filter(STATE %in% unique(agg_results$STATE)) |>
  dplyr::filter(STATE != "AK") |>
  dplyr::group_by(exposure_group) |>
  dplyr::summarise(reported_acres = sum(acres, na.rm = TRUE),
                   reported_cost = sum(total_cost, na.rm = TRUE),
                   n_trt = n(),
                   n_trt_w_acres = sum(!is.na(acres)),
                   n_trt_w_cost = sum(!is.na(total_cost)),
                   .groups = "drop") |>
  sf::st_drop_geometry()

area_summary_noak <- area_summary_noak |>
  left_join(man_area_noak) |>
  mutate(perc_treated = reported_acres / MANAGE_AREA_ACRES * 100,
         version = "No-AK")


both_area_summaries <- area_summary |>
  rbind(area_summary_noak)

ggplot(both_area_summaries) +
  geom_col(aes(x = exposure_group, y = perc_treated, fill = version), position = "dodge") +
  theme_minimal() +
  labs(title = "Area treated as percent of management type total area")
ggsave(filename = here(dir_figs, "percent_treated_summary.png"),
       units = "px",
       width = 2500,
       height = 1500)




top_10_roadless_area <- agg_results |>
  filter(exposure_group == "Exposed roadless") |>
  arrange(desc(MANAGE_AREA_ACRES)) |>
  slice_head(n = 10)


top_10_roadless_area_treated <- agg_results |>
  filter(exposure_group == "Exposed roadless") |>
  arrange(desc(perc_treated)) |>
  slice_head(n = 10)



# states as factor
top_10_roadless_area <- top_10_roadless_area |>
  dplyr::mutate(
    STATE = factor(STATE, levels = unique(STATE))
  )

top_ten_roadless_area_states <- top_10_roadless_area |> pull(STATE)

top_10_roadless_area_treated <- top_10_roadless_area_treated |>
  dplyr::mutate(
    STATE = factor(STATE, levels = unique(STATE))
  )


v1 <- ggplot(top_10_roadless_area) +
  geom_col(
    aes(
      x = STATE,
      y = perc_treated,
      fill = STATE == "UT"
    )
  ) +
  scale_fill_manual(
    values = c(`FALSE` = "grey60", `TRUE` = "red"),
    guide = "none"
  ) +
  theme_minimal() +
  labs(title = "Percent treated: Top 10 states by exposed roadless area")

v2 <- ggplot(top_10_roadless_area_treated) +
  geom_col(
    aes(
      x = STATE,
      y = perc_treated,
      fill = STATE == "UT"
    )
  ) +
  scale_fill_manual(
    values = c(`FALSE` = "grey60", `TRUE` = "red"),
    guide = "none"
  ) +
  theme_minimal() +
  labs(title = "Percent treated: Top 10 states by treated area / exposed roadless area",
       caption = "Top ten's going from left to right") +
  coord_cartesian(ylim = c(0, 100))


v3 <- v1 / v2
v3

ggsave(filename = here(dir_figs, "top_ten_plot.png"),
       v3)



# col by state top ten
area_summary_tojoin <- area_summary |> rename(STATE = version)

area_summary_joined <- agg_results |> rbind(area_summary_tojoin)

top_ten_col <- area_summary_joined |>
  dplyr::filter(
    STATE %in% top_ten_roadless_area_states | STATE == "All"
  ) |>
  dplyr::mutate(
    STATE = factor(
      STATE,
      levels = c("All", levels(top_ten_roadless_area_states))
    ),
    exposure_group = factor(
      exposure_group,
      levels = c(
        "Non-roadless",
        "Unexposed roadless",
        "Exposed roadless"
      )
    )
  ) |>
  ggplot(
    aes(
      x = STATE,
      y = perc_treated,
      fill = exposure_group,
      color = STATE == "All"
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.9),
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Non-roadless" = "cadetblue4",
      "Unexposed roadless" = "goldenrod3",
      "Exposed roadless" = "deeppink3"
    )
  ) +
  scale_color_manual(
    values = c(
      `TRUE` = "black",
      `FALSE` = NA
    ),
    guide = "none"
  ) +
  theme_minimal() +
  labs(x = "Top ten states by exposed roadless area",
       y = "Area treated as percentage of total area",
       fill = "Exposure group")

top_ten_col

ggsave(top_ten_col, filename = here(dir_figs, "summary_fig_1.png"))



ratio_by_state <- agg_results |>
  dplyr::filter(
    state %in% top_ten_roadless_area_states,
    management %in% c("roadless", "usfs non-roadless")
  ) |>
  dplyr::select(state, management, perc_treated, MANAGE_AREA_ACRES) |>
  tidyr::pivot_wider(
    names_from = management,
    values_from = c(perc_treated, MANAGE_AREA_ACRES)
  ) |>
  dplyr::mutate(
    treatment_ratio =
      perc_treated_roadless / `perc_treated_usfs non-roadless`
  ) |>
  dplyr::transmute(
    state,
    roadless_MANAGE_AREA_ACRES = MANAGE_AREA_ACRES_roadless,
    roadless_perc_treated = perc_treated_roadless,
    usfs_nonroadless_perc_treated = `perc_treated_usfs non-roadless`,
    treatment_ratio
  ) |>
  arrange(desc(roadless_MANAGE_AREA_ACRES)) |>
  dplyr::mutate(
    state = factor(state, levels = unique(state))
  )

ratio_p <- ggplot(ratio_by_state) +
  geom_col(aes(x = state, y = treatment_ratio)) +
  labs(title = "Roadless/non-roadless treatment ratio, normalized by potential area",
       subtitle = "Top ten states by roadless area (left to right)",
       x = "State",
       y = "Roadless area treated / USFS non-roadless area treated") +
  theme_minimal()
ratio_p
# ggsave(filename = here(dir_figs, "top_ten_trt_ratio_plot.png"),
#        ratio_p)

overall_ratios <- agg_results |>
  dplyr::filter(
    state %in% top_ten_roadless_area_states,
    management %in% c("roadless", "usfs non-roadless")
  ) |>
  dplyr::group_by(management) |>
  dplyr::summarise(
    reported_acres = sum(reported_acres, na.rm = TRUE),
    MANAGE_AREA_ACRES = sum(MANAGE_AREA_ACRES, na.rm = TRUE),
    perc_treated = reported_acres / MANAGE_AREA_ACRES,
    .groups = "drop"
  ) |>
  dplyr::select(
    management,
    reported_acres,
    perc_treated
  ) |>
  tidyr::pivot_wider(
    names_from = management,
    values_from = c(reported_acres, perc_treated)
  ) |>
  dplyr::summarise(
    `Normalized by available area` =
      perc_treated_roadless /
      `perc_treated_usfs non-roadless`,
    `Raw treated area` =
      reported_acres_roadless /
      `reported_acres_usfs non-roadless`
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "ratio_type",
    values_to = "ratio"
  ) |>
  dplyr::mutate(
    label = paste0("Overall = ", round(ratio, 2))
  )


ratio_compare_by_state <- agg_results |>
  dplyr::filter(
    state %in% top_ten_roadless_area_states,
    management %in% c("roadless", "usfs non-roadless")
  ) |>
  dplyr::select(
    state,
    management,
    perc_treated,
    MANAGE_AREA_ACRES,
    reported_acres
  ) |>
  tidyr::pivot_wider(
    names_from = management,
    values_from = c(
      perc_treated,
      MANAGE_AREA_ACRES,
      reported_acres
    )
  ) |>
  dplyr::mutate(
    normalized_ratio =
      perc_treated_roadless / `perc_treated_usfs non-roadless`,
    raw_area_ratio =
      reported_acres_roadless / `reported_acres_usfs non-roadless`
  ) |>
  dplyr::transmute(
    state,
    roadless_MANAGE_AREA_ACRES = MANAGE_AREA_ACRES_roadless,
    normalized_ratio,
    raw_area_ratio
  ) |>
  dplyr::arrange(desc(roadless_MANAGE_AREA_ACRES)) |>
  dplyr::mutate(
    state = factor(state, levels = unique(state))
  ) |>
  tidyr::pivot_longer(
    cols = c(normalized_ratio, raw_area_ratio),
    names_to = "ratio_type",
    values_to = "ratio"
  ) |>
  dplyr::mutate(
    ratio_type = dplyr::recode(
      ratio_type,
      normalized_ratio = "Normalized by available area",
      raw_area_ratio = "Raw treated area"
    )
  )
ratio_compare_p <- ggplot() +
  geom_col(
    data = ratio_compare_by_state,
    aes(
      x = state,
      y = ratio,
      fill = ratio_type
    ),
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  # geom_hline(
  #   yintercept = 1,
  #   linetype = "dashed"
  # ) +
  geom_hline(
    data = overall_ratios,
    aes(
      yintercept = ratio,
      linetype = ratio_type
    ),
    linewidth = 0.8
  ) +
  geom_text(
    data = overall_ratios,
    aes(
      x = "NM",
      y = ratio,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = -0.4
  ) +
  labs(
    title = "Roadless/non-roadless treatment ratios",
    subtitle = "Top ten states by roadless area (left to right)",
    x = "State",
    y = "Roadless / USFS non-roadless",
    fill = NULL,
    linetype = "Overall ratio",
    caption = "The y axis value indicates the ratio of treatments occurring in roadless\n areas compared to non-roadless areas.\nFor example, a value of 0.5 indicates that half as much area was treated in\nroadless areas as in non-roadless.\nThe red bars are results normalized by available area of each type"
  ) +
  theme_minimal()


ratio_compare_p

ggsave(
  filename = here(
    dir_figs,
    "top_ten_trt_ratio_normalized_vs_raw.png"
  ),
  plot = ratio_compare_p
)



# reported vs calculated area difference
trt_area_compare <- dats |>
  select(acres, shape_Area) |>
  sf::st_drop_geometry() |>
  mutate(shape_MANAGE_AREA_ACRES = shape_Area * 0.000247105,
         ratio = shape_MANAGE_AREA_ACRES / acres)

med <- median(trt_area_compare$ratio)

ggplot(trt_area_compare) +
  geom_density(aes(x = ratio)) +
  geom_vline(xintercept = med, color = "red") +
  theme_minimal() +
  labs(title = "Geospatial versus reported area summary",
       x = "Shape area / reported area",
       caption = glue("Median ratio = {round(med, 2)}"))

ggsave(filename = here(dir_figs, "twig_trt_area_compare.png"))
