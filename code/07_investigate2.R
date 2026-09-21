
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

buff_results <- read_csv(here(dir_derived, "distance_aware_stats_by_state.csv"))

buff_results <- buff_results |>
  mutate(roadless_perc_of_available = roadless_buff_twig_area_acres_corrected / roadless_buffered_area_acres * 100,
         usfsnonroadless_perc_of_available = usfsnonroadless_buff_twig_area_acres_corrected / usfsnonroadless_buffered_area_acres * 100)

buff_results_long <- buff_results |>
  tidyr::pivot_longer(
    cols = -state,
    names_to = c("management", ".value"),
    names_pattern = "^(roadless|usfsnonroadless)_(.*)$"
  )


summary <- buff_results_long |>
  group_by(management) |>
  summarize(buffered_area_acres = sum(buffered_area_acres),
            buff_twig_area_acres_corrected = sum(buff_twig_area_acres_corrected)) |>
  mutate(perc_of_available = buff_twig_area_acres_corrected / buffered_area_acres * 100)



ggplot(buff_results_long) +
  geom_col(aes(x = state, y = perc_of_available, fill = management), position = "dodge") +
  geom_hline(aes(yintercept = 2.95), color = "salmon2", lty = "dotted", lwd = 1.5) +
  geom_hline(aes(yintercept = 7.62), color = "turquoise3", lty = "dotted", lwd = 1.5) +
  theme_minimal() +
  labs(title = "Estimated percentage of area treated within state-specific road buffers",
       x = "State",
       y = "Percentage of near-road area treated",
       caption = "State specific road buffers computed based on median distance to treatment per state\n
       Estimated treated area computed by using geographic union overlap \n
       and data-driven correction factor between geographic/reported\n
       NOTE: Does NOT take into account presence of forested ecosystem near roads\n
       Dotted lines indicate state-summed values (2.95% and 7.62%)")

ggsave(filename = here(dir_figs, "treated_within_buffers_by_state.png"))


