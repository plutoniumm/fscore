library(f1dataR)
library(dplyr)

options(f1dataR.cache = "./data/")

wipe = function() {
  cat(paste(rep(" ", 30), collapse = ""), "\r")
}

get_lap_swaps <- function(results) {
  results$position <- as.numeric(results$position)
  results$position[results$position == "None"] <- 20

  results <- results %>%
    mutate(position = as.numeric(position)) %>%  # Ensure position is numeric
    arrange(driver_id, lap) %>%
    group_by(driver_id) %>%
    mutate(pos_change = abs(position - lag(position, default = first(position)))) %>%
    summarise(avg_pos_change = mean(pos_change, na.rm = TRUE), .groups = 'drop')

  mean(results$avg_pos_change, na.rm = TRUE)
}

get_season_results <- function(season) {
  sched <- load_schedule(season = season)
  num_races <- nrow(sched)

  season_swaps <- 0
  three_swaps <- 0

  for (round in 1:num_races) {
    results <- load_laps(season = season, round = round)
    results <- results %>% mutate(position = as.numeric(position))
    swap_count <- get_lap_swaps(results)

    wipe()
    cat(sprintf("Round %d: Swaps = %.2f @ %s\r", round, swap_count, sched$circuit_name[round]))

    season_swaps <- season_swaps + swap_count
    if (round <= 3) {
      three_swaps <- three_swaps + swap_count
    }
  }

  list(season_swaps = season_swaps / num_races,
       three_swaps = three_swaps / min(3, num_races))
}

seasons <- 2014:2020
for (season in seasons) {
  swaps <- get_season_results(season)

  wipe()
  cat(sprintf("Season %d: Swaps = %.2f, First3 = %.2f\n",
              season, swaps$season_swaps, swaps$three_swaps))
}
