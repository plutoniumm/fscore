library(f1dataR)
library(dplyr)
library(ggplot2)

options(f1dataR.cache = "./data/")

log = function(...) {print(paste(...))}
wipe = function() {cat(paste(rep(" ", 30), collapse = ""), "\r")}

# return first 4 moments: µ, σ², skw, kurt
process_points <- function(data) {
  n <- length(data)
  vmean <- mean(data)
  vsd <- sd(data)/vmean

  vskw <- (n * sum((data - vmean)^3)) / ((n - 1) * (n - 2) * vsd^3)
  vkurt <- (n * sum((data - vmean)^4)) / ((n - 1) * (n - 2) * (n - 3) * vsd^4) - 3

  vsd = round(vsd, 2)
  vskw = round(vskw, 2)
  vkurt = round(vkurt, 2)

  return(c(vsd, vskw, vkurt))
}

get_season <- function(season) {
  sched <- load_standings(season=season, round='last')
  points <- as.numeric(sched$points)

  return(process_points(points))
}

seasons <- 2003:2024
rsds <- c()
for (season in seasons) {
  data <- get_season(season)
  rsds <- c(rsds, data[1])

  cat(paste("Season", season, "RSD:", data[1]), "\n")
}


df <- data.frame(season=seasons, rsd=rsds)
ggplot(df, aes(x=season, y=rsd)) +
  geom_line() +
  geom_point() +
  labs(title="RSD F1 points", x="Season", y="RSD") +
  theme_minimal()