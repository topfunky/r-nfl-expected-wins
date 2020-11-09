#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("mkuhn/dict")
#install.packages("tidyverse")
#install.packages("GGally")
library("tidyverse")
#library("GGally")
library(dict)

read_file_for_year <- function(year, name, skip = 0) {
  read.csv(str_interp("data/${year}/${name}.csv"), skip = skip)
}

# Team name sometimes ends in '*' or '+' to indicate conference or division winner.
clean_winner_star_plus <- function(data) {
  mutate(data, Tm = str_replace_all(Tm, '[*+]', ''))
}

calculate_offense_metrics <- function(data) {
  # ✔️Off Pass Yds/Att
  # ✔️Off Fumble Rate : fumbles per total number of plays
  # ✔️Off Int Rate : interceptions per pass attempt
  # ✔️Off Pen Rate : penalty yards per total number of plays
  # ✔️Off Run Yds/Att
  mutate(
    data,
    OffPassYardsPerAttempt = Yds.1 / Att,
    OffRunYardsPerAttempt = Yds.2 / Att.1,
    OffPenYardsPerPlay = Yds.3 / Ply,
    OffIntRate = Int / Att,
    OffFumbleRate = FL / Att
  ) %>% select(
    Tm,
    OffPassYardsPerAttempt,
    OffRunYardsPerAttempt,
    OffPenYardsPerPlay,
    OffIntRate,
    OffFumbleRate
  )
}

calculate_defense_metrics <- function(data) {
  # Def Pass Yds/Att
  # Def FFumble Rate
  # Def Int Rate
  # Def Run Yds/Att
  # Also: Def Penalty Yards Per Play
  mutate(
    data,
    DefPassYardsPerAttempt = Yds.1 / Att,
    DefRunYardsPerAttempt = Yds.2 / Att.1,
    DefPenYardsPerPlay = Yds.3 / Ply,
    DefIntRate = Int / Att,
    DefFumbleRate = FL / Att
  ) %>% select(
    Tm,
    DefPassYardsPerAttempt,
    DefRunYardsPerAttempt,
    DefPenYardsPerPlay,
    DefIntRate,
    DefFumbleRate
  )
}

build_stats_for_year <- function(year) {
  # Remove * and + from team name
  afc <-
    read_file_for_year(year, "afc") %>% clean_winner_star_plus()
  nfc <-
    read_file_for_year(year, "nfc") %>% clean_winner_star_plus()

  offense <-
    read_file_for_year(year, "offense", 1) %>% calculate_offense_metrics()
  defense <-
    read_file_for_year(year, "defense", 1) %>% calculate_defense_metrics()

  # Calculate avg, stddev
  # Calculate ZOffYardsPerAttempt, etc.
  # merge() on Tm so wins and off/def stats are in a single frame
  # Build lm model
  # Add field to each row with ActualWins and PredictedWins
  # Chart ActualWins and PredictedWins

  # For debugging
  d <- dict()
  d[["afc"]] <- afc
  d[["nfc"]] <- nfc
  d[["offense"]] <- offense
  d[["defense"]] <- defense
  return(d)
}

stats <- build_stats_for_year(2019)
