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
  tmpData <- mutate(
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

  # Calculate overall mean, sd for each field
  OffPassYardsPerAttemptMean = mean(tmpData$OffPassYardsPerAttempt)
  OffPassYardsPerAttemptSd = sd(tmpData$OffPassYardsPerAttempt)

  OffRunYardsPerAttemptMean = mean(tmpData$OffRunYardsPerAttempt)
  OffRunYardsPerAttemptSd = sd(tmpData$OffRunYardsPerAttempt)

  OffPenYardsPerPlayMean = mean(tmpData$OffPenYardsPerPlay)
  OffPenYardsPerPlaySd = sd(tmpData$OffPenYardsPerPlay)

  OffIntRateMean = mean(tmpData$OffIntRate)
  OffIntRateSd = sd(tmpData$OffIntRate)

  OffFumbleRateMean = mean(tmpData$OffFumbleRate)
  OffFumbleRateSd = sd(tmpData$OffFumbleRate)

  # Calculate Stddev weighted value for each metric
  tmpData <- mutate(
    tmpData,
    ZOffPassYardsPerAttempt = (OffPassYardsPerAttempt - OffPassYardsPerAttemptMean) /
      OffPassYardsPerAttemptSd,
    ZOffRunYardsPerAttempt = (OffRunYardsPerAttempt - OffRunYardsPerAttemptMean) / OffRunYardsPerAttemptSd,
    ZOffPenYardsPerPlay = (OffPenYardsPerPlay - OffPenYardsPerPlayMean) / OffPenYardsPerPlaySd,
    ZOffIntRate = (OffIntRate - OffIntRateMean) / OffIntRateSd,
    ZOffFumbleRate = (OffFumbleRate - OffFumbleRateMean) / OffFumbleRateSd
  )

  return(tmpData)
}

calculate_defense_metrics <- function(data) {
  # Def Pass Yds/Att
  # Def FFumble Rate
  # TODO: Currently using Fumbles; Needs to be Forced Fumbles only
  # Def Int Rate
  # Def Run Yds/Att
  # Also: Def Penalty Yards Per Play

  tmpData <- mutate(
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
  # Calculate overall mean, sd for each field
  DefPassYardsPerAttemptMean = mean(tmpData$DefPassYardsPerAttempt)
  DefPassYardsPerAttemptSd = sd(tmpData$DefPassYardsPerAttempt)

  DefRunYardsPerAttemptMean = mean(tmpData$DefRunYardsPerAttempt)
  DefRunYardsPerAttemptSd = sd(tmpData$DefRunYardsPerAttempt)

  DefPenYardsPerPlayMean = mean(tmpData$DefPenYardsPerPlay)
  DefPenYardsPerPlaySd = sd(tmpData$DefPenYardsPerPlay)

  DefIntRateMean = mean(tmpData$DefIntRate)
  DefIntRateSd = sd(tmpData$DefIntRate)

  DefFumbleRateMean = mean(tmpData$DefFumbleRate)
  DefFumbleRateSd = sd(tmpData$DefFumbleRate)

  # Calculate Stddev weighted value for each metric
  tmpData <- mutate(
    tmpData,
    ZDefPassYardsPerAttempt = (DefPassYardsPerAttempt - DefPassYardsPerAttemptMean) /
      DefPassYardsPerAttemptSd,
    ZDefRunYardsPerAttempt = (DefRunYardsPerAttempt - DefRunYardsPerAttemptMean) / DefRunYardsPerAttemptSd,
    ZDefPenYardsPerPlay = (DefPenYardsPerPlay - DefPenYardsPerPlayMean) / DefPenYardsPerPlaySd,
    ZDefIntRate = (DefIntRate - DefIntRateMean) / DefIntRateSd,
    ZDefFumbleRate = (DefFumbleRate - DefFumbleRateMean) / DefFumbleRateSd
  )

  return(tmpData)
}

build_stats_for_year <- function(year) {
  # Remove * and + from team name
  afc <-
    read_file_for_year(year, "afc") %>% clean_winner_star_plus() %>% select(Tm, W)
  nfc <-
    read_file_for_year(year, "nfc") %>% clean_winner_star_plus() %>% select(Tm, W)
  nflTeams <- bind_rows(afc, nfc)

  offense <-
    read_file_for_year(year, "offense", 1) %>% calculate_offense_metrics()
  defense <-
    read_file_for_year(year, "defense", 1) %>% calculate_defense_metrics()

  # merge() on Tm so wins and off/def stats are in a single frame
  offenseWithWins <- merge(nflTeams, offense)
  allColumns <- merge(offenseWithWins, defense)

  # Build lm model
  # TODO: Build model from all years
  m <- lm(
    W ~ ZDefPassYardsPerAttempt +
      ZDefRunYardsPerAttempt +
      ZDefPenYardsPerPlay +
      ZDefIntRate +
      ZDefFumbleRate +
      ZOffPassYardsPerAttempt +
      ZOffRunYardsPerAttempt +
      ZOffPenYardsPerPlay +
      ZOffIntRate  +
      ZOffFumbleRate,
    data = allColumns
  )

  # Add field to each row with ActualWins and PredictedWins
  # Chart ActualWins and PredictedWins

  # For debugging
  d <- dict()
  d[["nflTeams"]] <- nflTeams
  d[["offense"]] <- offense
  d[["defense"]] <- defense
  d[["allColumns"]] <- allColumns
  d[["m"]] <- m

  return(d)
}

# TODO: For each year, build stats and `bind_rows()` to append to full dataset
#for (i in seq(2002, 2019, by=1)) {
#  stats <- build_stats_for_year(i)
#  bind rows to full dataset
#}
stats <- build_stats_for_year(2019)
