#if (!require("devtools")) install.packages("devtools")
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

source("theme_darker.R")

source("dvoa_expected_wins.R")

# Colors
vividchalk_cyan <- "#339999"
vividchalk_magenta <- "#9933CC"
vividchalk_yellow <- "#FFCC00"

# Mapping of abbreviation to name to mascot.
#
# Example: SEA, "Seattle Seahawks", "Seahawks"
read_team_names <- function() {
  read.csv(str_interp("data/team_names.csv"))
}

# Read basic stats.
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
    OffFumbleRate = FL / Ply
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
  # Def FFumble Rate : Fumbles per total plays
  # TODO: Currently using Fumbles; Needs to be Forced Fumbles only
  # Def Int Rate
  # Def Run Yds/Att

  tmpData <- mutate(
    data,
    DefPassYardsPerAttempt = Yds.1 / Att,
    DefRunYardsPerAttempt = Yds.2 / Att.1,
    DefIntRate = Int / Att,
    DefFumbleRate = FL / Ply
  ) %>% select(Tm,
               DefPassYardsPerAttempt,
               DefRunYardsPerAttempt,
               DefIntRate,
               DefFumbleRate)
  # Calculate overall mean, sd for each field
  DefPassYardsPerAttemptMean = mean(tmpData$DefPassYardsPerAttempt)
  DefPassYardsPerAttemptSd = sd(tmpData$DefPassYardsPerAttempt)

  DefRunYardsPerAttemptMean = mean(tmpData$DefRunYardsPerAttempt)
  DefRunYardsPerAttemptSd = sd(tmpData$DefRunYardsPerAttempt)

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
    ZDefIntRate = (DefIntRate - DefIntRateMean) / DefIntRateSd,
    ZDefFumbleRate = (DefFumbleRate - DefFumbleRateMean) / DefFumbleRateSd
  )

  return(tmpData)
}

build_stats_for_year <- function(year) {
  # Remove * and + from team name
  afc <-
    read_file_for_year(year, "afc") %>% clean_winner_star_plus() %>% select(Tm, W, PF, PA)
  nfc <-
    read_file_for_year(year, "nfc") %>% clean_winner_star_plus() %>% select(Tm, W, PF, PA)
  nflTeams <- bind_rows(afc, nfc)

  offense <-
    read_file_for_year(year, "offense", 1) %>% filter(G > 0) %>% calculate_offense_metrics()
  defense <-
    read_file_for_year(year, "defense", 1) %>% filter(G > 0) %>% calculate_defense_metrics()

  team_names <- read_team_names()
  dvoa_data <- dvoa_load_data_for_year(year)
  dvoa_data <- merge(dvoa_data, team_names)

  # Merge so all attributes are in a single data frame.
  #
  # The correct field name will be detected (Tm or TEAM).
  data <-
    merge(nflTeams, offense) %>%
    merge(defense) %>%
    merge(dvoa_data) %>%
    mutate(Year = year)

  return(data)
}

load_data <- function(start_year, end_year) {
  # For each year, build stats.
  data <- data.frame()
  for (i in seq(start_year, end_year, by = 1)) {
    stats <- build_stats_for_year(i)
    data <- bind_rows(data, stats)
  }
  return(data)
}

plot_wins <- function(data, start_year, end_year, title) {
  # Chart ActualWins and PredictedWins
  chart <-
    ggplot(data = data, aes(x = Year, y = W)) +
    # Reference line: 8 wins
    geom_hline(yintercept = 8, color = "#d8d8d8") +
    # Vertical line: current year
    geom_vline(
      xintercept = 2020,
      color = "grey90",
      size = 4,
      alpha = 0.1
    ) +
    # Predicted wins
    geom_line(aes(x = Year, y = PredictedW), color = vividchalk_cyan) +
    # Pythagorean wins
    geom_line(aes(x = Year, y = PythagoreanW), color = vividchalk_magenta) +
    # DVOA predicted wins
    geom_line(aes(x = Year, y = DVOAW), color = vividchalk_yellow) +
    # Actual
    geom_line(color = "white", size = 1) + geom_point(color = "white") +
    # Styling
    scale_y_continuous(breaks = seq(0, 16, by = 4)) +
    theme_minimal() +
    labs(
      title = str_interp("${title}, ${start_year}-${end_year}"),
      subtitle = "Predicted wins from an efficiency metrics model (cyan), Pythagorean wins (magenta), DVOA (yellow)",
      y = "Wins",
      caption = "Based on data from pro-football-reference.com and footballoutsiders.com"
    ) +
    theme_darker() +
    facet_wrap(~ TEAM.MASCOT)
}

plot_models_only <- function(data, start_year, end_year, title) {
  # Chart model prediction lines
  chart <-
    ggplot(data = data, aes(x = Year, y = W)) +
    # Reference line: 8 wins
    geom_hline(yintercept = 8, color = "#d8d8d8") +
    # Predicted wins
    geom_line(aes(x = Year, y = PredictedW), color = vividchalk_cyan) +
    annotate(
      "text",
      x = 2002,
      y = 10,
      label = "AFA",
      color = vividchalk_cyan,
      family = "InputMono"
    ) +
    # Pythagorean wins
    geom_line(aes(x = Year, y = PythagoreanW), color = vividchalk_magenta) +
    annotate(
      "text",
      x = 2013.75,
      y = 9,
      label = "Pythagorean",
      color = vividchalk_magenta,
      family = "InputMono"
    ) +
    # DVOA predicted wins
    geom_line(aes(x = Year, y = DVOAW), color = vividchalk_yellow) +
    annotate(
      "text",
      x = 2006,
      y = 5,
      label = "DVOA",
      color = vividchalk_yellow,
      family = "InputMono"
    ) +
    # Styling
    scale_y_continuous(breaks = seq(0, 16, by = 4)) +
    theme_minimal() +
    labs(
      title = str_interp("${title}, ${start_year}-${end_year}"),
      subtitle = "Three models",
      y = "Wins",
      caption = "Data from pro-football-reference.com and footballoutsiders.com"
    ) +
    theme_darker() +
    facet_wrap(~ TEAM.MASCOT)
}

# Based a model described by Advanced Football Analytics
# http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-4.html
build_regression_model <- function(data) {
  lm(
    W ~ ZDefPassYardsPerAttempt +
      ZDefRunYardsPerAttempt +
      ZDefIntRate +
      ZDefFumbleRate +
      ZOffPassYardsPerAttempt +
      ZOffRunYardsPerAttempt +
      ZOffPenYardsPerPlay +
      ZOffIntRate  +
      ZOffFumbleRate,
    data = data
  )
}

# From Football Outsiders' defense adjusted value over average.
# https://www.footballoutsiders.com/stats/nfl/team-efficiency/2020
build_dvoa_regression_model <- function(data) {
  lm(W ~ OFFENSEDVOA + DEFENSEDVOA + S.T.DVOA,
     data = data)
}

# Traditional Bill James method for calculating expected wins.
# https://en.wikipedia.org/wiki/Pythagorean_expectation
calculate_pythagorean_wins <- function(PF, PA) {
  (1 / (1 + (PA / PF) ^ 2)) * 16
}

load_data_and_build_model <- function(training_years, all_years) {
  training_data <- load_data(training_years[1], training_years[2])
  # Run regression model on training years
  nfl_win_model <- build_regression_model(training_data)
  dvoa_win_model <- build_dvoa_regression_model(training_data)

  # Load other years for prediction and display
  data <- load_data(all_years[1], all_years[2])

  # Add field to each row of `data` with PredictedW
  data <-
    mutate(
      data,
      PredictedW = predict(nfl_win_model, data[row_number(), ]),
      PythagoreanW = calculate_pythagorean_wins(PF, PA),
      DVOAW = predict(dvoa_win_model, data[row_number(), ])
    )
  return(data)
}

run_report <- function() {
  training_years <- c(2015, 2019)
  all_years <- c(2002, 2020)

  data <- load_data_and_build_model(training_years, all_years)

  if (!dir.exists("out")) {
    dir.create("out")
  }

  # All teams, all models and predicted wins.
  plot <-
    plot_wins(data,
              all_years[1],
              all_years[2],
              "NFL Predicted Wins vs Actual Wins")
  ggsave(
    plot = plot,
    filename = "out/wins.png",
    width = 12,
    height = 8
  )

  # Only the Patriots and Browns with all models and actual wins.
  data_two_teams <-
    data %>% filter(TEAM.MASCOT == "Patriots" |
                      TEAM.MASCOT == "Browns")
  plot2 <-
    plot_wins(data_two_teams, all_years[1], all_years[2], "Browns and Patriots")
  ggsave(
    plot = plot2,
    filename = "out/wins-detail.png",
    width = 12,
    height = 4
  )

  # Only the Seahawks for a plot that shows the three prediction models
  # (but not actual wins).
  data_seahawks <-
    data %>% filter(TEAM.MASCOT == "Seahawks", Year < 2020)
  plot_models <-
    plot_models_only(data_seahawks, all_years[1], 2019, "Prediction Models")
  ggsave(
    plot = plot_models,
    filename = "out/wins-models.png",
    width = 6,
    height = 4
  )
}

run_report()
