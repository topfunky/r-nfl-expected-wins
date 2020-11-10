#if (!require("devtools")) install.packages("devtools")
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

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
  # Also: Def Penalty Yards Per Play

  tmpData <- mutate(
    data,
    DefPassYardsPerAttempt = Yds.1 / Att,
    DefRunYardsPerAttempt = Yds.2 / Att.1,
    DefPenYardsPerPlay = Yds.3 / Ply,
    DefIntRate = Int / Att,
    DefFumbleRate = FL / Ply
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
    read_file_for_year(year, "offense", 1) %>% filter(G > 0) %>% calculate_offense_metrics()
  defense <-
    read_file_for_year(year, "defense", 1) %>% filter(G > 0) %>% calculate_defense_metrics()

  # merge() on Tm so wins and off/def stats are in a single frame
  data <-
    merge(nflTeams, offense) %>% merge(defense) %>% mutate(Year = year)

  return(data)
}

build_regression_model <- function(data) {
  lm(
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
    data = data
  )
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

plot_wins <- function(data) {
  # Chart ActualWins and PredictedWins
  chart <-
    ggplot(data = data, aes(x = Year, y =
                              W)) +
    # Predicted
    geom_line(aes(x = Year, y = PredictedW), color = "grey") +
    geom_point(aes(x =
                     Year, y = PredictedW), color = "grey") +
    # Actual
    geom_line() + geom_point() +
    # Styling
    ylim(0, 16) + theme_minimal() +
    labs(title = "NFL Predicted vs Actual Wins, 2002-2019",
         subtitle = "Predicted wins (grey) calculated from offense, defense, and turnover efficiency metrics") +
    facet_wrap(~ Tm) + style_fonts("Sentinel", "Avenir", "InputSans")
  ggsave(
    plot = chart,
    filename = "wins.png",
    width = 16,
    height = 9
  )
}

style_fonts <-
  function(title_font,
           subtitle_font,
           mono_font) {
    theme(
      plot.title = ggplot2::element_text(
        family = title_font,
        size = 36,
        face = "bold",
        color = "#222222"
      )
    ) + theme(strip.text = ggplot2::element_text(family = subtitle_font,
                                                 size = 10)) + theme(
                                                   plot.subtitle = ggplot2::element_text(
                                                     family = subtitle_font,
                                                     size = 22,
                                                     margin = ggplot2::margin(7, 0, 9, 0)
                                                   )
                                                 ) + theme(
                                                   legend.text = ggplot2::element_text(
                                                     family = subtitle_font,
                                                     size = 18,
                                                     color = "#222222"
                                                   ),
                                                   axis.text = ggplot2::element_text(
                                                     family = mono_font,
                                                     size = 8,
                                                     color = "#222222"
                                                   )
                                                 )
  }

run_report <- function(start_year, end_year) {
  data <- load_data(start_year, end_year)
  # Run regression model on all years.
  nflWinModel <- build_regression_model(data)

  # Add field to each row of `data` with PredictedW
  data <-
    mutate(data, PredictedW = predict(nflWinModel, data[row_number(),]))
  plot_wins(data)
}


run_report(2002, 2019)
