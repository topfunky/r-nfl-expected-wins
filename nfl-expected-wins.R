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

  # merge() on Tm so wins and off/def stats are in a single frame
  data <-
    merge(nflTeams, offense) %>% merge(defense) %>% mutate(Year = year)

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

plot_wins <- function(data, start_year, end_year) {
  # Chart ActualWins and PredictedWins
  chart <-
    ggplot(data = data, aes(x = Year, y =
                              W)) +
    # Reference line: 8 wins
    geom_hline(yintercept = 8, color = "#d8d8d8") +
    geom_vline(
      xintercept = 2020,
      color = "#00cc00",
      size = 2,
      alpha = 0.1
    ) +
    # Predicted
    geom_line(aes(x = Year, y = PredictedW),
              color = "#cc0000",
              alpha = 0.2) +
    # Pythagorean
    geom_line(aes(x = Year, y = PythagoreanW),
              color = "#0000cc",
              alpha = 0.2) +
    # Actual
    geom_line() + geom_point() +
    # Styling
    scale_y_continuous(breaks = seq(0, 16, by = 4)) +
    theme_minimal()  + style_fonts("Sentinel", "Avenir", "InputSans") +
    labs(
      title = str_interp("NFL Predicted vs Actual Wins, ${start_year}-${end_year}"),
      subtitle = "Predicted wins from an efficiency metrics model (red) and Pythagorean wins (blue)",
      y = "Wins",
      caption = "Based on data from pro-football-reference.com"
    ) +
    facet_wrap(~ Tm)

  if (!dir.exists("out")) {
    dir.create("out")
  }

  ggsave(
    plot = chart,
    filename = "out/wins.png",
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
      ),
      strip.text = ggplot2::element_text(family = subtitle_font,
                                         size = 10),
      plot.subtitle = ggplot2::element_text(
        family = subtitle_font,
        size = 22,
        margin = ggplot2::margin(7, 0, 9, 0)
      ),
      plot.caption = ggplot2::element_text(family = subtitle_font),
      axis.title = ggplot2::element_text(family = subtitle_font,
                                         size = 18),
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

# Traditional Bill James method for calculating expected wins.
# https://en.wikipedia.org/wiki/Pythagorean_expectation
calculate_pythagorean_wins <- function(PF, PA) {
  (1 / (1 + (PA / PF) ^ 2)) * 16
}

run_report <- function() {
  training_years <- c(2015, 2019)
  all_years <- c(2002, 2020)

  training_data <- load_data(training_years[1], training_years[2])
  # Run regression model on training years
  nflWinModel <- build_regression_model(training_data)

  # Load other years for prediction and display
  data <- load_data(all_years[1], all_years[2])

  # Add field to each row of `data` with PredictedW
  data <-
    mutate(data,
           PredictedW = predict(nflWinModel, data[row_number(), ]),
           PythagoreanW = calculate_pythagorean_wins(PF, PA))

  plot_wins(data, all_years[1], all_years[2])
}

run_report()
