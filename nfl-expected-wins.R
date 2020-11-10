#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("mkuhn/dict")
#install.packages("tidyverse")
#install.packages("GGally")
library("tidyverse")
#library("GGally")
library(dict)
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


# For each year, build stats.
data <- data.frame()
for (i in seq(2002, 2006, by = 1)) {
  stats <- build_stats_for_year(i)
  data <- bind_rows(data, stats)
}
# Run regression model on all years.
nflWinModel <- build_regression_model(data)

#nflWinModel <- build_regression_model(build_stats_for_year(2003))

# Add field to each row of `data` with PredictedW
data <-
  mutate(data, PredictedW = predict(nflWinModel, data[row_number(),]))

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
  labs(title = "Predicted (grey) vs Actual Wins") +
  scale_colour_discrete(name  =
                          "Wins",
                        labels =
                          c("Predicted", "Actual")) +
  theme(legend.position =
          "none") + facet_wrap(~ Tm)
ggsave(plot = chart, filename = "wins.png")
