library(tidyverse)
library(rvest)

dvoa_url_for_year <- function(year) {
  str_interp("https://www.footballoutsiders.com/stats/nfl/team-efficiency/${year}")
}

dvoa_local_filename_for_year <- function(year) {
  str_interp("data/${year}/dvoa.html")
}

dvoa_download_file_for_year <- function(year) {
  url <- dvoa_url_for_year(year)
  local_filename <- dvoa_local_filename_for_year(year)

  if (!file.exists(local_filename)) {
    download.file(url, local_filename)
  }
}

dvoa_load_data_for_year <- function(year) {
  dvoa_download_file_for_year(year)
  filename <- dvoa_local_filename_for_year(year)
  file_contents <- read_html(filename)
  nodes <- html_nodes(file_contents, "table")[[1]]
  data <- html_table(nodes, fill = TRUE)

  # Rename column since source splits it across two lines
  colnames(data)[3] <- "TOTAL.DVOA"

  # Column numbers and names changed in 2020
  if (year >= 2020) {
    colnames(data)[8] <- "OFFENSEDVOA"
    colnames(data)[10] <- "DEFENSEDVOA"
    colnames(data)[12] <- "S.T.DVOA"
  }

  data <- data %>%
    select(-1) %>% # Drop unnecessary first column with numerical indexes
    select(TEAM, TOTAL.DVOA, OFFENSEDVOA, DEFENSEDVOA, S.T.DVOA) %>%
    mutate(TOTAL.DVOA=parse_number(TOTAL.DVOA),
           OFFENSEDVOA=parse_number(OFFENSEDVOA),
           DEFENSEDVOA=parse_number(DEFENSEDVOA),
           S.T.DVOA=parse_number(S.T.DVOA),
           Year = year)

  # TODO: Map short team ID to team name (SEA = "Seattle Seahawks")
  return(data)
}

dvoa_load_data <- function(start_year, end_year) {
  data <- data.frame()
  for (i in seq(start_year, end_year, by = 1)) {
    stats <- dvoa_load_data_for_year(i)
    data <- bind_rows(data, stats)
  }
  return(data)
}
