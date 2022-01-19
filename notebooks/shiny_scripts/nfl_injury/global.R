library(tidyverse)
library(plotly)


injury_df <- read.csv("data/nfl_injury_cleanedv2.csv", header = TRUE)


date_column_filter <- c("Team","details","Date","nfl_year","week_num")
date_na_df <- injury_df %>%
  filter(!is.na(details))

date_team_df <- date_na_df[date_column_filter]

date_count_df <- date_team_df %>%
  count(week_num)

date_count_df <- date_count_df %>% 
  rename("Injuries" = "n" )