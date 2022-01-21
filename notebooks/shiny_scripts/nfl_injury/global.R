library(tidyverse)
library(plotly)


injury_df <- read.csv("data/nfl_injury_cleanedv3.csv", header = TRUE)

#Team names
team_list <- sort(unique(injury_df$Team))

#removes rows that have na for the details
injury_na_df <- injury_df %>%
  filter(!is.na(details))

date_column_filter <- c("Team","details","Date","nfl_year","week_num")

date_team_df <- injury_na_df[date_column_filter] %>%
  count(week_num) %>%
  rename("Injuries" = "n")


#Handles data for injuries by date
date_column_filter <- c("Team","details","Date","nfl_year","week_num")
week_filter <- c("ConfChamp", "Division", "Offseason", "SuperBowl", "WildCard")

week_df <- injury_na_df[date_column_filter] %>%
  filter(!(week_num %in% week_filter))

week_inj_count_df <- week_df %>%
  count(week_num) %>%
  rename("Injuries" = "n")

date_sortv2 <- transform(week_inj_count_df, week_num = as.numeric(week_num))

date_sort <- date_sortv2 %>%
  arrange(week_num)
