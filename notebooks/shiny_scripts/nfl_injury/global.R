library(tidyverse)
library(plotly)


injury_df <- read.csv("data/nfl_injury_cleanedv3.csv", header = TRUE)

#Team names
team_list <- sort(unique(injury_df$Team))

#Injury Types
injury_type_list <- c("Ankle","ACL", "Concussion", "Foot", "MCL", "Toe")

week_filter <- c("ConfChamp", "Division", "Offseason", "SuperBowl", "WildCard")
injury_week_df <- injury_df %>%
  filter(!(week_num %in% week_filter))

injury_week_df <- transform(injury_week_df, week_num = as.numeric(week_num))

#sets up the injury dataframe for graph
date_injury_sum_df <- injury_week_df %>%
  group_by(week_num) %>%
  summarize(Ankle=sum(ankle), ACL=sum(ACL), Concussion=sum(concussion), Foot=sum(foot), MCL=sum(MCL), Toe=sum(toe))

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


upper_lower_sum_df <- injury_week_df %>%
  group_by(week_num) %>%
  summarize(Upper=sum(upper), Lower=sum(lower))

ul_list <- c("week_num","Upper","Lower")
upper_lower_df <- upper_lower_sum_df[ul_list] %>%
  pivot_longer(cols = -week_num, names_to = "Type", values_to = "Number") %>%
  arrange(week_num)

#handles plot for correlation
corr_data <- date_sort %>%
  filter(week_num != 18)
