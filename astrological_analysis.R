devtools::install_github(repo = "sportsdataverse/fastRhockey")
library(fastRhockey)
library(tidyverse)
library(DescTools)
source("./fixed_functions.R")

colours <- list(
"BOS" = c("#154734", "#A2AAAD"),
"MIN" = c("#2E1A47"),
"MON" = c("#862633", "#333F48", "#DDCBA4"),
"NY" = c("#A2AAAD", "#00B2A9", "#0C2340"),
"OTT" = c("#333F48", "#A6192E"),
"TOR" = c("#307FE2")
)



# create a nice function to lapply to our team vector

team_stats <- function() {
 
  # get vector of team abbreviations
  teams <- fastRhockey::pwhl_teams()$team_code

  all_teams <- data.frame()

  for (team in teams){

  # here we use our modified functions
  df_stats <- pwhl_stats_fix(position = "skater", team = team, season = 2023)

  df_team <- pwhl_team_roster_fix(
    team = team,
    season = 2023
  ) %>%
      mutate(
        sign = DescTools::Zodiac(as.Date(dob))
      ) %>%
      merge(
        df_stats,
        by.x = c("player_id","player_name", "team"),
        by.y = c("player_id", "player","team")
      )
    
    all_teams <- rbind(all_teams, df_team)
  }

  return(all_teams)

}


all_teams <- team_stats()
