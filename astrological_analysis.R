devtools::install_github(repo = "sportsdataverse/fastRhockey")
library(fastRhockey)
library(tidyverse)
library(DescTools)

df_stats <- pwhl_stats(position = "skater", team = "OTT")

df_team <- fastRhockey::pwhl_team_roster(
    team = "Ottawa",
    season = 2023
) %>%
    mutate(
        sign = DescTools::Zodiac(as.Date(dob))
    ) %>%
    merge(
        df_stats,
        by.x = c("player_id","player_name"),
        by.y = c("player_id", "player"),
        all = TRUE
    )


