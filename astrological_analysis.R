# devtools::install_github(repo = "sportsdataverse/fastRhockey")

library(fastRhockey)
library(tidyverse)
library(DescTools)
library(janitor)
source("./Rladies-IWD-2024/fixed_functions.R")

colours <- list(
"BOS" = "#154734",
"MIN" = "#2E1A47",
"MON" = "#862633",
"NY" = "#00B2A9",
"OTT" = "#A6192E",
"TOR" = "#307FE2"
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

# let's double check we didn't grab anyone twice:

all_teams %>% get_dupes(contains("player_name"))

# let's look at where our signs are distributed

all_teams %>% group_by(sign, position.x) %>%
  summarize(n = n())

# let's look at the signs of our top scorers and top penalty-havers

# Visualizations

tail(all_teams[order(as.numeric(all_teams$goals)),], 10) %>%
  ggplot(aes(sign, fill = team)) +
  geom_bar() +
  scale_fill_manual("legend", values = colours) +
  ggtitle("Sign of top 10 scorers") +
  theme(
    plot.title = element_text(size=24)
  )
  
tail(all_teams[order(as.numeric(all_teams$penalty_minutes)),], 10) %>%
  ggplot(aes(sign, fill = team)) +
  geom_bar() + 
  scale_fill_manual(values = colours) +
  ggtitle("Sign of top 10 penalty-havers") +
  theme(
    plot.title = element_text(size=24)
  )

# let's look at totals by sign

all_teams %>% ggplot(aes(sign, fill = team)) +
  geom_bar() + 
  scale_fill_manual(values = colours) +
  ggtitle("Signs of the PWHL by team") +
  theme(
    plot.title = element_text(size=24)
  )

all_teams %>% ggplot(aes(sign, goals, fill = team)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colours) +
  ggtitle("Signs of the PWHL by team") +
  theme(
    plot.title = element_text(size=24)
  )



all_teams %>% group_by(team, sign) %>%
  summarize(goals = sum(as.numeric(goals))) %>%
  ggplot(aes(sign, goals, fill = team)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = colours) +
    ggtitle("Goals by sign") +
    theme(
      plot.title = element_text(size=24)
    )

# this could be skewed if there are many libras in the PWHL, for example.

all_teams %>% group_by(team, sign) %>%
  summarize(goals = sum(as.numeric(goals)), n = n()) %>%
  mutate(adjusted_goals = goals / n) %>%
  ggplot(aes(sign, adjusted_goals, fill = team)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = colours) +
    ggtitle("Goals by sign") +
    theme(
      plot.title = element_text(size=24)
    )


# sim for penalty min

all_teams %>% group_by(team, sign) %>%
  summarize(penalty_minutes = sum(as.numeric(penalty_minutes))) %>%
  ggplot(aes(sign, penalty_minutes, fill = team)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = colours) +
    ggtitle("Penalty minutes by sign") +
    theme(
      plot.title = element_text(size=24)
    )


all_teams %>% group_by(team, sign) %>%
  summarize(penalty_minutes = sum(as.numeric(penalty_minutes)), n = n()) %>%
  mutate(adjusted_penalty_minutes = penalty_minutes / n) %>%
  ggplot(aes(sign, adjusted_penalty_minutes, fill = team)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = colours) +
    ggtitle("Penalty min by sign") +
    theme(
      plot.title = element_text(size=24)
    )
