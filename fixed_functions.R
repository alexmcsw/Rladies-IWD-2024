#' @title  **PWHL Rosters**
#' @description PWHL Rosters lookup
#'
#' @param season Season (YYYY) to pull the roster from, the concluding year in XXXX-YY format
#' @param team Team to pull the roster data for
#' @param regular Bool for whether to pull regular or pre-season rosters
#' @return A data frame with roster data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_team_roster(season = 2023, team = "Toronto"))
#' }

pwhl_team_roster_fix <- function(team, season = 2023, regular = TRUE) {

  # team_id <- 1 # will need the team/season look ups
  team_id <- fastRhockey::pwhl_teams() %>%
    dplyr::filter(.data$team_code == team)
  # season_id <- 2 # 1 is regular season, 2 is pre-season
  if (regular) {
    season_id <- 1
  } else if (! regular) {
    season_id <- 2
  }
  # base_url <- "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=1&season_id=2&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  full_url <- paste0(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=",
    team_id$team_id,
    "&season_id=",
    season_id,
    "&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  )

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")

  res <- gsub("angular.callbacks._h\\(", "", res)
  res <- gsub("}}]}]}]})", "}}]}]}]}", res)

  r <- res %>%
    jsonlite::parse_json()

  team_name <- r[[1]]
  team_logo <- r[[2]]
  roster_year <- r[[3]]
  league <- r[[4]]

  players <- r[[5]][[1]]$sections

  roster_data <- data.frame()
  staff_data <- data.frame()

  player_types <- c("Forwards", "Defenders", "Goalies")

  tryCatch(
    expr = {
      for (i in seq_along(players)) {

        # i = 1

        if (players[[i]]$title %in% player_types) {
          # print('yes')

          for (p in seq_along(players[[i]]$data)) {

            if (is.null(players[[i]]$data[[p]]$row$shoots)) {
              hand <- players[[i]]$data[[p]]$row$catches
            } else {
              hand <- players[[i]]$data[[p]]$row$shoots
            }

            "player_id" %in% names(players[[i]]$data[[p]]$row)

            suppressWarnings(
              player_info <- data.frame(
                "player_id" = c(if ("player_id" %in% names(players[[i]]$data[[p]]$row)) players[[i]]$data[[p]]$row$player_id else NA),
                "player_name" = c(if ("name" %in% names(players[[i]]$data[[p]]$row)) players[[i]]$data[[p]]$row$name else NA),
                "primary_hand" = c(hand),
                "dob" = c(if ("birthdate" %in% names(players[[i]]$data[[p]]$row)) players[[i]]$data[[p]]$row$birthdate else NA),
                "height" = c(if ("height_hyphenated" %in% names(players[[i]]$data[[p]]$row)) players[[i]]$data[[p]]$row$height_hyphenated else NA),
                "position" = c(if ("position" %in% names(players[[i]]$data[[p]]$row)) players[[i]]$data[[p]]$row$position else NA),
                "home_town" = c(if ("hometown" %in% names(players[[i]]$data[[p]]$row)) players[[i]]$data[[p]]$row$hometown else NA)
              ) %>%
                tidyr::separate(player_name, into = c("first_name", "last_name"), remove = FALSE, sep=" ")
            )

            # players[[i]]$data[[p]]$prop

            roster_data <- rbind(
              roster_data,
              player_info
            )

          }

        } else {
          next
        }

      }

      roster_data <- roster_data %>%
        dplyr::mutate(
          league = "pwhl",
          age = round(lubridate::time_length(as.Date(paste0(season, "-01-01")) - as.Date(.data$dob), "years")),
          player_headshot = paste0("https://assets.leaguestat.com/pwhl/240x240/", .data$player_id, ".jpg"),
          regular_season = ifelse(season_id == 1, TRUE, FALSE),
          season = season,
          player_id = as.numeric(player_id),
          team_id = as.numeric(team_id$team_id),
          team = team
        ) %>%
        dplyr::relocate("team_id", .after = player_id) %>%
        dplyr::relocate("season", .after = team_id)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(roster_data)

}

# one of the functions from the package is not behaving as expected.
# we'll fix it up here!

#' @title  **PWHL Stats**
#' @description PWHL Stats lookup
#'
#' @param position either goalie or skater. If skater, need to select a team.
#' @param season Season (YYYY) to pull the roster from, the concluding year in XXXX-YY format
#' @param team Team to pull the roster data for
#' @param regular Bool for whether to pull regular or pre-season rosters
#' @return A data frame with roster data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @import tidyverse
#' @export

pwhl_stats_fix <- function(position = "skater", team = "BOS", season = 2023) {
  team_id <- pwhl_teams() %>%
    dplyr::filter(.data$team_code == team)

  team_id <- team_id$team_id

  tryCatch(
    expr = {
        URL <- glue::glue("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season=1&team={team_id}&position=skaters&rookies=0&statsType=standard&rosterstatus=undefined&site_id=2&first=0&limit=20&sort=points&league_id=1&lang=en&division=-1&key=694cfeed58c932ee&client_code=pwhl&league_id=1&callback=angular.callbacks._6")

        res <- httr::RETRY(
          "GET",
          URL
        )

        res <- res %>%
          httr::content(as = "text", encoding = "utf-8")

        res <- gsub("angular.callbacks._6\\(", "", res)
        res <- gsub("}]}]}])", "}]}]}]", res)
        r <- res %>%
          jsonlite::parse_json()

        players <- data.frame()

        data = r[[1]]$sections[[1]]$data

        for (y in 1:length(data)) {

          player_df <- data.frame(
            player_id = c(data[[y]]$row$player_id),
            player = c(data[[y]]$row$name),
            current_team = c(data[[y]]$row$active),
            position = c(data[[y]]$row$position),
            team = c(data[[y]]$row$team_code),
            games_played = c(data[[y]]$row$games_played),
            goals = c(data[[y]]$row$goals),
            shots = c(data[[y]]$row$shots),
            shooting_pct = c(data[[y]]$row$shooting_percentage),
            assists = c(data[[y]]$row$assists),
            points = c(data[[y]]$row$points),
            points_per_game = (data[[y]]$row$points_per_game),
            plus_minus = c(data[[y]]$row$plus_minus),
            penalty_minutes = c(data[[y]]$row$penalty_minutes),
            penalty_minutes_per_game = c(data[[y]]$row$penalty_minutes_per_game),
            power_play_goals = c(data[[y]]$row$power_play_goals),
            power_play_assists = c(data[[y]]$row$power_play_assists),
            short_handed_goals = c(data[[y]]$row$short_handed_goals),
            short_handed_assists = c(data[[y]]$row$short_handed_assists),
            shootout_goals = c(data[[y]]$row$shootout_goals),
            shootout_attempts = c(data[[y]]$row$shootout_attempts),
            shootout_pct = c(data[[y]]$row$shootout_percentage),
            shootout_winning_goals = c(data[[y]]$row$shootout_winning_goals),
            faceoff_attempts = c(data[[y]]$row$faceoff_attempts),
            faceoff_wins = c(data[[y]]$row$faceoff_wins),
            faceoff_pct = c(data[[y]]$row$faceoff_pct)
          )

          players <- dplyr::bind_rows(players, player_df)
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(players)
}
