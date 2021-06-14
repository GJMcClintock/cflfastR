#' @name cfl_games
#' @aliases cfl_games
#' @title
#' **CFL Games Endpoint Overview**
#' @description Get results, statistics and information for games\cr
#' \describe{
#'   \item{`cfl_game_player_stats()`:}{ Get results information from games.}
#'   \item{`cfl_game_team_stats()`:}{ Get team statistics by game.}
#'   \item{`cfl_game_info()`:}{ Get results information from games.}
#'   \item{`cfl_game_records()`:}{ Get team records by year.}
#'   \item{`cfl_calendar()`:}{ Get calendar of weeks by season.}
#' }
#' @details
#'
#' ### **Get game advanced box score information.**
#' ```r
#' cfbd_game_box_advanced(game_id = 401114233)
#' ```
#' ### **Get player statistics by game**
#' ```r
#' cfbd_game_player_stats(2018, week = 15, conference = "Ind")
#'
#' cfbd_game_player_stats(2013, week = 1, team = "Florida State", category = "passing")
#' ```
#' ### **Get team records by year**
#' ```r
#' cfbd_game_records(2018, team = "Notre Dame")
#'
#' cfbd_game_records(2013, team = "Florida State")
#' ```
#' ### **Get team statistics by game**
#' ```r
#' cfbd_game_team_stats(2019, team = "LSU")
#'
#' cfbd_game_team_stats(2013, team = "Florida State")
#' ```
#' ### **Get results information from games.**
#' ```r
#' cfbd_game_info(2018, week = 1)
#'
#' cfbd_game_info(2018, week = 7, conference = "Ind")
#'
#' # 7 OTs LSU @ TAMU
#' cfbd_game_info(2018, week = 13, team = "Texas A&M", quarter_scores = TRUE)
#' ```
#' ### **Get calendar of weeks by season.**
#' ```r
#' cfbd_calendar(2019)
#' ```
#' ### **Get game media information (TV, radio, etc).**
#' ```r
#' cfbd_game_media(2019, week = 4, conference = "ACC")
#' ```
#'
NULL

#' @title
#' **Get results information from games.**
#' @param year (\emph{Integer} required): Year, 4 digit format(\emph{YYYY})
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game - also requires a season
#'
#'
#'
#' @return [cfl_game_info()] - A data frame with 14 variables:
#' \describe{
#'   \item{`game_id`: integer.}{Referencing game id.}
#'   \item{`date_start`: character.}{Timestamp of kickoff.}
#'   \item{`game_number`: integer.}{The sequential league wide game number within the season.}
#'   \item{`week`: integer.}{The week of the season.}
#'   \item{`season`: integer.}{Season/Year.}
#'   \item{`attendance`: integer.}{Recorded number of fans at game.}
#'   \item{`event_type`: character.}{The season type: Pre-season, regular season, post-season.}
#'   \item{`venue`: character.}{Game location.}
#'   \item{`coin_toss`: character.}{Winner of the opening coin toss.}
#'   \item{`winner`: character.}{Winning team in the game.}
#'   \item{`home_team`: character.}{Home team.}
#'   \item{`away_team`: character.}{Away team.}
#'   \item{`home_score`: integer.}{Home points.}
#'   \item{`away_score`: integer.}{Away points.}
#' }
#' @source \url{http://api.cfl.ca/v1/games/}
#' @keywords Game Info
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET RETRY
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' \donttest{
#'   cfl_game_info(2019, 2552)

#' }
cfl_game_info <- function(season = NA,  week = NA, team = NA, event_type = NA, game_id = NA) {
  # Check if year is numeric
  assertthat::assert_that(is.numeric(season) & nchar(season) == 4,
     msg = "Enter valid season as a number (YYYY)"
  )

  if (!is.na(week)) {
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
      msg = "Enter valid week as number"
    )
  }
  if (!is.na(team)) {
    # Check if team is appropriate, if not NULL
    assertthat::assert_that(team %in% c("BC", "CGY", "EDM", "HAM", "MTL", "OTT", "SSK","TOR", "WPG"),
       msg = "Enter valid team abbreviation, use cfl_teams() to find abbreviations by year"
    )
  }
  if (!is.na(event_type)) {
    # Check if season type is appropriate, if not NULL
    assertthat::assert_that(event_type %in% c("Preseason", "Regular Season", "Playoffs", "Grey Cup", "Exhibition"),
                            msg = "Enter valid season_type: Preseason, Regular Season, Playoffs, Grey Cup, Exhibition"
    )
  }

  if (is.na(season)) {
    stop("A season year is required to find game_id data",call. = FALSE)
  } else if (!is.na(season) && is.na(game_id)) {
    url <- paste0('http://api.cfl.ca/v1', '/games/',
                  season)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/games/', season,
                  '/game/', game_id)
  }

  url <- build_url(url)
  games_call <- httr::GET(url)
  httr::stop_for_status(games_call)
  games_data_JSON <- httr::content(games_call)
  if(length(games_call) == 0) {
    games_data <- data.frame()
  } else {
    games_data <- dplyr::bind_rows(
      lapply(1:length(games_data_JSON$data),
             function(x) flatten_single_game(games_data_JSON$data[[x]])
      )
    )
  }

  if (!is.na(week)) {
    games_data <- filter(.data=games_data, games_data$week == !!week)
  }
  if (!is.na(team)) {
    games_data <- filter(.data=games_data, games_data$home_team == team | games_data$away_team == team)

  }
  if (!is.na(event_type)) {
    games_data <- filter(.data=games_data, games_data$event_type == !!event_type)
  }


  if (nrow(games_data) == 0) {
    # stop("No data found", call. = FALSE)
    NULL
  } else {
    games_data
  }
}

#' @title
#' **Get calendar of weeks by season.**
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param verbose Logical parameter (TRUE/FALSE, default: FALSE) to return warnings and messages from function
#' @return [cfl_calendar()] - A data frame with 5 variables:
#' \describe{
#'   \item{`season`: character.}{Calendar season.}
#'   \item{`week`: integer.}{Calendar game week.}
#'   \item{`season_type`: character}{Season type of calendar week.}
#'   \item{`first_game_start`: character.}{First game start time of the calendar week.}
#'   \item{`last_game_start`: character.}{Last game start time of the calendar week.}
#' }
#' @source \url{https://api.collegefootballdata.com/calendar}
#' @importFrom dplyr rename mutate
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   cfl_calendar(2019)
#' }

cfl_calendar <- function(year,
                          verbose = FALSE) {

  # check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
    msg = "Enter valid year as a number (YYYY)"
  )


  x <- data.frame()

  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      x <- cfl_game_info(year)
      df <- arrange(summarise(.data = group_by(.data = x, season, week, event_type), first_game_start = min(date_start), last_game_start = max(date_start)), first_game_start)
      if(verbose){
        message(glue::glue("{Sys.time()}: Scraping calendar..."))
      }
    },
    error = function(e) {
        message(glue::glue("{Sys.time()}:Invalid arguments or no calendar data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}

#' @title
#' **Get player statistics by game**
#' @param year (\emph{Integer} required): Year, 4 digit format(\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default regular): Select Season Type: regular or postseason
#' @param team (\emph{String} optional): D-I Team
#' @param category (\emph{String} optional): Category filter (e.g defensive)\cr
#' Offense: passing, receiving, rushing\cr
#' Defense: defensive, fumbles, interceptions\cr
#' Special Teams: punting, puntReturns, kicking, kickReturns\cr
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the [cfbd_game_info()] function
#' @param verbose Logical parameter (TRUE/FALSE, default: FALSE) to return warnings and messages from function
#'
#' @return [cfl_game_player_stats()] - A data frame with 32 variables:
#' \describe{
#'  \item{`game_id` : integer.}{UUID for game}
#'  \item{`season` : integer.}{Season game played}
#'  \item{`week` : integer.}{Week of Season}
#'  \item{`date_start` : character.}{Start date of kickoff}
#'  \item{`team` : character.}{Player team}
#'  \item{`cfl_central_id` : integer.}{UUID for player}
#'  \item{`first_name` : character.}{Player first name}
#'  \item{`middle_name` : character.}{Player middle name}
#'  \item{`last_name` : character.}{Player last name}
#'  \item{`birth_date` : character.}{Player DOB}
#'  \item{`pass_attempts` : integer.}{Pass Attempts}
#'  \item{`pass_completions` : integer.}{Pass Completios}
#'  \item{`pass_net_yards` : integer.}{Net Passing Yards}
#'  \item{`pass_long` : integer.}{Long Pass}
#'  \item{`pass_touchdowns` : integer.}{Passing TDs}
#'  \item{`pass_completion_percentage` : character.}{Completition Percentage}
#'  \item{`pass_efficiency` : character.}{Pass Efficiency Rating}
#'  \item{`pass_interceptions` : integer.}{Interceptions Thrown}
#'  \item{`pass_fumbles` : integer.}{Fumbles while passing}
#'  \item{`rush_attempts` : integer.}{Rush attempts}
#'  \item{`rush_net_yards` : integer.}{Net rushing yards}
#'  \item{`rush_long` : integer.}{Longest rush}
#'  \item{`rush_touchdowns` : integer.}{Rushing touchdowns}
#'  \item{`rush_long_touchdowns` : integer.}{Longest rushing TD}
#'  \item{`receive_attempts` : integer.}{Targets}
#'  \item{`receive_caught` : integer.}{Receptions}
#'  \item{`receive_yards` : integer.}{Receiving yards}
#'  \item{`receive_long` : integer.}{Longest reception}
#'  \item{`receive_touchdowns` : integer.}{Receiving TDs}
#'  \item{`receive_long_touchdowns` : integer.}{Long receiving TD}
#'  \item{`receive_yards_after_catch` : integer.}{Yards After Catch}
#'  \item{`receive_fumbles` : integer.}{Fumbles after reception}
#'  \item{`punts` : integer.}{Punts}
#'  \item{`punt_yards` : integer.}{Punt Yards}
#'  \item{`punt_net_yards` : integer.}{Net Punt Yards}
#'  \item{`punt_long` : integer.}{Longest Punt}
#'  \item{`punt_singles` : integer.}{Punt Singles}
#'  \item{`punts_blocked` : integer.}{Punts Blocked}
#'  \item{`punts_in_10` : integer.}{Punts downed inside 10}
#'  \item{`punts_in_20` : integer.}{Punts downed inside 20}
#'  \item{`punts_returned` : integer.}{Punts Returns Defended}
#'  \item{`punt_returns` : integer.}{Received Punts Returns}
#'  \item{`punt_returns_yards` : integer.}{Punt Return Yards}
#'  \item{`punt_returns_touchdowns` : integer.}{Punt Return TDs}
#'  \item{`punt_returns_long` : integer.}{Longest Punt Return}
#'  \item{`punt_returns_touchdowns_long` : integer.}{Longest Punt Return TD}
#'  \item{`kick_returns` : integer.}{Kick Returns}
#'  \item{`kick_returns_yards` : integer.}{Kick returns yards}
#'  \item{`kick_returns_touchdowns` : integer.}{KR Touchdowns}
#'  \item{`kick_returns_long` : integer.}{Longest kick return}
#'  \item{`kick_returns_touchdowns_long` : integer.}{Longest kick return TD}
#'  \item{`field_goal_attempts` : integer.}{Field goal attempts}
#'  \item{`field_goal_made` : integer.}{Field goals made}
#'  \item{`field_goal_yards` : integer.}{Attempt distances}
#'  \item{`field_goal_singles` : integer.}{Field goal singles}
#'  \item{`field_goal_long` : integer.}{Longest field goals}
#'  \item{`field_goal_missed_list` : character.}{List of missing distances}
#'  \item{`field_goal_points` : integer.}{Points from field goal attempts}
#'  \item{`kicks` : integer.}{Kickoffs}
#'  \item{`kick_yards` : integer.}{Kickoff yards}
#'  \item{`kicks_net_yards` : integer.}{Net kickoff yards}
#'  \item{`kicks_long` : integer.}{Longest kickoff}
#'  \item{`kicks_singles` : integer.}{Kickoff singles}
#'  \item{`kicks_out_of_end_zone` : integer.}{Kicks out of end zone}
#'  \item{`kicks_onside` : integer.}{Onside kicks}
#'  \item{`one_point_converts_attempts` : integer.}{XP attempts}
#'  \item{`one_point_converts_made` : integer.}{XP conversions}
#'  \item{`tackles_total` : integer.}{2 pt attempts}
#'  \item{`tackles_defensive` : integer.}{2 pt conversions}
#'  \item{`tackles_special_teams` : integer.}{Total tackles}
#'  \item{`sacks_qb_made` : integer.}{Defensive tackles}
#'  \item{`interceptions` : integer.}{Special teams tackles}
#'  \item{`fumbles_forced` : integer.}{Sacks made}
#'  \item{`fumbles_recovered` : integer.}{Interceptions forced}
#'  \item{`passes_knocked_down` : integer.}{Fumbles forced}
#'  \item{`fumbles_recovered` : integer.}{Fumble recoveries}
#'  \item{`passes_knocked_down` : integer.}{Batted passes}
#' }
#' @source \url{http://api.cfl.ca/v1/games/?include=playbyplay}
#' @keywords Game Info
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET RETRY
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#' \donttest{
#'   cfl_game_player_stats(year = 2020, week = 15, team = "BC")
#'
#' }

cfl_game_player_stats <- function(season = NA, game_id = NA, week = NA, team = NA, event_type = NA) {
  assertthat::assert_that(is.numeric(season) & nchar(season) == 4,
                          msg = "Enter valid season as a number (YYYY)"
  )

  if (!is.na(week)) {
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                            msg = "Enter valid week as number"
    )
  }
  if (!is.na(team)) {
    # Check if team is appropriate, if not NULL
    assertthat::assert_that(team %in% c("BC", "CGY", "EDM", "HAM", "MTL", "OTT", "SSK","TOR", "WPG"),
                            msg = "Enter valid team abbreviation: BC, CGY, EDM, HAM, MTL, OTT, SSK, TOR, WPG"
    )
  }
  if (!is.na(event_type)) {
    # Check if season type is appropriate, if not NULL
    assertthat::assert_that(event_type %in% c("Preseason", "Regular Season", "Playoffs", "Grey Cup", "Exhibition"),
                            msg = "Enter valid season_type: Preseason, Regular Season, Playoffs, Grey Cup, Exhibition"
    )
  }

  if(!is.na(game_id)) {
    ids <- c(game_id)
  } else {
    fetch_games <- cfl_game_info(season = season, week = week, team = team, event_type = event_type)
  }
  player_game_stats <- purrr::map2_dfr(fetch_games$game_id, fetch_games$season, game_to_player_box)
  return(player_game_stats)
}




#' @title
#' **Get team records by year**
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): Team - Select a valid team, D1 football
#' @param conference (\emph{String} optional): DI Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC
#' @param verbose Logical parameter (TRUE/FALSE, default: FALSE) to return warnings and messages from function
#' @return [cfl_game_records()] - A data frame with 20 variables:
#' \describe{
#'  \item{`division_id` : integer.}{Division ID}
#'  \item{`division_name` : character.}{Division Name}
#'  \item{`place` : integer.}{Place in Division}
#'  \item{`flags` : character.}{Flags}
#'  \item{`team_id` : integer.}{Team Id}
#'  \item{`letter` : character.}{First letter of team abbreviation}
#'  \item{`abbreviation` : character.}{Team abbreviation}
#'  \item{`location` : character.}{Team location}
#'  \item{`nickname` : character.}{Team Mascot}
#'  \item{`full_name` : character.}{Full name (eg: BC Lions)}
#'  \item{`games_played` : integer.}{Games Played}
#'  \item{`wins` : integer.}{Wins}
#'  \item{`losses` : integer.}{Losses}
#'  \item{`ties` : integer.}{Ties}
#'  \item{`points` : integer.}{Points in Standings (2pts for W, 1pt for T)}
#'  \item{`winning_percentage` : integer.}{Win %: Pts / (Games Played * 2)}
#'  \item{`points_for` : integer.}{Points Scored}
#'  \item{`points_against` : integer.}{Points Scored Against}
#'  \item{`home_wins` : integer.}{Home Wins}
#'  \item{`home_losses` : integer.}{Home Losses}
#'  \item{`home_ties` : integer.}{Home Ties}
#'  \item{`away_wins` : integer.}{Road wins}
#'  \item{`away_losses` : integer.}{Road losses}
#'  \item{`away_ties` : integer.}{Road ties}
#'  \item{`division_wins` : integer.}{Division Wins}
#'  \item{`division_losses` : integer.}{Division Losses}
#'  \item{`division_ties` : integer.}{Division Ties}
#'  \item{`season` : integer.}{Season}
#' }
#' @source \url{https://api.cfl.ca/v1/standings/}
#' @keywords Team Info
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET RETRY
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#' \donttest{
#'   cfl_game_records(2018, team = "WPG")
#'
#' }

cfl_game_records <- function(season = NA, team = NA) {
  # Check if year is numeric
  assertthat::assert_that(is.numeric(season) & nchar(season) == 4,
                          msg = "Enter valid season as a number (YYYY)"
  )

  if (!is.na(team)) {
    # Check if team is appropriate, if not NULL
    assertthat::assert_that(team %in% c("BC", "CGY", "EDM", "HAM", "MTL", "OTT", "SSK","TOR", "WPG"),
                            msg = "Enter valid team abbreviation, use cfl_teams() to find abbreviations by year"
    )
  }
  url <- paste0('http://api.cfl.ca/v1', '/standings/',
                season)
  url <- build_url(url)
  standings <- fromJSON(url,  flatten = TRUE)$data
  standings <- flatten_dfr(map(standings, ~map(.x, ~.x$standings)))
  if (!is.na(team)) {
    standings <- filter(.data=standings, standings$abbreviation == team)
  }
  return(standings)
}



#' @title
#' **Get team statistics by game**
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param season_type (\emph{String} default: regular): Select Season Type - regular, postseason, or both
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game\cr
#' Can be found using the [cfbd_game_info()] function
#' @param rows_per_team (\emph{Integer} default 1): Both Teams for each game on one or two row(s), Options: 1 or 2
#' @param verbose Logical parameter (TRUE/FALSE, default: FALSE) to return warnings and messages from function
#'
#' @return [cfl_game_team_stats()] - A data frame with 78 variables:
#' \describe{
#'  \item{`game_id` : integer.}{UUID of game}
#'  \item{`date_start` : character.}{Date of kickoff}
#'  \item{`week` : integer.}{Week of season}
#'  \item{`season` : integer.}{Season}
#'  \item{`event_type` : character.}{Event Type (Preseason, Regular Season, Playoffs, Grey Cup)}
#'  \item{`venue` : character.}{Venue}
#'  \item{`team` : character.}{Team}
#'  \item{`home_away` : character.}{Home/Away flag}
#'  \item{`points_for` : integer.}{Points Scored}
#'  \item{`points_against` : integer.}{Points Against}
#'  \item{`time_of_pos` : character.}{Time of Posession}
#'  \item{`fumbles` : integer.}{Fumbles Lost}
#'  \item{`int` : integer.}{Interceptions Thrown}
#'  \item{`downs` : integer.}{Turnovers on Downs}
#'  \item{`pass_attempts` : integer.}{Pass Attempts}
#'  \item{`pass_completions` : integer.}{Pass Completions}
#'  \item{`pass_net_yards` : integer.}{Net Passing Yards}
#'  \item{`pass_long` : integer.}{Longest pass}
#'  \item{`pass_touchdowns` : integer.}{Passing TDs}
#'  \item{`completion_percentage` : character.}{Team completion %}
#'  \item{`pass_efficiency` : character.}{Team passer rating}
#'  \item{`pass_interceptions` : integer.}{Interceptions Thrown}
#'  \item{`pass_fumbles` : integer.}{Fumbles lost while passing}
#'  \item{`rush_attempts` : integer.}{Rush Attempts}
#'  \item{`rush_net_yards` : integer.}{Net rushing yards}
#'  \item{`rush_long` : integer.}{Longest rush}
#'  \item{`rush_touchdowns` : integer.}{Rushing TDs}
#'  \item{`rush_long_touchdowns` : integer.}{Longest rushing TD}
#'  \item{`rec_attempts` : integer.}{Targets}
#'  \item{`rec_caught` : integer.}{Receptions}
#'  \item{`rec_yards` : integer.}{Receiving yards}
#'  \item{`rec_long` : integer.}{Longest reception}
#'  \item{`rec_touchdowns` : integer.}{Receiving TDs}
#'  \item{`rec_long_touchdowns` : integer.}{Longest TD Reception}
#'  \item{`rec_yards_after_catch` : integer.}{Yards after Catch}
#'  \item{`rec_fumbles` : integer.}{Fumbles after reception}
#'  \item{`punts` : integer.}{Punts}
#'  \item{`punt_yards` : integer.}{Punt Yards}
#'  \item{`punt_net_yards` : integer.}{Net Punt Yards}
#'  \item{`punt_long` : integer.}{Longest Punt}
#'  \item{`punt_singles` : integer.}{Punt Singles}
#'  \item{`punts_blocked` : integer.}{Punts Blocked}
#'  \item{`punts_in_10` : integer.}{Punts downed inside 10}
#'  \item{`punts_in_20` : integer.}{Punts downed inside 20}
#'  \item{`punts_returned` : integer.}{Punts Returns Defended}
#'  \item{`punt_returns` : integer.}{Received Punts Returns}
#'  \item{`punt_returns_yards` : integer.}{Punt Return Yards}
#'  \item{`punt_returns_touchdowns` : integer.}{Punt Return TDs}
#'  \item{`punt_returns_long` : integer.}{Longest Punt Return}
#'  \item{`punt_returns_touchdowns_long` : integer.}{Longest Punt Return TD}
#'  \item{`kick_returns` : integer.}{Kick Returns}
#'  \item{`kick_returns_yards` : integer.}{Kick returns yards}
#'  \item{`kick_returns_touchdowns` : integer.}{KR Touchdowns}
#'  \item{`kick_returns_long` : integer.}{Longest kick return}
#'  \item{`kick_returns_touchdowns_long` : integer.}{Longest kick return TD}
#'  \item{`field_goal_attempts` : integer.}{Field goal attempts}
#'  \item{`field_goal_made` : integer.}{Field goals made}
#'  \item{`field_goal_yards` : integer.}{Attempt distances}
#'  \item{`field_goal_singles` : integer.}{Field goal singles}
#'  \item{`field_goal_long` : integer.}{Longest field goals}
#'  \item{`field_goal_points` : integer.}{Points from field goal attempts}
#'  \item{`kicks` : integer.}{Kickoffs}
#'  \item{`kick_yards` : integer.}{Kickoff yards}
#'  \item{`kicks_net_yards` : integer.}{Net kickoff yards}
#'  \item{`kicks_long` : integer.}{Longest kickoff}
#'  \item{`kicks_singles` : integer.}{Kickoff singles}
#'  \item{`kicks_out_of_end_zone` : integer.}{Kicks out of end zone}
#'  \item{`kicks_onside` : integer.}{Onside kicks}
#'  \item{`one_point_convert_attempts` : integer.}{XP attempts}
#'  \item{`one_point_convert_made` : integer.}{XP conversions}
#'  \item{`two_point_convert_attempts` : integer.}{2 pt attempts}
#'  \item{`two_point_convert_made` : integer.}{2 pt conversions}
#'  \item{`tackles_total` : integer.}{Total tackles}
#'  \item{`tackles_defensive` : integer.}{Defensive tackles}
#'  \item{`tackles_special_teams` : integer.}{Special teams tackles}
#'  \item{`sacks_qb_made` : integer.}{Sacks made}
#'  \item{`interceptions` : integer.}{Interceptions forced}
#'  \item{`fumbles_forced` : integer.}{Fumbles forced}
#'  \item{`fumbles_recovered` : integer.}{Fumble recoveries}
#'  \item{`passes_knocked_down` : integer.}{Batted passes}
#'  \item{`defensive_touchdowns` : integer.}{Defensive TDs}
#'  \item{`defensive_safeties` : integer.}{Safeties forced}
#'  \item{`pass_attempts_allowed` : integer.}{Pass Attempts Allowed}
#'  \item{`pass_completions_allowed` : integer.}{Pass Completions Allowed}
#'  \item{`pass_net_yards_allowed` : integer.}{Net Passing Yards Allowed}
#'  \item{`pass_long_allowed` : integer.}{Longest pass Allowed}
#'  \item{`pass_touchdowns_allowed` : integer.}{Passing TDs Allowed}
#'  \item{`completion_percentage_allowed` : character.}{Team completion % Allowed}
#'  \item{`pass_efficiency_allowed` : character.}{Team passer rating Allowed}
#'  \item{`pass_interceptions_allowed` : integer.}{Interceptions Thrown Allowed}
#'  \item{`pass_fumbles_forced` : integer.}{Fumbles forced while opponent passing}
#'  \item{`rush_attempts_allowed` : integer.}{Rush Attempts Allowed}
#'  \item{`rush_net_yards_allowed` : integer.}{Net rushing yards Allowed}
#'  \item{`rush_long_allowed` : integer.}{Longest rush Allowed}
#'  \item{`rush_touchdowns_allowed` : integer.}{Rushing TDs Allowed}
#'  \item{`rush_long_touchdowns_allowed` : integer.}{Longest rushing TD Allowed}
#'  \item{`rec_attempts_allowed` : integer.}{Targets Allowed}
#'  \item{`rec_caught_allowed` : integer.}{Receptions Allowed}
#'  \item{`rec_yards_allowed` : integer.}{Receiving yards Allowed}
#'  \item{`rec_long_allowed` : integer.}{Longest reception Allowed}
#'  \item{`rec_touchdowns_allowed` : integer.}{Receiving TDs Allowed}
#'  \item{`rec_long_touchdowns_allowed` : integer.}{Longest TD Reception Allowed}
#'  \item{`rec_yards_after_catch_allowed` : integer.}{Yards after Catch Allowed}
#'  \item{`rec_fumbles_forced` : integer.}{Fumbles after reception Forced}
#'  \item{`total_penalties` : integer.}{Total penalties}
#'  \item{`total_penalty_yards` : integer.}{Penalty Yards}
#'  \item{`offence_penalties` : integer.}{Offensive penalties}
#'  \item{`offence_penalty_yards` : integer.}{Offensive penalty yards}
#'  \item{`defence_penalties` : integer.}{Defensive penalties}
#'  \item{`defence_penalty_yards` : integer.}{Defensive penalty yards}
#'  \item{`special_teams_coverage_penalties` : integer.}{Special Teams Coverage Penalties}
#'  \item{`special_teams_coverage_penalty_yards` : integer.}{ST Coverage penalty yards}
#'  \item{`special_teams_retrurn_penalties` : integer.}{ST Return Penalties}
#'  \item{`special_teams_return_penalty_yards` : integer.}{ST Return Penalty yards}
#' }
#' @source \url{https://api.collegefootballdata.com/games/teams}
#' @keywords Team Game Stats
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET RETRY
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom purrr map2_dfr
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#' \donttest{
#'   cfl_game_team_stats(season = 2019, team = "EDM", week = 1)
#'
#' }
#'
#'

cfl_game_team_stats <- function(season = NA, game_id = NA, week = NA, team = NA, event_type = NA) {
  assertthat::assert_that(is.numeric(season) & nchar(season) == 4,
                          msg = "Enter valid season as a number (YYYY)"
  )

  if (!is.na(week)) {
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                            msg = "Enter valid week as number"
    )
  }
  if (!is.na(team)) {
    # Check if team is appropriate, if not NULL
    assertthat::assert_that(team %in% c("BC", "CGY", "EDM", "HAM", "MTL", "OTT", "SSK","TOR", "WPG"),
                            msg = "Enter valid team abbreviation: BC, CGY, EDM, HAM, MTL, OTT, SSK, TOR, WPG"
    )
  }
  if (!is.na(event_type)) {
    # Check if season type is appropriate, if not NULL
    assertthat::assert_that(event_type %in% c("Preseason", "Regular Season", "Playoffs", "Grey Cup", "Exhibition"),
                            msg = "Enter valid season_type: Preseason, Regular Season, Playoffs, Grey Cup, Exhibition"
    )
  }

  if(!is.na(game_id)) {
    ids <- c(game_id)
  } else {
    fetch_games <- cfl_game_info(season = season, week = week, team = team, event_type = event_type)
  }

  hold_df <- purrr::map2_dfr(fetch_games$game_id, fetch_games$season, game_to_box)
  boxscores <- flatten_boxscore(hold_df)

    if (nrow(boxscores) == 0) {
      stop("No data found", call. = FALSE)
      NULL
    } else {
      boxscores
    }
}
