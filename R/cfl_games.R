#' @name cfl_games
#' @aliases cfl_games
#' @title
#' **CFBD Games Endpoint Overview**
#' @description Get results, statistics and information for games\cr
#' \describe{
#'   \item{`cfbd_game_box_advanced()`:}{ Get game advanced box score information.}
#'   \item{`cfbd_game_player_stats()`:}{ Get results information from games.}
#'   \item{`cfl_game_team_stats()`:}{ Get team statistics by game.}
#'   \item{`cfl_game_info()`:}{ Get results information from games.}
#'   \item{`cfbd_game_records()`:}{ Get team records by year.}
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
cfl_game_info <- function(season = NA, game_id = NA) {
  if (is.na(season)) {
    stop("A season year is required to find game_id data", call. = FALSE)
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
    games_data <- dplyr::tbl_df()
  } else {
    games_data <- dplyr::bind_rows(
      lapply(1:length(games_data_JSON$data),
             function(x) flatten_single_game(games_data_JSON$data[[x]])
      )
    )
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
#'   cfbd_calendar(2019)
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
#' **Get game advanced box score information.**
#' @param game_id (\emph{Integer} required): Game ID filter for querying a single game
#' Can be found using the [cfbd_game_info()] function
#' @param long (\emph{Logical} default `FALSE`): Return the data in a long format.
#' @param verbose Logical parameter (TRUE/FALSE, default: FALSE) to return warnings and messages from function
#' @return [cfbd_game_box_advanced()] - A data frame with 2 rows and 69 variables:
#' \describe{
#'   \item{`team`: character.}{Team name.}
#'   \item{`plays`: double.}{Number of plays.}
#'   \item{`ppa_overall_total`: double.}{Predicted points added (PPA) overall total.}
#'   \item{`ppa_overall_quarter1`: double.}{Predicted points added (PPA) overall Q1.}
#'   \item{`ppa_overall_quarter2`: double.}{Predicted points added (PPA) overall Q2.}
#'   \item{`ppa_overall_quarter3`: double.}{Predicted points added (PPA) overall Q3.}
#'   \item{`ppa_overall_quarter4`: double.}{Predicted points added (PPA) overall Q4.}
#'   \item{`ppa_passing_total`: double.}{Passing predicted points added (PPA) total.}
#'   \item{`ppa_passing_quarter1`: double.}{Passing predicted points added (PPA) Q1.}
#'   \item{`ppa_passing_quarter2`: double.}{Passing predicted points added (PPA) Q2.}
#'   \item{`ppa_passing_quarter3`: double.}{Passing predicted points added (PPA) Q3.}
#'   \item{`ppa_passing_quarter4`: double.}{Passing predicted points added (PPA) Q4.}
#'   \item{`ppa_rushing_total`: double.}{Rushing predicted points added (PPA) total.}
#'   \item{`ppa_rushing_quarter1`: double.}{Rushing predicted points added (PPA) Q1.}
#'   \item{`ppa_rushing_quarter2`: double.}{Rushing predicted points added (PPA) Q2.}
#'   \item{`ppa_rushing_quarter3`: double.}{Rushing predicted points added (PPA) Q3.}
#'   \item{`ppa_rushing_quarter4`: double.}{Rushing predicted points added (PPA) Q4.}
#'   \item{`cumulative_ppa_plays`: double.}{Cumulative predicted points added (PPA) added total.}
#'   \item{`cumulative_ppa_overall_total`: double.}{Cumulative predicted points added (PPA) total.}
#'   \item{`cumulative_ppa_overall_quarter1`: double.}{Cumulative predicted points added (PPA) Q1.}
#'   \item{`cumulative_ppa_overall_quarter2`: double.}{Cumulative predicted points added (PPA) Q2.}
#'   \item{`cumulative_ppa_overall_quarter3`: double.}{Cumulative predicted points added (PPA) Q3.}
#'   \item{`cumulative_ppa_overall_quarter4`: double.}{Cumulative predicted points added (PPA) Q4.}
#'   \item{`cumulative_ppa_passing_total`: double.}{Cumulative passing predicted points added (PPA) total.}
#'   \item{`cumulative_ppa_passing_quarter1`: double.}{Cumulative passing predicted points added (PPA) Q1.}
#'   \item{`cumulative_ppa_passing_quarter2`: double.}{Cumulative passing predicted points added (PPA) Q2.}
#'   \item{`cumulative_ppa_passing_quarter3`: double.}{Cumulative passing predicted points added (PPA) Q3.}
#'   \item{`cumulative_ppa_passing_quarter4`: double.}{Cumulative passing predicted points added (PPA) Q4.}
#'   \item{`cumulative_ppa_rushing_total`: double.}{Cumulative rushing predicted points added (PPA) total.}
#'   \item{`cumulative_ppa_rushing_quarter1`: double.}{Cumulative rushing predicted points added (PPA) Q1.}
#'   \item{`cumulative_ppa_rushing_quarter2`: double.}{Cumulative rushing predicted points added (PPA) Q2.}
#'   \item{`cumulative_ppa_rushing_quarter3`: double.}{Cumulative rushing predicted points added (PPA) Q3.}
#'   \item{`cumulative_ppa_rushing_quarter4`: double.}{Cumulative rushing predicted points added (PPA) Q4.}
#'   \item{`success_rates_overall_total`: double.}{Success rates overall total.}
#'   \item{`success_rates_overall_quarter1`: double.}{Success rates overall Q1.}
#'   \item{`success_rates_overall_quarter2`: double.}{Success rates overall Q2.}
#'   \item{`success_rates_overall_quarter3`: double.}{Success rates overall Q3.}
#'   \item{`success_rates_overall_quarter4`: double.}{Success rates overall Q4.}
#'   \item{`success_rates_standard_downs_total`: double.}{Success rates standard downs total.}
#'   \item{`success_rates_standard_downs_quarter1`: double.}{Success rates standard downs Q1.}
#'   \item{`success_rates_standard_downs_quarter2`: double.}{Success rates standard downs Q2.}
#'   \item{`success_rates_standard_downs_quarter3`: double.}{Success rates standard downs Q3.}
#'   \item{`success_rates_standard_downs_quarter4`: double.}{Success rates standard downs Q4.}
#'   \item{`success_rates_passing_downs_total`: double.}{Success rates passing downs total.}
#'   \item{`success_rates_passing_downs_quarter1`: double.}{Success rates passing downs Q1.}
#'   \item{`success_rates_passing_downs_quarter2`: double.}{Success rates passing downs Q2.}
#'   \item{`success_rates_passing_downs_quarter3`: double.}{Success rates passing downs Q3.}
#'   \item{`success_rates_passing_downs_quarter4`: double.}{Success rates passing downs Q4.}
#'   \item{`explosiveness_overall_total`: double.}{Explosiveness rates overall total.}
#'   \item{`explosiveness_overall_quarter1`: double.}{Explosiveness rates overall Q1.}
#'   \item{`explosiveness_overall_quarter2`: double.}{Explosiveness rates overall Q2.}
#'   \item{`explosiveness_overall_quarter3`: double.}{Explosiveness rates overall Q3.}
#'   \item{`explosiveness_overall_quarter4`: double.}{Explosiveness rates overall Q4.}
#'   \item{`rushing_power_success`: double.}{Rushing power success rate.}
#'   \item{`rushing_stuff_rate`: double.}{Rushing stuff rate.}
#'   \item{`rushing_line_yds`: double.}{Rushing offensive line yards.}
#'   \item{`rushing_line_yds_avg`: double.}{Rushing line yards average.}
#'   \item{`rushing_second_lvl_yds`: double.}{Rushing second-level yards.}
#'   \item{`rushing_second_lvl_yds_avg`: double.}{Average second level yards per rush.}
#'   \item{`rushing_open_field_yds`: double.}{Rushing open field yards.}
#'   \item{`rushing_open_field_yds_avg`: double.}{Average rushing open field yards average.}
#'   \item{`havoc_total`: double.}{Total havoc rate.}
#'   \item{`havoc_front_seven`: double.}{Front-7 players havoc rate.}
#'   \item{`havoc_db`: double.}{Defensive back players havoc rate.}
#'   \item{`scoring_opps_opportunities`: double.}{Number of scoring opportunities.}
#'   \item{`scoring_opps_points`: double.}{Points on scoring opportunity drives.}
#'   \item{`scoring_opps_pts_per_opp`: double.}{Points per scoring opportunity drives.}
#'   \item{`field_pos_avg_start`: double.}{Average starting field position.}
#'   \item{`field_pos_avg_starting_predicted_pts`: double.}{Average starting predicted points (PP) for the average starting field position.}
#' }
#' @source \url{https://api.collegefootballdata.com/game/box/advanced}
#' @keywords Game Advanced Box Score
#' @importFrom tibble enframe
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET RETRY
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom stringr str_detect
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#' \donttest{
#'  cfbd_game_box_advanced(game_id = 401114233)
#' }
#'

cfbd_game_box_advanced <- function(game_id, long = FALSE,
                                   verbose = FALSE) {
  if (!is.null(game_id)) {
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
      msg = "Enter valid game_id (numeric value)"
    )
  }

  base_url <- "https://api.collegefootballdata.com/game/box/advanced?"

  full_url <- paste0(
    base_url,
    "gameId=", game_id
  )

  # Check for CFBD API key
  if (!has_cfbd_key()) stop("CollegeFootballData.com now requires an API key.", "\n       See ?register_cfbd for details.", call. = FALSE)

  # Create the GET request and set response as res
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(Authorization = paste("Bearer", cfbd_key()))
  )

  # Check the result
  check_status(res)

  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content, tidyr::unnest, and return result as data.frame
      df <- res %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        furrr::future_map_if(is.data.frame, list) %>%
        furrr::future_map_if(is.data.frame, list)

      df <- tibble::enframe(unlist(df$teams, use.names = TRUE))
      team1 <- seq(1, nrow(df) - 1, by = 2)
      df1 <- df[team1, ] %>%
        dplyr::rename(
          stat = .data$name,
          team1 = .data$value
        )

      team2 <- seq(2, nrow(df), by = 2)
      df2 <- df[team2, ] %>%
        dplyr::rename(team2 = .data$value) %>%
        dplyr::select(.data$team2)

      df <- data.frame(cbind(df1, df2))
      df$stat <- substr(df$stat, 1, nchar(df$stat) - 1)
      df$stat <- sub(".overall.", "_overall_", df$stat)
      df$stat <- sub("Downs.", "_downs_", df$stat)
      df$stat <- sub("Rates.", "_rates_", df$stat)
      df$stat <- sub("Rate", "_rate", df$stat)
      df$stat <- sub(".passing.", "_passing_", df$stat)
      df$stat <- sub(".rushing.", "_rushing_", df$stat)
      df$stat <- sub("rushing.", "rushing_", df$stat)
      df$stat <- sub("rushing.", "rushing_", df$stat)
      df$stat <- sub("fieldPosition.", "field_pos_", df$stat)
      df$stat <- sub("lineYards", "line_yds", df$stat)
      df$stat <- sub("secondLevelYards", "second_lvl_yds", df$stat)
      df$stat <- sub("openFieldYards", "open_field_yds", df$stat)
      df$stat <- sub("Success", "_success", df$stat)
      df$stat <- sub("scoringOpportunities.", "scoring_opps_", df$stat)
      df$stat <- sub("pointsPerOpportunity", "pts_per_opp", df$stat)
      df$stat <- sub("Seven", "_seven", df$stat)
      df$stat <- sub("havoc.", "havoc_", df$stat)
      df$stat <- sub(".Average", "_avg", df$stat)
      df$stat <- sub("averageStartingPredictedPoints", "avg_starting_predicted_pts", df$stat)
      df$stat <- sub("averageStart", "avg_start", df$stat)
      df$stat <- sub(".team", "_team", df$stat)
      df$stat <- sub(".plays", "_plays", df$stat)
      df$stat <- sub("cumulativePpa", "cumulative_ppa", df$stat)

      if (!long) {
        team <- df %>%
          dplyr::filter(.data$stat == "ppa_team") %>%
          tidyr::pivot_longer(cols = c(.data$team1, .data$team2)) %>%
          dplyr::transmute(team = .data$value)

        df <- df %>%
          dplyr::filter(!stringr::str_detect(.data$stat, "team")) %>%
          tidyr::pivot_longer(cols = c(.data$team1, .data$team2)) %>%
          tidyr::pivot_wider(names_from = .data$stat, values_from = .data$value) %>%
          dplyr::select(-.data$name) %>%
          dplyr::mutate_all(as.numeric) %>%
          dplyr::bind_cols(team)  %>%
          dplyr::select(.data$team, tidyr::everything()) %>%
          as.data.frame()
        df <- df %>%
          dplyr::rename(
            rushing_line_yds_avg = .data$rushing_line_yd_avg,
            rushing_second_lvl_yds_avg = .data$rushing_second_lvl_yd_avg,
            rushing_open_field_yds_avg = .data$rushing_open_field_yd_avg)
      }

      if(verbose){
        message(glue::glue("{Sys.time()}: Scraping game advanced box score data for game_id '{game_id}'..."))
      }
    },
    error = function(e) {
        message(glue::glue("{Sys.time()}: game_id '{game_id}' invalid or no game advanced box score data available!"))
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
#' @return [cfbd_game_player_stats()] - A data frame with 32 variables:
#' \describe{
#'   \item{`game_id`: integer.}{Referencing game id.}
#'   \item{`team`: character.}{Team name.}
#'   \item{`conference`: character.}{Conference of the team.}
#'   \item{`home_away`: character.}{Flag for if the team was the home or away team.}
#'   \item{`points`: integer.}{Team points.}
#'   \item{`category`: character.}{Statistic category.}
#'   \item{`athlete_id`: character.}{Athlete referencing id.}
#'   \item{`name`: character.}{Player name.}
#'   \item{`c_att`: character.}{Completions - Pass attempts.}
#'   \item{`yds`: double.}{Statistic yardage.}
#'   \item{`avg`: double.}{Average per statistic.}
#'   \item{`td`: double.}{Touchdowns scored.}
#'   \item{`int`: double.}{Interceptions thrown.}
#'   \item{`qbr`: double.}{Quarterback rating.}
#'   \item{`car`: double.}{Number of rushing carries.}
#'   \item{`long`: double.}{Longest carry/reception of the game.}
#'   \item{`rec`: double.}{Number of pass receptions.}
#'   \item{`no`: double.}{Player number.}
#'   \item{`fg`: character.}{Field goal attempts in the game.}
#'   \item{`pct`: double.}{Field goal percentage in the game.}
#'   \item{`xp`: character.}{Extra points kicked in the game.}
#'   \item{`pts`: double.}{Total kicking points in the game.}
#'   \item{`tb`: double.}{Touchbacks (for kicking) in the game.}
#'   \item{`in_20`: double.}{Punts inside the 20 yardline in the game.}
#'   \item{`fum`: double.}{Player fumbles in the game.}
#'   \item{`lost`: double.}{Player fumbles lost in the game.}
#'   \item{`tot`: double.}{Total tackles in the game.}
#'   \item{`solo`: double.}{Solo tackles in the game.}
#'   \item{`sacks`: double.}{Total sacks in the game.}
#'   \item{`tfl`: double.}{Total tackles for loss in the game.}
#'   \item{`pd`: double.}{Total passes defensed in the game.}
#'   \item{`qb_hur`: double.}{Total quarterback hurries in the game.}
#' }
#' @source \url{https://api.collegefootballdata.com/games/players}
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
#'   cfbd_game_player_stats(year = 2020, week = 15, team = "Alabama")
#'
#'   cfbd_game_player_stats(2013, week = 1, team = "Florida State", category = "passing")
#' }

cfbd_game_player_stats <- function(year,
                                   week = NULL,
                                   season_type = "regular",
                                   team = NULL,
                                   conference = NULL,
                                   category = NULL,
                                   game_id = NULL,
                                   verbose = FALSE) {

  stat_categories <- c(
    "passing", "receiving", "rushing", "defensive", "fumbles",
    "interceptions", "punting", "puntReturns", "kicking", "kickReturns"
  )

  args <- list(year, week, season_type, team, conference, category, game_id)

  args <- args[lengths(args) != 0]

  # Check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
    msg = "Enter valid year as a number (YYYY)"
  )
  if (!is.null(week)) {
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
      msg = "Enter valid week 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)"
    )
  }
  if (season_type != "regular") {
    # Check if season_type is appropriate, if not regular
    assertthat::assert_that(season_type %in% c("postseason"),
      msg = "Enter valid season_type: regular, postseason"
    )
  }
  if (!is.null(team)) {
    if (team == "San Jose State") {
      team <- utils::URLencode(paste0("San Jos", "\u00e9", " State"), reserved = TRUE)
    } else {
      # Encode team parameter for URL if not NULL
      team <- utils::URLencode(team, reserved = TRUE)
    }
  }
  if (!is.null(conference)) {
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbfastR::cfbd_conf_types_df$abbreviation,
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference <- utils::URLencode(conference, reserved = TRUE)
  }
  if (!is.null(category)) {
    # Check category parameter in category if not NULL
    assertthat::assert_that(category %in% stat_categories,
      msg = "Incorrect category, potential misspelling.\nOffense: passing, receiving, rushing\nDefense: defensive, fumbles, interceptions\nSpecial Teams: punting, puntReturns, kicking, kickReturns"
    )
    # Encode conference parameter for URL, if not NULL
    category <- utils::URLencode(category, reserved = TRUE)
  }
  if (!is.null(game_id)) {
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
      msg = "Enter valid game_id (numeric value)"
    )
  }

  base_url <- "https://api.collegefootballdata.com/games/players?"

  full_url <- paste0(
    base_url,
    "year=", year,
    "&week=", week,
    "&seasonType=", season_type,
    "&team=", team,
    "&conference=", conference,
    "&category=", category,
    "&gameId=", game_id
  )

  # Check for CFBD API key
  if (!has_cfbd_key()) stop("CollegeFootballData.com now requires an API key.", "\n       See ?register_cfbd for details.", call. = FALSE)

  # Create the GET request and set response as res
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(Authorization = paste("Bearer", cfbd_key()))
  )


  # Check the result
  check_status(res)

  cols <- c(
    "game_id", "team", "conference", "home_away", "points", "category",
    "athlete_id", "name", "c_att", "yds", "avg", "td", "int", "qbr",
    "car", "long", "rec", "no", "fg", "pct", "xp", "pts", "tb", "in_20",
    "fum", "lost", "tot", "solo", "sacks", "tfl", "pd", "qb_hur"
  )
  numeric_cols <- c(
    "yds", "avg", "td", "int", "qbr",
    "car", "long", "rec", "no", "pct", "pts", "tb", "in_20",
    "fum", "lost", "tot", "solo", "sacks", "tfl", "pd", "qb_hur"
  )

  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content, tidyr::unnest, and return result as data.frame
      df <- res %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        furrr::future_map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(game_id = .data$id) %>%
        tidyr::unnest(.data$teams) %>%
        furrr::future_map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        tidyr::unnest(.data$categories) %>%
        furrr::future_map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(category = .data$name) %>%
        tidyr::unnest(.data$types) %>%
        furrr::future_map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(stat_category = .data$name) %>%
        tidyr::unnest(.data$athletes) %>%
        dplyr::rename(
          athlete_id = .data$id,
          team = .data$school,
          value = .data$stat
        ) %>%
        tidyr::pivot_wider(names_from = .data$stat_category, values_from = .data$value, values_fn = list) %>%
        janitor::clean_names()

      df[cols[!(cols %in% colnames(df))]] <- NA

      df <- df %>%
        dplyr::select(cols, dplyr::everything())

      if(verbose){
        message(glue::glue("{Sys.time()}: Scraping game player stats data..."))
      }
    },
    error = function(e) {
        message(glue::glue("{Sys.time()}: Invalid arguments or no game player stats data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  # is_c_att_present <- any(grepl("C/ATT",colnames(df)))
  # if(is_c_att_present){
  #   df <- df %>%
  #    dplyr::mutate("C/ATT"="0/0")
  # }
  return(df)
}




#' @title
#' **Get team records by year**
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): Team - Select a valid team, D1 football
#' @param conference (\emph{String} optional): DI Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC
#' @param verbose Logical parameter (TRUE/FALSE, default: FALSE) to return warnings and messages from function
#' @return [cfbd_game_records()] - A data frame with 20 variables:
#' \describe{
#'   \item{`year`: integer.}{Season of the games.}
#'   \item{`team`: character.}{Team name.}
#'   \item{`conference`: character.}{Conference of the team.}
#'   \item{`division`: character.}{Division in the conference of the team.}
#'   \item{`total_games`: integer.}{Total number of games played.}
#'   \item{`total_wins`: integer.}{Total wins.}
#'   \item{`total_losses`: integer.}{Total losses.}
#'   \item{`total_ties`: integer.}{Total ties.}
#'   \item{`conference_games`: integer.}{Number of conference games.}
#'   \item{`conference_wins`: integer.}{Total conference wins.}
#'   \item{`conference_losses`: integer.}{Total conference losses.}
#'   \item{`conference_ties`: integer.}{Total conference ties.}
#'   \item{`home_games`: integer.}{Total home games.}
#'   \item{`home_wins`: integer.}{Total home wins.}
#'   \item{`home_losses`: integer.}{Total home losses.}
#'   \item{`home_ties`: integer.}{Total home ties.}
#'   \item{`away_games`: integer.}{Total away games.}
#'   \item{`away_wins`: integer.}{Total away wins.}
#'   \item{`away_losses`: integer.}{Total away losses.}
#'   \item{`away_ties`: integer.}{Total away ties.}
#' }
#' @source \url{https://api.collegefootballdata.com/records}
#' @keywords Team Info
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET RETRY
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' \donttest{
#'   cfbd_game_records(2018, team = "Notre Dame")
#'
#'   cfbd_game_records(2013, team = "Florida State")
#' }

cfbd_game_records <- function(year,
                              team = NULL,
                              conference = NULL,
                              verbose = FALSE) {


  ## check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
    msg = "Enter valid year (Integer): 4 digits (YYYY)"
  )

  if (!is.null(team)) {
    if (team == "San Jose State") {
      team <- utils::URLencode(paste0("San Jos", "\u00e9", " State"), reserved = TRUE)
    } else {
      # Encode team parameter for URL if not NULL
      team <- utils::URLencode(team, reserved = TRUE)
    }
  }
  if (!is.null(conference)) {
    # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbfastR::cfbd_conf_types_df$abbreviation,
    #                         msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # # Encode conference parameter for URL, if not NULL
    conference <- utils::URLencode(conference, reserved = TRUE)
  }

  base_url <- "https://api.collegefootballdata.com/records?"

  full_url <- paste0(
    base_url,
    "year=", year,
    "&team=", team,
    "&conference=", conference
  )

  # Check for CFBD API key
  if (!has_cfbd_key()) stop("CollegeFootballData.com now requires an API key.", "\n       See ?register_cfbd for details.", call. = FALSE)

  # Create the GET request and set response as res
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(Authorization = paste("Bearer", cfbd_key()))
  )


  # Check the result
  check_status(res)

  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      df <- res %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        dplyr::rename(
          total_games = .data$total.games,
          total_wins = .data$total.wins,
          total_losses = .data$total.losses,
          total_ties = .data$total.ties,
          conference_games = .data$conferenceGames.games,
          conference_wins = .data$conferenceGames.wins,
          conference_losses = .data$conferenceGames.losses,
          conference_ties = .data$conferenceGames.ties,
          home_games = .data$homeGames.games,
          home_wins = .data$homeGames.wins,
          home_losses = .data$homeGames.losses,
          home_ties = .data$homeGames.ties,
          away_games = .data$awayGames.games,
          away_wins = .data$awayGames.wins,
          away_losses = .data$awayGames.losses,
          away_ties = .data$awayGames.ties
        )
      if(verbose){
        message(glue::glue("{Sys.time()}: Scraping game records data..."))
      }
    },
    error = function(e) {
        message(glue::glue("{Sys.time()}: Invalid arguments or no game records data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
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
#'   \item{`game_id`: integer.}{Referencing game id.}
#'   \item{`school`: character.}{Team name.}
#'   \item{`conference`: character.}{Conference of the team.}
#'   \item{`home_away`: character.}{Home/Away Flag.}
#'   \item{`points`: integer.}{Team points.}
#'   \item{`total_yards`: character.}{Team total yards.}
#'   \item{`net_passing_yards`: character.}{Team net passing yards.}
#'   \item{`completion_attempts`:character.}{Team completion attempts.}
#'   \item{`passing_tds`: character.}{Team passing touchdowns.}
#'   \item{`yards_per_pass`: character.}{Team game yards per pass.}
#'   \item{`passes_intercepted`: character.}{Team passes intercepted.}
#'   \item{`interception_yards`: character.}{Interception yards.}
#'   \item{`interception_tds`: character.}{Interceptions returned for a touchdown.}
#'   \item{`rushing_attempts`: character.}{Team rushing attempts. see also: ESTABLISH IT.}
#'   \item{`rushing_yards`: character.}{Team rushing yards.}
#'   \item{`rush_tds`: character.}{Team rushing touchdowns.}
#'   \item{`yards_per_rush_attempt`: character.}{Team yards per rush attempt.}
#'   \item{`first_downs`: character.}{First downs earned by the team.}
#'   \item{`third_down_eff`: character.}{Third down efficiency.}
#'   \item{`fourth_down_eff`: character.}{Fourth down efficiency.}
#'   \item{`punt_returns`: character.}{Team punt returns.}
#'   \item{`punt_return_yards`: character.}{Team punt return yards.}
#'   \item{`punt_return_tds`: character.}{Team punt return touchdowns.}
#'   \item{`kick_return_yards`: character.}{Team kick return yards.}
#'   \item{`kick_return_tds`: character.}{Team kick return touchdowns.}
#'   \item{`kick_returns`: character.}{Team kick returns.}
#'   \item{`kicking_points`: character.}{Team points from kicking the ball.}
#'   \item{`fumbles_recovered`: character.}{Team fumbles recovered.}
#'   \item{`fumbles_lost`: character.}{Team fumbles lost.}
#'   \item{`total_fumbles`: character.}{Team total fumbles.}
#'   \item{`tackles`: character.}{Team tackles.}
#'   \item{`tackles_for_loss`: character.}{Team tackles for a loss.}
#'   \item{`sacks`: character.}{Team sacks.}
#'   \item{`qb_hurries`: character.}{Team QB hurries.}
#'   \item{`interceptions`: character.}{Team interceptions.}
#'   \item{`passes_deflected`: character.}{Team passes deflected.}
#'   \item{`turnovers`: character.}{Team turnovers.}
#'   \item{`defensive_tds`: character.}{Team defensive touchdowns.}
#'   \item{`total_penalties_yards`: character.}{Team total penalty yards.}
#'   \item{`possession_time`: character.}{Team time of possession.}
#'   \item{`conference_allowed`: character.}{Conference of the opponent team.}
#'   \item{`home_away_allowed`: character.}{Flag for if the opponent was the home or away team.}
#'   \item{`points_allowed`: integer.}{Points for the opponent.}
#'   \item{`total_yards_allowed`: character.}{Opponent total yards.}
#'   \item{`net_passing_yards_allowed`: character.}{Opponent net passing yards.}
#'   \item{`completion_attempts_allowed`: character.}{Oppponent completion attempts.}
#'   \item{`passing_tds_allowed`: character.}{Opponent passing TDs.}
#'   \item{`yards_per_pass_allowed`: character.}{Opponent yards per pass allowed.}
#'   \item{`passes_intercepted_allowed`: character.}{Opponent passes intercepted.}
#'   \item{`interception_yards_allowed`: character.}{Opponent interception yards.}
#'   \item{`interception_tds_allowed`: character.}{Opponent interception TDs.}
#'   \item{`rushing_attempts_allowed`: character.}{Opponent rushing attempts.}
#'   \item{`rushing_yards_allowed`: character.}{Opponent rushing yards.}
#'   \item{`rush_tds_allowed`: character.}{Opponent rushing touchdownss.}
#'   \item{`yards_per_rush_attempt_allowed`: character.}{Opponent rushing yards per attempt.}
#'   \item{`first_downs_allowed`: character.}{Opponent first downs.}
#'   \item{`third_down_eff_allowed`: character.}{Opponent third down efficiency.}
#'   \item{`fourth_down_eff_allowed`: character.}{Opponent fourth down efficiency.}
#'   \item{`punt_returns_allowed`: character.}{Opponent punt returns.}
#'   \item{`punt_return_yards_allowed`: character.}{Opponent punt return yards.}
#'   \item{`punt_return_tds_allowed`: character.}{Opponent punt return touchdowns.}
#'   \item{`kick_return_yards_allowed`: character.}{Opponent kick return yards.}
#'   \item{`kick_return_tds_allowed`: character.}{Opponent kick return touchdowns.}
#'   \item{`kick_returns_allowed`: character.}{Opponent kick returns.}
#'   \item{`kicking_points_allowed`: character.}{Opponent points from kicking.}
#'   \item{`fumbles_recovered_allowed`: character.}{Opponent fumbles recovered.}
#'   \item{`fumbles_lost_allowed`: character.}{Opponent fumbles lost.}
#'   \item{`total_fumbles_allowed`:character.}{Opponent total number of fumbles.}
#'   \item{`tackles_allowed`:character.}{Opponent tackles.}
#'   \item{`tackles_for_loss_allowed`: character.}{Opponent tackles for loss.}
#'   \item{`sacks_allowed`: character.}{Opponent sacks.}
#'   \item{`qb_hurries_allowed`: character.}{Opponent quarterback hurries.}
#'   \item{`interceptions_allowed`: character.}{Opponent interceptions.}
#'   \item{`passes_deflected_allowed`: character.}{Opponent passes deflected.}
#'   \item{`turnovers_allowed`: character.}{Opponent turnovers.}
#'   \item{`defensive_tds_allowed`: character.}{Opponent defensive touchdowns.}
#'   \item{`total_penalties_yards_allowed`: character.}{Opponent total penalty yards.}
#'   \item{`possession_time_allowed`: character.}{Opponent time of possession.}
#' }
#' @source \url{https://api.collegefootballdata.com/games/teams}
#' @keywords Team Game Stats
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
#'   cfbd_game_team_stats(2019, team = "LSU")
#'
#'   cfbd_game_team_stats(2013, team = "Florida State")
#' }

cfl_game_team_stats <- function(season = NA, game_id = NA) {
  if (is.na(season)) {
    stop("A season year is required", call. = FALSE)
  } else if (!is.na(season) && is.na(game_id)) {
    stop('A game_id is required', call. = FALSE)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/games/', season,
                  '/game/', game_id, '?include=boxscore')
  }

  url <- build_url(url)
  games_call <- GET(url)
  games_data_JSON <- content(games_call)

  boxscore_JSON <- games_data_JSON$data[[1]]

  if(length(games_call) == 0) {
    boxscore_data <- data.frame()
  } else {
    boxscore_data <- flatten_boxscore(boxscore_JSON)
  }

  if (nrow(boxscore_data) == 0) {
    stop("No data found", call. = FALSE)
    NULL
  } else {
    boxscore_data
  }
}
