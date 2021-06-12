#' @name cfl_plays
#' @aliases cfl_plays
#' @title
#' **CFL Games Endpoint Overview**
#' @description Get results, statistics and information for games\cr
#' \describe{
#'   \item{`cfl_pbp_data()`:}{ Get play by play data from games.}
#' }
#' @details
#'
#' ###** Get PxP Data by year **
#'  ```r
#' bc_pbp <- cfl_pbp_data(season = 2018, team = 'BC')
#'
#'
#'

cfl_pbp_fetch <- function(season = NA, game_id = NA) {
  Sys.sleep(2)
  if (is.na(season)) {
    stop("A season year is required", call. = FALSE)
  } else if (!is.na(season) && is.na(game_id)) {
    stop('A game_id is required', call. = FALSE)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/games/', season,
                  '/game/', game_id, '?include=play_by_play')
  }

  url <- build_url(url)
  games_call <- GET(url)
  games_data_JSON <- content(games_call)$data[[1]]
  game_data <- extract_game_data_for_pbp(games_data_JSON)

  pbp_JSON <- games_data_JSON$play_by_play

  single_play_JSON <- pbp_JSON[[1]]

  if(length(games_call) == 0) {
    pbp_data <- data.frame()
  } else {
    pbp_data <- dplyr::bind_rows(
      lapply(1:length(pbp_JSON),
             function(x) flatten_play_by_play(pbp_JSON[[x]])))
  }

  if (nrow(pbp_data) == 0) {
    print(paste0("Missing data for ", game_data$game_id))
    NULL
  } else {
    data.frame(cbind(game_data, pbp_data))
  }
}
#' Query for CFL Play-by_Play information
#' @param season Required parameter for the football season.
#' @param game_id Specific Game Id
#' @param week Week of the season
#' @param team Request for specific team
#' @param event_type Request specific event type (Preseason, Regular, Grey Cup, etc)
#' @return [cfl_pbp_data()] - A data frame with 35 variables:
#' \describe{
#'  \item{`game_id` : integer.}{UUID of game}
#'  \item{`home_team` : character.}{Home team abbreviation}
#'  \item{`away_team` : character.}{Away abbreviation}
#'  \item{`season` : integer.}{Season}
#'  \item{`week` : integer.}{Week of season}
#'  \item{`play_id` : integer.}{UUID of play}
#'  \item{`play_sequence` : integer.}{Play number of game}
#'  \item{`quarter` : integer.}{Quarter}
#'  \item{`play_clock_start` : character.}{Play clock at snap}
#'  \item{`play_clock_start_in_secs` : integer.}{Seconds rem in quarter at snap}
#'  \item{`field_position_start` : character.}{Field position start}
#'  \item{`field_position_end` : character.}{Field position end}
#'  \item{`down` : integer.}{Down}
#'  \item{`yards_to_go` : integer.}{Yards for first down}
#'  \item{`is_in_red_zone` : logical.}{Is inside 20}
#'  \item{`team_home_score` : integer.}{Home score}
#'  \item{`team_visitor_score` : integer.}{Away score}
#'  \item{`play_type_id` : integer.}{Play Type Id}
#'  \item{`play_result_type_id` : integer.}{Result Type Id}
#'  \item{`play_result_yards` : integer.}{Result Yards}
#'  \item{`play_result_points` : integer.}{Result Points}
#'  \item{`play_success_id` : integer.}{Success Id}
#'  \item{`play_change_of_possession_occurred` : logical.}{Last play of posession}
#'  \item{`team_abbreviation` : character.}{Offense Abbreviation}
#'  \item{`team_id` : integer.}{Offensive Team Id}
#'  \item{`play_summary` : character.}{Play Summary}
#'  \item{`distance_to_goal` : numeric.}{Yards to Goal}
#'  \item{`seconds_remain_half` : numeric.}{Seconds rem in  half at snap}
#'  \item{`seconds_remain_game` : numeric.}{Seconds rem in  game at snap}
#'  \item{`quarterback_id` : integer.}{Quarterback CFL Player Id}
#'  \item{`ball_carrier_id` : integer.}{Ball carrier CFL player Id}
#'  \item{`primary_defender_id` : integer.}{Primary defender CFL Player Id}
#'  \item{`play_result_type_desc` : character.}{Play Result Type}
#'  \item{`play_type_desc` : character.}{Play Type}
#'  \item{`play_success_desc` : character.}{Success Description}
#'  }
#' @examples \dontrun{
#' cfl_pbp_data(season = 2016, game_id = 2280)
#' }
#' @export
#' @importFrom httr stop_for_status GET content
#' @importFrom dplyr bind_rows
#' @import assertthat
#' @importFrom purrr map_dfr2
cfl_pbp_data <- function(season = NA, game_id = NA, week = NA, team = NA, event_type = NA) {
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
  pbp <- purrr::map2_dfr(fetch_games$season, fetch_games$game_id, cfl_pbp_fetch)
  return(pbp)
}
