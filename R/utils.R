#' @keywords Internal
#' @importFrom httr status_code stop_for_status GET content
#' @import dplyr
#'
check_status <- function(res) {

    x = status_code(res)

    if(x != 200) stop("The API returned an error", call. = FALSE)

}
# read qs files form an url
qs_from_url <- function(url) qs::qdeserialize(curl::curl_fetch_memory(url)$content)



# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots
message_completed <- function(x, in_builder = FALSE) {
  if (!in_builder) {
    usethis::ui_done("{usethis::ui_field(x)}")
  } else if (in_builder) {
    usethis::ui_done(x)
  }
}
user_message <- function(x, type) {
  if (type == "done") {
    usethis::ui_done("{my_time()} | {x}")
  } else if (type == "todo") {
    usethis::ui_todo("{my_time()} | {x}")
  } else if (type == "info") {
    usethis::ui_info("{my_time()} | {x}")
  } else if (type == "oops") {
    usethis::ui_oops("{my_time()} | {x}")
  }
}
# Identify sessions with sequential future resolving
is_sequential <- function() inherits(future::plan(), "sequential")
# check if a package is installed
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

most_recent_season <- function() {
  dplyr::if_else(
    as.double(substr(Sys.Date(), 6, 7)) >= 9,
    as.double(substr(Sys.Date(), 1, 4)),
    as.double(substr(Sys.Date(), 1, 4)) - 1
  )
}
my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")


# Set base API endpoint URL and build URLs using key

base_url <- function(base_url) {
  if (!missing(base_url)) {
    options(base_url = base_url)
  }
  invisible(getOption('base_url', 'http://api.cfl.ca/api/v1'))
}

build_url <- function(url) {
  if (!has_cfl_key()) {
    stop("The api_key is required, start by adding `CFL_API_KEY=XXXX-YOUR-API-KEY-HERE-XXXXX` to your .Renviron file.")
  }

  if (length(grep('\\?', url)) == 0) {
    paste0(url, '?key=', cfl_key())
  } else {
    paste0(url, '&key=', cfl_key())
  }
}

# Flatten a single game JSON to a dataframe
flatten_single_game <- function(single_game_JSON) {
  result1 <- dplyr::bind_rows(single_game_JSON[c("game_id", "date_start", "game_number", "week", "season", "attendance")])
  home <- ifelse(single_game_JSON$team_1$is_at_home, "team_1", "team_2")
  away <- ifelse(home == 'team_1', 'team_2', 'team_1')
  result2 <- data.frame(
    event_type = single_game_JSON$event_type$name,
    venue = single_game_JSON$venue$name,
    coin_toss = single_game_JSON$coin_toss$coin_toss_winner,
    winner = ifelse(
      single_game_JSON$event_status$name == 'Pre-Game',
      NA,
      ifelse(
        single_game_JSON$team_1$is_winner,
        single_game_JSON$team_1$abbreviation,
        single_game_JSON$team_2$abbreviation
      )
    ),
    home_team = single_game_JSON[[home]]$abbreviation,
    away_team = single_game_JSON[[away]]$abbreviation,
    home_score = single_game_JSON[[home]]$score,
    away_score = single_game_JSON[[away]]$score,
    stringsAsFactors = FALSE
  )
  return(dplyr::bind_cols(result1, result2))
}

# flatten a play-by-play JSON object to a tibble
flatten_play_by_play <- function(single_play_JSON) {
  fields1 <- c("play_id", "play_sequence", "quarter", "play_clock_start", "play_clock_start_in_secs", "field_position_start",
               "field_position_end", "down", "yards_to_go", "is_in_red_zone", "team_home_score",
               "team_visitor_score", "play_type_id", "play_result_type_id", "play_result_yards",
               "play_result_points", "play_success_id", "play_change_of_possession_occurred", "team_abbreviation",
               "team_id", "play_summary"
  )

  # Player IDs
  quarterback_id <- ifelse(
    single_play_JSON$players$quarterback$cfl_central_id == 0,
    NA,
    single_play_JSON$players$quarterback$cfl_central_id)

  ball_carrier_id <- ifelse(
    single_play_JSON$players$ball_carrier$cfl_central_id == 0,
    NA,
    single_play_JSON$players$ball_carrier$cfl_central_id)

  primary_defender_id <- ifelse(
    single_play_JSON$players$primary_defender$cfl_central_id == 0,
    NA,
    single_play_JSON$players$primary_defender$cfl_central_id)


  # Play descriptions/results
  play_type <- subset(play_type_id,
                      play_type_id == single_play_JSON$play_type_id,
                      select=play_type_desc)

  play_result <- subset(play_result_type_id,
                        play_result_type_id == single_play_JSON$play_result_type_id,
                        select=play_result_type_desc)

  play_success <- subset(play_success_id,
                         play_success_id == single_play_JSON$play_success_id,
                         select=play_success_desc)

  # clock Calcs
  seconds_remain_half <- (
    min(single_play_JSON$quarter,4) %% 2) * 900 +
    single_play_JSON$play_clock_start_in_secs

  seconds_remain_game <- (
    4 - min(single_play_JSON$quarter,4)) * 900 +
    single_play_JSON$play_clock_start_in_secs

  # Other Calcs
  distance_to_goal <- ifelse(
    substr(single_play_JSON$team_abbreviation,1,1)==substr(single_play_JSON$field_position_start,1,1),
    110 - as.integer(substr(single_play_JSON$field_position_start,2,nchar(single_play_JSON$field_position_start))),
    as.integer(substr(single_play_JSON$field_position_start,2,nchar(single_play_JSON$field_position_start)))
  )

  # Added player ids. Is a recursive reference best practice?
  result1 <- dplyr::bind_rows(single_play_JSON[fields1])
  result1 <- cbind(result1,distance_to_goal,seconds_remain_half,
                   seconds_remain_game,quarterback_id,ball_carrier_id,
                   primary_defender_id,play_result,play_type,play_success)
}

# get game metadata for a Play-by_play object
extract_game_data_for_pbp <- function(single_game_JSON) {
  home <- ifelse(single_game_JSON$team_1$is_at_home, "team_1", "team_2")
  away <- ifelse(home == 'team_1', 'team_2', 'team_1')
  result <- data.frame(
    game_id = single_game_JSON$game_id,
    home_team = single_game_JSON[[home]]$abbreviation,
    away_team = single_game_JSON[[away]]$abbreviation,
    stringsAsFactors=FALSE)
  return(result)
}

# Flatten a single game JSON to a dataframe with the boxscore
flatten_boxscore <- function(single_game_JSON) {
  game_meta <- dplyr::bind_rows(single_game_JSON[c("game_id", "date_start", "week", "season")])
  team1 <- data.frame(
    event_type = single_game_JSON$event_type$name,
    venue = single_game_JSON$venue$name,
    team = single_game_JSON$team_1$abbreviation,
    home_away = ifelse(single_game_JSON$team_1$is_at_home, 'home','away'),
    points_for = single_game_JSON$team_1$score,
    points_against = single_game_JSON$team_2$score,
    time_of_pos = single_game_JSON$boxscore$teams$team_1$offence$offence_possession_time,
    fumbles = single_game_JSON$boxscore$teams$team_1$turnovers$fumbles,
    int = single_game_JSON$boxscore$teams$team_1$turnovers$interceptions,
    downs = single_game_JSON$boxscore$teams$team_1$turnovers$downs,
    pass_attempts = single_game_JSON$boxscore$teams$team_1$passing$pass_attempts,
    pass_completions  = single_game_JSON$boxscore$teams$team_1$passing$pass_completions,
    pass_net_yards = single_game_JSON$boxscore$teams$team_1$passing$pass_net_yards,
    pass_long = single_game_JSON$boxscore$teams$team_1$passing$pass_long,
    pass_touchdowns = single_game_JSON$boxscore$teams$team_1$passing$pass_touchdowns,
    completion_percentage = single_game_JSON$boxscore$teams$team_1$passing$pass_completion_percentage,
    pass_efficiency = single_game_JSON$boxscore$teams$team_1$passing$pass_efficiency,
    pass_interceptions = single_game_JSON$boxscore$teams$team_1$passing$pass_interceptions,
    pass_fumbles = single_game_JSON$boxscore$teams$team_1$passing$pass_fumbles,
    rush_attempts = single_game_JSON$boxscore$teams$team_1$rushing$rush_attempts,
    rush_net_yards = single_game_JSON$boxscore$teams$team_1$rushing$rush_net_yards,
    rush_long = single_game_JSON$boxscore$teams$team_1$rushing$rush_long,
    rush_touchdowns = single_game_JSON$boxscore$teams$team_1$rushing$rush_touchdowns,
    rush_long_touchdowns = single_game_JSON$boxscore$teams$team_1$rushing$rush_long_touchdowns,
    rec_attempts = single_game_JSON$boxscore$teams$team_1$receiving$receive_attempts,
    rec_caught = single_game_JSON$boxscore$teams$team_1$receiving$receive_caught,
    rec_yards = single_game_JSON$boxscore$teams$team_1$receiving$receive_yards,
    rec_long = single_game_JSON$boxscore$teams$team_1$receiving$receive_long,
    rec_touchdowns = single_game_JSON$boxscore$teams$team_1$receiving$receive_touchdowns,
    rec_long_touchdowns = single_game_JSON$boxscore$teams$team_1$receiving$receive_long_touchdowns,
    rec_yards_after_catch = single_game_JSON$boxscore$teams$team_1$receiving$receive_yards_after_catch,
    rec_fumbles = single_game_JSON$boxscore$teams$team_1$receiving$receive_fumbles,
    punts = single_game_JSON$boxscore$teams$team_1$punts$punts,
    punt_yards = single_game_JSON$boxscore$teams$team_1$punts$punt_yards,
    punt_net_yards = single_game_JSON$boxscore$teams$team_1$punts$punt_net_yards,
    punt_long = single_game_JSON$boxscore$teams$team_1$punts$punt_long,
    punt_singles = single_game_JSON$boxscore$teams$team_1$punts$punt_singles,
    punts_blocked = single_game_JSON$boxscore$teams$team_1$punts$punts_blocked,
    punts_in_10 = single_game_JSON$boxscore$teams$team_1$punts$punts_in_10,
    punts_in_20 = single_game_JSON$boxscore$teams$team_1$punts$punts_in_20,
    punts_returned = single_game_JSON$boxscore$teams$team_1$punts$punts_returned,
    punt_returns = single_game_JSON$boxscore$teams$team_1$punt_returns$punt_returns,
    punt_returns_yards = single_game_JSON$boxscore$teams$team_1$punt_returns$punt_returns_yards,
    punt_returns_touchdowns = single_game_JSON$boxscore$teams$team_1$punt_returns$punt_returns_touchdowns,
    punt_returns_long = single_game_JSON$boxscore$teams$team_1$punt_returns$punt_returns_long,
    punt_returns_touchdowns_long = single_game_JSON$boxscore$teams$team_1$punt_returns$punt_returns_touchdowns_long,
    kick_returns = single_game_JSON$boxscore$teams$team_1$kick_returns$kick_returns,
    kick_returns_yards = single_game_JSON$boxscore$teams$team_1$kick_returns$kick_returns_yards,
    kick_returns_touchdowns = single_game_JSON$boxscore$teams$team_1$kick_returns$kick_returns_touchdowns,
    kick_returns_long = single_game_JSON$boxscore$teams$team_1$kick_returns$kick_returns_long,
    kick_returns_touchdowns_long = single_game_JSON$boxscore$teams$team_1$kick_returns$kick_returns_touchdowns_long,
    field_goal_attempts = single_game_JSON$boxscore$teams$team_1$field_goals$field_goal_attempts,
    field_goal_made = single_game_JSON$boxscore$teams$team_1$field_goals$field_goal_made,
    field_goal_yards = single_game_JSON$boxscore$teams$team_1$field_goals$field_goal_yards,
    field_goal_singles = single_game_JSON$boxscore$teams$team_1$field_goals$field_goal_singles,
    field_goal_long = single_game_JSON$boxscore$teams$team_1$field_goals$field_goal_long,
    field_goal_points = single_game_JSON$boxscore$teams$team_1$field_goals$field_goal_points,
    kicks = single_game_JSON$boxscore$teams$team_1$kicking$kicks,
    kick_yards = single_game_JSON$boxscore$teams$team_1$kicking$kick_yards,
    kicks_net_yards = single_game_JSON$boxscore$teams$team_1$kicking$kicks_net_yards,
    kicks_long = single_game_JSON$boxscore$teams$team_1$kicking$kicks_long,
    kicks_singles = single_game_JSON$boxscore$teams$team_1$kicking$kicks_singles,
    kicks_out_of_end_zone = single_game_JSON$boxscore$teams$team_1$kicking$kicks_out_of_end_zone,
    kicks_onside = single_game_JSON$boxscore$teams$team_1$kicking$kicks_onside,
    one_point_convert_attempts = single_game_JSON$boxscore$teams$team_1$converts$one_point_converts$attempts,
    one_point_convert_made = single_game_JSON$boxscore$teams$team_1$converts$one_point_converts$made,
    two_point_convert_attempts = single_game_JSON$boxscore$teams$team_1$converts$two_point_converts$attempts,
    two_point_convert_made = single_game_JSON$boxscore$teams$team_1$converts$two_point_converts$made,
    tackles_total = single_game_JSON$boxscore$teams$team_1$defence$tackles_total,
    tackles_defensive = single_game_JSON$boxscore$teams$team_1$defence$tackles_defensive,
    tackles_special_teams = single_game_JSON$boxscore$teams$team_1$defence$tackles_special_teams,
    sacks_qb_made = single_game_JSON$boxscore$teams$team_1$defence$sacks_qb_made,
    interceptions = single_game_JSON$boxscore$teams$team_1$defence$interceptions,
    fumbles_forced = single_game_JSON$boxscore$teams$team_1$defence$fumbles_forced,
    fumbles_recovered = single_game_JSON$boxscore$teams$team_1$defence$fumbles_recovered,
    passes_knocked_down = single_game_JSON$boxscore$teams$team_1$defence$passes_knocked_down,
    defensive_touchdowns = single_game_JSON$boxscore$teams$team_1$defence$defensive_touchdowns,
    defensive_safeties = single_game_JSON$boxscore$teams$team_1$defence$defensive_safeties,
    pass_attempts_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_attempts,
    pass_completions_allowed  = single_game_JSON$boxscore$teams$team_2$passing$pass_completions,
    pass_net_yards_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_net_yards,
    pass_long_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_long,
    pass_touchdowns_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_touchdowns,
    completion_percentage_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_completion_percentage,
    pass_efficiency_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_efficiency,
    pass_interceptions_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_interceptions,
    pass_fumbles_allowed = single_game_JSON$boxscore$teams$team_2$passing$pass_fumbles,
    rush_attempts_allowed = single_game_JSON$boxscore$teams$team_2$rushing$rush_attempts,
    rush_net_yards_allowed = single_game_JSON$boxscore$teams$team_2$rushing$rush_net_yards,
    rush_long_allowed = single_game_JSON$boxscore$teams$team_2$rushing$rush_long,
    rush_touchdowns_allowed = single_game_JSON$boxscore$teams$team_2$rushing$rush_touchdowns,
    rush_long_touchdowns_allowed = single_game_JSON$boxscore$teams$team_2$rushing$rush_long_touchdowns,
    rec_attempts_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_attempts,
    rec_caught_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_caught,
    rec_yards_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_yards,
    rec_long_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_long,
    rec_touchdowns_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_touchdowns,
    rec_long_touchdowns_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_long_touchdowns,
    rec_yards_after_catch_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_yards_after_catch,
    rec_fumbles_allowed = single_game_JSON$boxscore$teams$team_2$receiving$receive_fumbles,
    total_penalties = single_game_JSON$boxscore$teams$team_1$penalties$total,
    total_penalty_yards = single_game_JSON$boxscore$teams$team_1$penalties$yards,
    offence_penalties = single_game_JSON$boxscore$teams$team_1$penalties$offence_total,
    offence_penalty_yards = single_game_JSON$boxscore$teams$team_1$penalties$offence_yards,
    defence_penalties = single_game_JSON$boxscore$teams$team_1$penalties$defence_total,
    defence_penalty_yards = single_game_JSON$boxscore$teams$team_1$penalties$defence_yards,
    special_teams_coverage_penalties = single_game_JSON$boxscore$teams$team_1$penalties$special_teams_coverage_total,
    special_teams_coverage_penalty_yards = single_game_JSON$boxscore$teams$team_1$penalties$special_teams_coverage_yards,
    special_teams_retrurn_penalties = single_game_JSON$boxscore$teams$team_1$penalties$special_teams_return_total,
    special_teams_return_penalty_yards = single_game_JSON$boxscore$teams$team_1$penalties$special_teams_return_yards,
    stringsAsFactors = FALSE
  )

  team2 <- data.frame(
    event_type = single_game_JSON$event_type$name,
    venue = single_game_JSON$venue$name,
    team = single_game_JSON$team_1$abbreviation,
    home_away = ifelse(single_game_JSON$team_2$is_at_home, 'home','away'),
    points_for = single_game_JSON$team_2$score,
    points_against = single_game_JSON$team_1$score,
    time_of_pos = single_game_JSON$boxscore$teams$team_2$offence$offence_possession_time,
    fumbles = single_game_JSON$boxscore$teams$team_2$turnovers$fumbles,
    int = single_game_JSON$boxscore$teams$team_2$turnovers$interceptions,
    downs = single_game_JSON$boxscore$teams$team_2$turnovers$downs,
    pass_attempts = single_game_JSON$boxscore$teams$team_2$passing$pass_attempts,
    pass_completions  = single_game_JSON$boxscore$teams$team_2$passing$pass_completions,
    pass_net_yards = single_game_JSON$boxscore$teams$team_2$passing$pass_net_yards,
    pass_long = single_game_JSON$boxscore$teams$team_2$passing$pass_long,
    pass_touchdowns = single_game_JSON$boxscore$teams$team_2$passing$pass_touchdowns,
    completion_percentage = single_game_JSON$boxscore$teams$team_2$passing$pass_completion_percentage,
    pass_efficiency = single_game_JSON$boxscore$teams$team_2$passing$pass_efficiency,
    pass_interceptions = single_game_JSON$boxscore$teams$team_2$passing$pass_interceptions,
    pass_fumbles = single_game_JSON$boxscore$teams$team_2$passing$pass_fumbles,
    rush_attempts = single_game_JSON$boxscore$teams$team_2$rushing$rush_attempts,
    rush_net_yards = single_game_JSON$boxscore$teams$team_2$rushing$rush_net_yards,
    rush_long = single_game_JSON$boxscore$teams$team_2$rushing$rush_long,
    rush_touchdowns = single_game_JSON$boxscore$teams$team_2$rushing$rush_touchdowns,
    rush_long_touchdowns = single_game_JSON$boxscore$teams$team_2$rushing$rush_long_touchdowns,
    rec_attempts = single_game_JSON$boxscore$teams$team_2$receiving$receive_attempts,
    rec_caught = single_game_JSON$boxscore$teams$team_2$receiving$receive_caught,
    rec_yards = single_game_JSON$boxscore$teams$team_2$receiving$receive_yards,
    rec_long = single_game_JSON$boxscore$teams$team_2$receiving$receive_long,
    rec_touchdowns = single_game_JSON$boxscore$teams$team_2$receiving$receive_touchdowns,
    rec_long_touchdowns = single_game_JSON$boxscore$teams$team_2$receiving$receive_long_touchdowns,
    rec_yards_after_catch = single_game_JSON$boxscore$teams$team_2$receiving$receive_yards_after_catch,
    rec_fumbles = single_game_JSON$boxscore$teams$team_2$receiving$receive_fumbles,
    punts = single_game_JSON$boxscore$teams$team_2$punts$punts,
    punt_yards = single_game_JSON$boxscore$teams$team_2$punts$punt_yards,
    punt_net_yards = single_game_JSON$boxscore$teams$team_2$punts$punt_net_yards,
    punt_long = single_game_JSON$boxscore$teams$team_2$punts$punt_long,
    punt_singles = single_game_JSON$boxscore$teams$team_2$punts$punt_singles,
    punts_blocked = single_game_JSON$boxscore$teams$team_2$punts$punts_blocked,
    punts_in_10 = single_game_JSON$boxscore$teams$team_2$punts$punts_in_10,
    punts_in_20 = single_game_JSON$boxscore$teams$team_2$punts$punts_in_20,
    punts_returned = single_game_JSON$boxscore$teams$team_2$punts$punts_returned,
    punt_returns = single_game_JSON$boxscore$teams$team_2$punt_returns$punt_returns,
    punt_returns_yards = single_game_JSON$boxscore$teams$team_2$punt_returns$punt_returns_yards,
    punt_returns_touchdowns = single_game_JSON$boxscore$teams$team_2$punt_returns$punt_returns_touchdowns,
    punt_returns_long = single_game_JSON$boxscore$teams$team_2$punt_returns$punt_returns_long,
    punt_returns_touchdowns_long = single_game_JSON$boxscore$teams$team_2$punt_returns$punt_returns_touchdowns_long,
    kick_returns = single_game_JSON$boxscore$teams$team_2$kick_returns$kick_returns,
    kick_returns_yards = single_game_JSON$boxscore$teams$team_2$kick_returns$kick_returns_yards,
    kick_returns_touchdowns = single_game_JSON$boxscore$teams$team_2$kick_returns$kick_returns_touchdowns,
    kick_returns_long = single_game_JSON$boxscore$teams$team_2$kick_returns$kick_returns_long,
    kick_returns_touchdowns_long = single_game_JSON$boxscore$teams$team_2$kick_returns$kick_returns_touchdowns_long,
    field_goal_attempts = single_game_JSON$boxscore$teams$team_2$field_goals$field_goal_attempts,
    field_goal_made = single_game_JSON$boxscore$teams$team_2$field_goals$field_goal_made,
    field_goal_yards = single_game_JSON$boxscore$teams$team_2$field_goals$field_goal_yards,
    field_goal_singles = single_game_JSON$boxscore$teams$team_2$field_goals$field_goal_singles,
    field_goal_long = single_game_JSON$boxscore$teams$team_2$field_goals$field_goal_long,
    field_goal_points = single_game_JSON$boxscore$teams$team_2$field_goals$field_goal_points,
    kicks = single_game_JSON$boxscore$teams$team_2$kicking$kicks,
    kick_yards = single_game_JSON$boxscore$teams$team_2$kicking$kick_yards,
    kicks_net_yards = single_game_JSON$boxscore$teams$team_2$kicking$kicks_net_yards,
    kicks_long = single_game_JSON$boxscore$teams$team_2$kicking$kicks_long,
    kicks_singles = single_game_JSON$boxscore$teams$team_2$kicking$kicks_singles,
    kicks_out_of_end_zone = single_game_JSON$boxscore$teams$team_2$kicking$kicks_out_of_end_zone,
    kicks_onside = single_game_JSON$boxscore$teams$team_2$kicking$kicks_onside,
    one_point_convert_attempts = single_game_JSON$boxscore$teams$team_2$converts$one_point_converts$attempts,
    one_point_convert_made = single_game_JSON$boxscore$teams$team_2$converts$one_point_converts$made,
    two_point_convert_attempts = single_game_JSON$boxscore$teams$team_2$converts$two_point_converts$attempts,
    two_point_convert_made = single_game_JSON$boxscore$teams$team_2$converts$two_point_converts$made,
    tackles_total = single_game_JSON$boxscore$teams$team_2$defence$tackles_total,
    tackles_defensive = single_game_JSON$boxscore$teams$team_2$defence$tackles_defensive,
    tackles_special_teams = single_game_JSON$boxscore$teams$team_2$defence$tackles_special_teams,
    sacks_qb_made = single_game_JSON$boxscore$teams$team_2$defence$sacks_qb_made,
    interceptions = single_game_JSON$boxscore$teams$team_2$defence$interceptions,
    fumbles_forced = single_game_JSON$boxscore$teams$team_2$defence$fumbles_forced,
    fumbles_recovered = single_game_JSON$boxscore$teams$team_2$defence$fumbles_recovered,
    passes_knocked_down = single_game_JSON$boxscore$teams$team_2$defence$passes_knocked_down,
    defensive_touchdowns = single_game_JSON$boxscore$teams$team_2$defence$defensive_touchdowns,
    defensive_safeties = single_game_JSON$boxscore$teams$team_2$defence$defensive_safeties,
    pass_attempts_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_attempts,
    pass_completions_allowed  = single_game_JSON$boxscore$teams$team_1$passing$pass_completions,
    pass_net_yards_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_net_yards,
    pass_long_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_long,
    pass_touchdowns_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_touchdowns,
    completion_percentage_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_completion_percentage,
    pass_efficiency_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_efficiency,
    pass_interceptions_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_interceptions,
    pass_fumbles_allowed = single_game_JSON$boxscore$teams$team_1$passing$pass_fumbles,
    rush_attempts_allowed = single_game_JSON$boxscore$teams$team_1$rushing$rush_attempts,
    rush_net_yards_allowed = single_game_JSON$boxscore$teams$team_1$rushing$rush_net_yards,
    rush_long_allowed = single_game_JSON$boxscore$teams$team_1$rushing$rush_long,
    rush_touchdowns_allowed = single_game_JSON$boxscore$teams$team_1$rushing$rush_touchdowns,
    rush_long_touchdowns_allowed = single_game_JSON$boxscore$teams$team_1$rushing$rush_long_touchdowns,
    rec_attempts_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_attempts,
    rec_caught_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_caught,
    rec_yards_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_yards,
    rec_long_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_long,
    rec_touchdowns_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_touchdowns,
    rec_long_touchdowns_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_long_touchdowns,
    rec_yards_after_catch_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_yards_after_catch,
    rec_fumbles_allowed = single_game_JSON$boxscore$teams$team_1$receiving$receive_fumbles,
    total_penalties = single_game_JSON$boxscore$teams$team_2$penalties$total,
    total_penalty_yards = single_game_JSON$boxscore$teams$team_2$penalties$yards,
    offence_penalties = single_game_JSON$boxscore$teams$team_2$penalties$offence_total,
    offence_penalty_yards = single_game_JSON$boxscore$teams$team_2$penalties$offence_yards,
    defence_penalties = single_game_JSON$boxscore$teams$team_2$penalties$defence_total,
    defence_penalty_yards = single_game_JSON$boxscore$teams$team_2$penalties$defence_yards,
    special_teams_coverage_penalties = single_game_JSON$boxscore$teams$team_2$penalties$special_teams_coverage_total,
    special_teams_coverage_penalty_yards = single_game_JSON$boxscore$teams$team_2$penalties$special_teams_coverage_yards,
    special_teams_retrurn_penalties = single_game_JSON$boxscore$teams$team_2$penalties$special_teams_return_total,
    special_teams_return_penalty_yards = single_game_JSON$boxscore$teams$team_2$penalties$special_teams_return_yards,
    stringsAsFactors = FALSE
  )
  return(dplyr::bind_rows(bind_cols(game_meta, team1),bind_cols(game_meta, team2)))
}

# rule_header <- function(x) {
#   rlang::inform(
#     cli::rule(
#       left = crayon::bold(x),
#       right = paste0("cfbfastR version ", utils::packageVersion("cfbfastR")),
#       width = getOption("width")
#     )
#   )
# }
#
# rule_footer <- function(x) {
#   rlang::inform(
#     cli::rule(
#       left = crayon::bold(x),
#       width = getOption("width")
#     )
#   )
# }
