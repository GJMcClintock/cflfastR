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
    home_score = single_game_JSON[[home]]$score,
    home_time_of_pos = single_game_JSON$boxscore$teams[[home]]$offence$offence_possession_time,
    home_fumbles = single_game_JSON$boxscore$teams[[home]]$turnovers$fumbles,
    home_int = single_game_JSON$boxscore$teams[[home]]$turnovers$interceptions,
    home_downs = single_game_JSON$boxscore$teams[[home]]$turnovers$downs,
    home_pass_attempts = single_game_JSON$boxscore$teams[[away]]$passing$pass_attempts,
    home_pass_completions  = single_game_JSON$boxscore$teams[[away]]$passing$pass_completions,
    home_pass_net_yards = single_game_JSON$boxscore$teams[[away]]$passing$pass_net_yards,
    home_pass_long = single_game_JSON$boxscore$teams[[away]]$passing$pass_long,
    home_pass_touchdowns = single_game_JSON$boxscore$teams[[away]]$passing$pass_touchdowns,
    home_completion_percentage = single_game_JSON$boxscore$teams[[away]]$passing$pass_completion_percentage,
    home_pass_efficiency = single_game_JSON$boxscore$teams[[away]]$passing$pass_efficiency,
    home_pass_interceptions = single_game_JSON$boxscore$teams[[away]]$passing$pass_interceptions,
    home_pass_fumbles = single_game_JSON$boxscore$teams[[away]]$passing$pass_fumbles,
    home_rush_attempts = single_game_JSON$boxscore$teams[[away]]$rushing$rush_attempts,
    home_rush_net_yards = single_game_JSON$boxscore$teams[[away]]$rushing$rush_net_yards,
    home_rush_long = single_game_JSON$boxscore$teams[[away]]$rushing$rush_long,
    home_rush_touchdowns = single_game_JSON$boxscore$teams[[away]]$rushing$rush_touchdowns,
    home_rush_long_touchdowns = single_game_JSON$boxscore$teams[[away]]$rushing$rush_long_touchdowns,
    home_rec_attempts = single_game_JSON$boxscore$teams[[away]]$receiving$receive_attempts,
    home_rec_caught = single_game_JSON$boxscore$teams[[away]]$receiving$receive_caught,
    home_rec_yards = single_game_JSON$boxscore$teams[[away]]$receiving$receive_yards,
    home_rec_long = single_game_JSON$boxscore$teams[[away]]$receiving$receive_long,
    home_rec_touchdowns = single_game_JSON$boxscore$teams[[away]]$receiving$receive_touchdowns,
    home_rec_long_touchdowns = single_game_JSON$boxscore$teams[[away]]$receiving$receive_long_touchdowns,
    home_rec_yards_after_catch = single_game_JSON$boxscore$teams[[away]]$receiving$receive_yards_after_catch,
    home_rec_fumbles = single_game_JSON$boxscore$teams[[away]]$receiving$receive_fumbles,
    home_punts = single_game_JSON$boxscore$teams[[home]]$punts$punts,
    home_punt_yards = single_game_JSON$boxscore$teams[[home]]$punts$punt_yards,
    home_punt_net_yards = single_game_JSON$boxscore$teams[[home]]$punts$punt_net_yards,
    home_punt_long = single_game_JSON$boxscore$teams[[home]]$punts$punt_long,
    home_punt_singles = single_game_JSON$boxscore$teams[[home]]$punts$punt_singles,
    home_punts_blocked = single_game_JSON$boxscore$teams[[home]]$punts$punts_blocked,
    home_punts_in_10 = single_game_JSON$boxscore$teams[[home]]$punts$punts_in_10,
    home_punts_in_20 = single_game_JSON$boxscore$teams[[home]]$punts$punts_in_20,
    home_punts_returned = single_game_JSON$boxscore$teams[[home]]$punts$punts_returned,
    home_punt_returns = single_game_JSON$boxscore$teams[[home]]$punt_returns$punt_returns,
    home_punt_returns_yards = single_game_JSON$boxscore$teams[[home]]$punt_returns$punt_returns_yards,
    home_punt_returns_touchdowns = single_game_JSON$boxscore$teams[[home]]$punt_returns$punt_returns_touchdowns,
    home_punt_returns_long = single_game_JSON$boxscore$teams[[home]]$punt_returns$punt_returns_long,
    home_punt_returns_touchdowns_long = single_game_JSON$boxscore$teams[[home]]$punt_returns$punt_returns_touchdowns_long,
    home_kick_returns = single_game_JSON$boxscore$teams[[home]]$kick_returns$kick_returns,
    home_kick_returns_yards = single_game_JSON$boxscore$teams[[home]]$kick_returns$kick_returns_yards,
    home_kick_returns_touchdowns = single_game_JSON$boxscore$teams[[home]]$kick_returns$kick_returns_touchdowns,
    home_kick_returns_long = single_game_JSON$boxscore$teams[[home]]$kick_returns$kick_returns_long,
    home_kick_returns_touchdowns_long = single_game_JSON$boxscore$teams[[home]]$kick_returns$kick_returns_touchdowns_long,
    home_field_goal_attempts = single_game_JSON$boxscore$teams[[home]]$field_goals$field_goal_attempts,
    home_field_goal_made = single_game_JSON$boxscore$teams[[home]]$field_goals$field_goal_made,
    home_field_goal_yards = single_game_JSON$boxscore$teams[[home]]$field_goals$field_goal_yards,
    home_field_goal_singles = single_game_JSON$boxscore$teams[[home]]$field_goals$field_goal_singles,
    home_field_goal_long = single_game_JSON$boxscore$teams[[home]]$field_goals$field_goal_long,
    home_field_goal_points = single_game_JSON$boxscore$teams[[home]]$field_goals$field_goal_points,
    home_kicks = single_game_JSON$boxscore$teams[[home]]$kicking$kicks,
    home_kick_yards = single_game_JSON$boxscore$teams[[home]]$kicking$kick_yards,
    home_kicks_net_yards = single_game_JSON$boxscore$teams[[home]]$kicking$kicks_net_yards,
    home_kicks_long = single_game_JSON$boxscore$teams[[home]]$kicking$kicks_long,
    home_kicks_singles = single_game_JSON$boxscore$teams[[home]]$kicking$kicks_singles,
    home_kicks_out_of_end_zone = single_game_JSON$boxscore$teams[[home]]$kicking$kicks_out_of_end_zone,
    home_kicks_onside = single_game_JSON$boxscore$teams[[home]]$kicking$kicks_onside,
    home_one_point_convert_attempts = single_game_JSON$boxscore$teams[[home]]$converts$one_point_converts$attempts,
    home_one_point_convert_made = single_game_JSON$boxscore$teams[[home]]$converts$one_point_converts$made,
    home_two_point_convert_attempts = single_game_JSON$boxscore$teams[[home]]$converts$two_point_converts$attempts,
    home_two_point_convert_made = single_game_JSON$boxscore$teams[[home]]$converts$two_point_converts$made,
    home_tackles_total = single_game_JSON$boxscore$teams[[home]]$defence$tackles_total,
    home_tackles_defensive = single_game_JSON$boxscore$teams[[home]]$defence$tackles_defensive,
    home_tackles_special_teams = single_game_JSON$boxscore$teams[[home]]$defence$tackles_special_teams,
    home_sacks_qb_made = single_game_JSON$boxscore$teams[[home]]$defence$sacks_qb_made,
    home_interceptions = single_game_JSON$boxscore$teams[[home]]$defence$interceptions,
    home_fumbles_forced = single_game_JSON$boxscore$teams[[home]]$defence$fumbles_forced,
    home_fumbles_recovered = single_game_JSON$boxscore$teams[[home]]$defence$fumbles_recovered,
    home_passes_knocked_down = single_game_JSON$boxscore$teams[[home]]$defence$passes_knocked_down,
    home_defensive_touchdowns = single_game_JSON$boxscore$teams[[home]]$defence$defensive_touchdowns,
    home_defensive_safeties = single_game_JSON$boxscore$teams[[home]]$defence$defensive_safeties,
    home_total_penalties = single_game_JSON$boxscore$teams[[home]]$penalties$total,
    home_total_penalty_yards = single_game_JSON$boxscore$teams[[home]]$penalties$yards,
    home_offence_penalties = single_game_JSON$boxscore$teams[[home]]$penalties$offence_total,
    home_offence_penalty_yards = single_game_JSON$boxscore$teams[[home]]$penalties$offence_yards,
    home_defence_penalties = single_game_JSON$boxscore$teams[[home]]$penalties$defence_total,
    home_defence_penalty_yards = single_game_JSON$boxscore$teams[[home]]$penalties$defence_yards,
    home_special_teams_coverage_penalties = single_game_JSON$boxscore$teams[[home]]$penalties$special_teams_coverage_total,
    home_special_teams_coverage_penalty_yards = single_game_JSON$boxscore$teams[[home]]$penalties$special_teams_coverage_yards,
    home_special_teams_retrurn_penalties = single_game_JSON$boxscore$teams[[home]]$penalties$special_teams_return_total,
    home_special_teams_return_penalty_yards = single_game_JSON$boxscore$teams[[home]]$penalties$special_teams_return_yards,
    away_team = single_game_JSON[[away]]$abbreviation,
    away_score = single_game_JSON[[away]]$score,
    away_time_of_pos = single_game_JSON$boxscore$teams[[away]]$offence$offence_possession_time,
    away_fumbles = single_game_JSON$boxscore$teams[[away]]$turnovers$fumbles,
    away_int = single_game_JSON$boxscore$teams[[away]]$turnovers$interceptions,
    away_downs = single_game_JSON$boxscore$teams[[away]]$turnovers$downs,
    away_pass_attempts = single_game_JSON$boxscore$teams[[away]]$passing$pass_attempts,
    away_pass_completions  = single_game_JSON$boxscore$teams[[away]]$passing$pass_completions,
    away_pass_net_yards = single_game_JSON$boxscore$teams[[away]]$passing$pass_net_yards,
    away_pass_long = single_game_JSON$boxscore$teams[[away]]$passing$pass_long,
    away_pass_touchdowns = single_game_JSON$boxscore$teams[[away]]$passing$pass_touchdowns,
    away_completion_percentage = single_game_JSON$boxscore$teams[[away]]$passing$pass_completion_percentage,
    away_pass_efficiency = single_game_JSON$boxscore$teams[[away]]$passing$pass_efficiency,
    away_pass_interceptions = single_game_JSON$boxscore$teams[[away]]$passing$pass_interceptions,
    away_pass_fumbles = single_game_JSON$boxscore$teams[[away]]$passing$pass_fumbles,
    away_rush_attempts = single_game_JSON$boxscore$teams[[away]]$rushing$rush_attempts,
    away_rush_net_yards = single_game_JSON$boxscore$teams[[away]]$rushing$rush_net_yards,
    away_rush_long = single_game_JSON$boxscore$teams[[away]]$rushing$rush_long,
    away_rush_touchdowns = single_game_JSON$boxscore$teams[[away]]$rushing$rush_touchdowns,
    away_rush_long_touchdowns = single_game_JSON$boxscore$teams[[away]]$rushing$rush_long_touchdowns,
    away_rec_attempts = single_game_JSON$boxscore$teams[[away]]$receiving$receive_attempts,
    away_rec_caught = single_game_JSON$boxscore$teams[[away]]$receiving$receive_caught,
    away_rec_yards = single_game_JSON$boxscore$teams[[away]]$receiving$receive_yards,
    away_rec_long = single_game_JSON$boxscore$teams[[away]]$receiving$receive_long,
    away_rec_touchdowns = single_game_JSON$boxscore$teams[[away]]$receiving$receive_touchdowns,
    away_rec_long_touchdowns = single_game_JSON$boxscore$teams[[away]]$receiving$receive_long_touchdowns,
    away_rec_yards_after_catch = single_game_JSON$boxscore$teams[[away]]$receiving$receive_yards_after_catch,
    away_rec_fumbles = single_game_JSON$boxscore$teams[[away]]$receiving$receive_fumbles,
    away_punts = single_game_JSON$boxscore$teams[[away]]$punts$punts,
    away_punt_yards = single_game_JSON$boxscore$teams[[away]]$punts$punt_yards,
    away_punt_net_yards = single_game_JSON$boxscore$teams[[away]]$punts$punt_net_yards,
    away_punt_long = single_game_JSON$boxscore$teams[[away]]$punts$punt_long,
    away_punt_singles = single_game_JSON$boxscore$teams[[away]]$punts$punt_singles,
    away_punts_blocked = single_game_JSON$boxscore$teams[[away]]$punts$punts_blocked,
    away_punts_in_10 = single_game_JSON$boxscore$teams[[away]]$punts$punts_in_10,
    away_punts_in_20 = single_game_JSON$boxscore$teams[[away]]$punts$punts_in_20,
    away_punts_returned = single_game_JSON$boxscore$teams[[away]]$punts$punts_returned,
    away_punt_returns = single_game_JSON$boxscore$teams[[away]]$punt_returns$punt_returns,
    away_punt_returns_yards = single_game_JSON$boxscore$teams[[away]]$punt_returns$punt_returns_yards,
    away_punt_returns_touchdowns = single_game_JSON$boxscore$teams[[away]]$punt_returns$punt_returns_touchdowns,
    away_punt_returns_long = single_game_JSON$boxscore$teams[[away]]$punt_returns$punt_returns_long,
    away_punt_returns_touchdowns_long = single_game_JSON$boxscore$teams[[away]]$punt_returns$punt_returns_touchdowns_long,
    away_kick_returns = single_game_JSON$boxscore$teams[[away]]$kick_returns$kick_returns,
    away_kick_returns_yards = single_game_JSON$boxscore$teams[[away]]$kick_returns$kick_returns_yards,
    away_kick_returns_touchdowns = single_game_JSON$boxscore$teams[[away]]$kick_returns$kick_returns_touchdowns,
    away_kick_returns_long = single_game_JSON$boxscore$teams[[away]]$kick_returns$kick_returns_long,
    away_kick_returns_touchdowns_long = single_game_JSON$boxscore$teams[[away]]$kick_returns$kick_returns_touchdowns_long,
    away_field_goal_attempts = single_game_JSON$boxscore$teams[[away]]$field_goals$field_goal_attempts,
    away_field_goal_made = single_game_JSON$boxscore$teams[[away]]$field_goals$field_goal_made,
    away_field_goal_yards = single_game_JSON$boxscore$teams[[away]]$field_goals$field_goal_yards,
    away_field_goal_singles = single_game_JSON$boxscore$teams[[away]]$field_goals$field_goal_singles,
    away_field_goal_long = single_game_JSON$boxscore$teams[[away]]$field_goals$field_goal_long,
    away_field_goal_points = single_game_JSON$boxscore$teams[[away]]$field_goals$field_goal_points,
    away_kicks = single_game_JSON$boxscore$teams[[away]]$kicking$kicks,
    away_kick_yards = single_game_JSON$boxscore$teams[[away]]$kicking$kick_yards,
    away_kicks_net_yards = single_game_JSON$boxscore$teams[[away]]$kicking$kicks_net_yards,
    away_kicks_long = single_game_JSON$boxscore$teams[[away]]$kicking$kicks_long,
    away_kicks_singles = single_game_JSON$boxscore$teams[[away]]$kicking$kicks_singles,
    away_kicks_out_of_end_zone = single_game_JSON$boxscore$teams[[away]]$kicking$kicks_out_of_end_zone,
    away_kicks_onside = single_game_JSON$boxscore$teams[[away]]$kicking$kicks_onside,
    away_one_point_convert_attempts = single_game_JSON$boxscore$teams[[away]]$converts$one_point_converts$attempts,
    away_one_point_convert_made = single_game_JSON$boxscore$teams[[away]]$converts$one_point_converts$made,
    away_two_point_convert_attempts = single_game_JSON$boxscore$teams[[away]]$converts$two_point_converts$attempts,
    away_two_point_convert_made = single_game_JSON$boxscore$teams[[away]]$converts$two_point_converts$made,
    away_tackles_total = single_game_JSON$boxscore$teams[[away]]$defence$tackles_total,
    away_tackles_defensive = single_game_JSON$boxscore$teams[[away]]$defence$tackles_defensive,
    away_tackles_special_teams = single_game_JSON$boxscore$teams[[away]]$defence$tackles_special_teams,
    away_sacks_qb_made = single_game_JSON$boxscore$teams[[away]]$defence$sacks_qb_made,
    away_interceptions = single_game_JSON$boxscore$teams[[away]]$defence$interceptions,
    away_fumbles_forced = single_game_JSON$boxscore$teams[[away]]$defence$fumbles_forced,
    away_fumbles_recovered = single_game_JSON$boxscore$teams[[away]]$defence$fumbles_recovered,
    away_passes_knocked_down = single_game_JSON$boxscore$teams[[away]]$defence$passes_knocked_down,
    away_defensive_touchdowns = single_game_JSON$boxscore$teams[[away]]$defence$defensive_touchdowns,
    away_defensive_safeties = single_game_JSON$boxscore$teams[[away]]$defence$defensive_safeties,
    away_total_penalties = single_game_JSON$boxscore$teams[[away]]$penalties$total,
    away_total_penalty_yards = single_game_JSON$boxscore$teams[[away]]$penalties$yards,
    away_offence_penalties = single_game_JSON$boxscore$teams[[away]]$penalties$offence_total,
    away_offence_penalty_yards = single_game_JSON$boxscore$teams[[away]]$penalties$offence_yards,
    away_defence_penalties = single_game_JSON$boxscore$teams[[away]]$penalties$defence_total,
    away_defence_penalty_yards = single_game_JSON$boxscore$teams[[away]]$penalties$defence_yards,
    away_special_teams_coverage_penalties = single_game_JSON$boxscore$teams[[away]]$penalties$special_teams_coverage_total,
    away_special_teams_coverage_penalty_yards = single_game_JSON$boxscore$teams[[away]]$penalties$special_teams_coverage_yards,
    away_special_teams_retrurn_penalties = single_game_JSON$boxscore$teams[[away]]$penalties$special_teams_return_total,
    away_special_teams_return_penalty_yards = single_game_JSON$boxscore$teams[[away]]$penalties$special_teams_return_yards,
    stringsAsFactors = FALSE
  )
  return(dplyr::bind_cols(result1, result2))
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
