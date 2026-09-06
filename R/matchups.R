


#' @export
get_week <- function() {
  return(httr::content(httr::GET("https://api.sleeper.app/v1/state/nfl"))$week[1])
}

#' @export
get_matchups <- function(league_id = 1204180167983902720, week = 1) {

  t <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/matchups/", week)))

  matchups <- plyr::ldply(t, function(x) {
    t <- data.frame(
      player_id = unlist(x$starters), points = unlist(x$starters_points)
    )

    t$roster_id <- x$roster_id
    t$matchup_id <- x$matchup_id

    t$pos <- c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "FLEX1", "FLEX2", "FLEX3", "SFLEX")
    t$pos_order <- c(1:10)

    return(t)
  })

  players <- sleeperAPI:::get_players()

  matchups$player <- unlist(lapply(matchups$player, function(x) players$full_name[match(x, players$player_id)]))

  managers <- sleeperAPI:::get_managers(league_id)

  matchups$owner <- unlist(lapply(matchups$roster_id, function(x) managers$team_name[match(x, managers$roster_id)]))

  matchups <- data.table:::as.data.table(matchups)

  matchups[, team_points := sum(points), by = "roster_id"]

  matchups[, winning_team := team_points == max(team_points), by = "matchup_id"]

  winning_team <- matchups[winning_team == TRUE,]
  losing_team <- matchups[winning_team == FALSE,]

  matchups <- merge(matchups[winning_team == TRUE,], matchups[winning_team == FALSE,],
                    by = c("matchup_id", "pos", "pos_order"))

  data.table::setorder(matchups, matchup_id, pos_order)

  return(matchups)

}
