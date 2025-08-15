
# id: 1204180167983902720

#' @export
get_leagues <- function(id = 1204180167983902720, season = 2025) {

  user_id = "577448763228983296"
  t <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/user/", user_id, "/leagues/nfl/", season)))

  return(t)
}

#' @export
get_specific_league <- function(league_id = 1204180167983902720) {

  return(httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/league/", league_id))))
}

#' @export
get_managers <- function(league_id = 1204180167983902720) {
  managers <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/league/", league_id, "/users")))

  managers <- plyr::ldply(managers, function(x) {
    return(cbind(x$display_name, if(is.null(x$metadata$team_name)) {x$display_name} else {x$metadata$team_name}, x$user_id))
  })

  colnames(managers) <- c("manager", "team_name", "owner_id")

  roster <- unique(sleeperAPI:::get_rosters()[, c("roster_id", "owner_id")])

  managers <- merge(managers, roster, by = c("owner_id"))

  return(data.table::as.data.table(managers))
}

#' @export
get_rosters <- function(league_id = 1204180167983902720) {
  rosters <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/rosters")))

  return(
    data.table::as.data.table(plyr::ldply(rosters, function(x) {
      players <- data.frame(player_id = cbind(unlist(c(x$starters, x$players))))
      players$roster_id <- x$roster_id
      players$owner_id <- x$owner_id
      players
    }))
  )
}

#' @export
get_transactions <- function(league_id = 1204180167983902720, round = 1) {

  moves <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/league/", league_id, "/transactions/",  round)))

  trades <- moves[grep("trade", plyr::laply(moves, function(x) {x$type}))]

  traded_players <- plyr::ldply(trades, function(x) {
    if(!is.null(x$adds)) {
      players <- names(x$adds)
      team_to <- unlist(x$adds)
      team_from <- unlist(x$drops)

      players <- data.frame(player_id = players, to_owner = team_to, from_owner = team_from)

      players$transaction_id <- x$transaction_id
      players$transaction_date <- as.POSIXct(x$status_updated / 1000)

      return(players)

    }
  })

  traded_picks <- plyr::ldply(trades, function(x) {
    picks <- data.table::rbindlist(x$draft_picks)
    if(nrow(picks) > 0) {
      picks$league_id <- NULL
      data.table::setnames(picks, c("round", "season", "original_owner", "to_owner", "from_owner"))

      picks$transaction_id <- x$transaction_id
      picks$transaction_date <- as.POSIXct(x$status_updated / 1000)

      return(picks)
    }
  })

  return(list(data.table::as.data.table(traded_players), data.table::as.data.table(traded_picks)))
}


#' @export
get_all_traded_picks <- function(league_id = 1204180167983902720) {
  picks <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/league/", league_id, "/traded_picks")))
  picks <- data.table::rbindlist(picks)
  setnames(picks, c("round", "season", "original_owner", "to_owner", "from_owner"))

  managers <- sleeperAPI:::get_managers(league_id)

  picks$original_owner <- unlist(lapply(picks$original_owner, function(x) managers$manager[match(x, managers$roster_id)]))
  picks$to_owner <- unlist(lapply(picks$to_owner, function(x) managers$manager[match(x, managers$roster_id)]))
  picks$from_owner <- unlist(lapply(picks$from_owner, function(x) managers$manager[match(x, managers$roster_id)]))

  picks$id <- paste(picks$original_owner, picks$season, picks$round, sep = "_")

  return(picks)
}

#' @export
get_players <- function(league_id = 1204180167983902720, force_update = F) {

  if(force_update) {
    players <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/players/nfl")))
    saveRDS(players, "players.RDS")

    players <- do.call(rbind, players)

    players <- data.table::as.data.table(players[, c("team", "birth_date", "age", "player_id", "position", "full_name")])
    players <- players[position %in% c("QB", "RB", "WR", "TE"),]

    players <- sapply(players, as.character)

    write.csv(players, file = "players.csv", row.names = F)

    return(data.table::as.data.table(players))
  } else
    return(data.table::as.data.table(read.csv("players.csv", header = T, stringsAsFactors = F)))

}
