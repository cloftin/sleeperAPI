
# id: 1204180167983902720

#' @export
get_leagues <- function(id = 1204180167983902720, season = 2024) {

  user_id = "577448763228983296"
  t <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/user/", user_id, "/leagues/nfl/", 2024)))

}

get_specific_league <- function(league_id = 1204180167983902720, season = 2025) {

  httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/league/", league_id)))
}

#' @export
get_managers <- function(league_id = 1204180167983902720, season = 2025) {
  managers <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/league/", league_id, "/users")))

  managers <- plyr::ldply(managers, function(x) {
    return(cbind(x$display_name, if(is.null(x$metadata$team_name)) {x$display_name} else {x$metadata$team_name}, x$user_id))
  })

  colnames(managers) <- c("manager", "team_name", "user_id")

  return(managers)
}

#' @export
get_rosters <- function(league_id = 1204180167983902720) {
  rosters <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/rosters")))

  plyr::ldply(rosters, function(x) {
    players <- data.frame(players = cbind(unlist(c(x$starters, x$players))))
    players$roster_id <- x$roster_id
    players$ownder_id <- x$owner_id
    players
  })

}

get_transactions <- function(league_id = 1204180167983902720) {

  moves <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/league/", league_id, "/transactions/1")))

  trades <- moves[grep("trade", plyr::laply(moves, function(x) {x$type}))]

  trades <- do.call(rbind, trades)

  trades <-

}


get_players <- function(league_id = 1204180167983902720, force_update = F) {

  if(force_update) {
    players <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/players/nfl")))
    saveRDS(players, "players.RDS")

    players <- do.call(rbind, players)

    players <- data.table::as.data.table(players[, c("team", "birth_date", "age", "player_id", "position", "full_name")])
    players <- players[position %in% c("QB", "RB", "WR", "TE"),]

    players <- sapply(players, as.character)

    write.csv(players, file = "players.csv", row.names = F)

  } else
    return(read.csv("players.csv", row.names = F, stringsAsFactors = F))

}
