

#' @export
get_players <- function(league_id = 1204180167983902720, force_update = F) {

  if(force_update | !file.exists("players.csv")) {
    players <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/players/nfl")))
    saveRDS(players, "players.RDS")

    players <- do.call(rbind, players)

    players <- data.table::as.data.table(players[, c("team", "birth_date", "age", "player_id", "position", "full_name")])
    players[full_name == 'Travis Hunter', position := "WR"]
    players <- players[position %in% c("QB", "RB", "WR", "TE"),]

    players <- sapply(players, as.character)

    write.csv(players, file = "players.csv", row.names = F)

    return(data.table::as.data.table(players))
  } else
    return(data.table::as.data.table(read.csv("players.csv", header = T, stringsAsFactors = F)))

}
