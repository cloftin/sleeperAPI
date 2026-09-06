

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
get_all_traded_picks <- function(league_id = 1316257737595715584) {
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
#' @import data.table
combine_transactions <- function(trades, picks) {

  picks <- data.table::as.data.table(picks)
  picks[, type := "draft_pick"]

  picks$player_id <- NULL

  setnames(picks, c("player", "draft_start"), c("asset", "transaction_date"))

  picks[, c("owner_to", "owner_from", "transaction_id") := NA]
  trades[, c("round", "pick", "owner") := NA]

  t <- rbind(picks, trades, fill = T)
  setorder(t, transaction_date, transaction_id)

  return(t)

}
