

#' @export
get_user <- function(username = NULL) {

  t <- httr::content(httr::GET(url = paste0("https://api.sleeper.app/v1/user/", username)))
  return(data.frame(username = username, user_id = t$user_id, avatar = t$avatar))
}

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
    return(cbind(x$display_name, if(is.null(x$metadata$team_name)) {x$display_name} else {x$metadata$team_name}, x$user_id, x$avatar))
  })

  colnames(managers) <- c("manager", "team_name", "owner_id", "avatar")

  roster <- unique(sleeperAPI:::get_rosters(league_id)[, c("roster_id", "owner_id")])

  managers <- merge(managers, roster, by = c("owner_id"))

  return(data.table::as.data.table(managers))
}

#' @export
get_rosters <- function(league_id = 1204180167983902720) {
  rosters <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/rosters")))

  rosters <- data.table::as.data.table(plyr::ldply(rosters, function(x) {
    starters <- as.data.frame(cbind(unlist(x$starters), rep("Starter", length(x$starters))))
    bench <- as.data.frame(cbind(unlist(x$players), rep("Bench", length(x$players))))
    taxi <- as.data.frame(cbind(unlist(x$taxi), rep("Taxi", length(x$taxi))))

    bench <- bench[!(bench[,1] %in% starters[,1]) & !(bench[,1] %in% taxi[,1]),]

    players <- rbind(starters, bench, taxi)

    players$roster_id <- x$roster_id
    players$owner_id <- x$owner_id
    colnames(players) <- c("player_id", "roster_spot", "roster_id", "owner_id")
    return(players)
  }))

  players <- sleeperAPI:::get_players()

  rosters$player <- unlist(lapply(rosters$player_id, function(x) players$full_name[match(x, players$player_id)]))

  return(rosters)

}
