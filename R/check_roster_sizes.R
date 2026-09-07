
#' @export
check_roster_sizes <- function(league_name = NULL, season_id = 2026) {

  league_id <- get_leagues(season = season_id)[grep(league_name, plyr::laply(get_leagues(season = season_id), function(x) {x$name}))][[1]]$league_id

  rosters <- sleeperAPI:::get_rosters(league_id)

  owners <- sleeperAPI:::get_managers(league_id)

  rosters <- rosters[roster_spot != "Taxi" & roster_spot != "IR",]

  rosters[, count := .N, by = "owner_id"]
  rosters <- unique(rosters[, c("owner_id", "count")])

  rosters$owner <- unlist(lapply(rosters$owner_id, function(x) owners$manager[match(x, owners$owner_id)]))

  return(rosters[, c("owner", "count")])
}
