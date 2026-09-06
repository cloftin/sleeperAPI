
#' @export
check_taxi_squads <- function(league_name = "Momma", season_id = 2026) {

  league_id <- get_leagues(season = season_id)[grep(league_name, plyr::laply(get_leagues(season = season_id), function(x) {x$name}))][[1]]$league_id

  rosters <- sleeperAPI:::get_rosters(league_id)

  owners <- sleeperAPI:::get_managers(league_id)


  all_draft_picks <- data.frame()

  for(season in 2020:season_id) {

    league <- get_leagues(season = season)
    if(length(grep(league_name, plyr::laply(league, function(x) {x$name}))) > 0) {
      league_id <- league[[grep(league_name, plyr::laply(league, function(x) {x$name}))]]$league_id
      print(season)
      draft_id <- get_draft_ids(league_id)

      for(i in 1:nrow(draft_id)) {
        picks <- get_draft_picks(league_id = league_id, draft_id = draft_id$draft_id[i])
        picks <- picks[!is.na(picks$player),]
        picks$season <- season
        picks$draft_type <- draft_id$type[i]

        all_draft_picks <- rbind(all_draft_picks, picks)
      }
    }
  }

  all_draft_picks$picked_by <- unlist(lapply(all_draft_picks$picked_by, function(x) owners$manager[match(x, owners$owner_id)]))

  rosters$owner <- unlist(lapply(rosters$owner_id, function(x) owners$manager[match(x, owners$owner_id)]))

  rosters <- merge(rosters, all_draft_picks, by = c("player_id", "player"), all = T)

  return(rosters[roster_spot == 'Taxi' & (owner != picked_by | is.na(picked_by)),])

}
