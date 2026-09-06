
#' @export
get_draft_ids <- function(league_id = 1204180167983902720) {

  drafts <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/drafts")))

  drafts <- plyr::ldply(drafts, function(x) {
    data.frame(season = x$season, type = x$type, draft_id = x$draft_id)
  })

  return(drafts)

}

get_draft_start <- function(draft_id = 1204180168000667648) {

  return(as.POSIXct(httr::content(httr::GET(paste0("https://api.sleeper.app/v1/draft/", draft_id)))$start_time / 1000))

}

#' @export
get_draft_order <- function(league_id = 1314005821864022016, draft_id = 1204180168000667648) {
  t <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/draft/", draft_id)))
  t <- data.frame(owner_id = names(t$draft_order), slot = unlist(t$draft_order))
  row.names(t) <- c(1:12)

  managers <- sleeperAPI:::get_managers(league_id)

  t <- merge(t, managers, by = "owner_id", all.x = T)

  if(473510706843480064 %in% t$owner_id) {
    t[t$owner_id == 473510706843480064,]$manager <- "chip"
  }

  return(t)
}

#' @export
get_draft_picks <- function(league_id = NULL, draft_id = 1204180168000667648) {
  picks <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/draft/", draft_id, "/picks")))
  draft_start <- sleeperAPI:::get_draft_start(draft_id)

  draft_order <- sleeperAPI:::get_draft_order(league_id, draft_id)

  picks <- plyr::ldply(picks, function(x) {
    original_owner <- draft_order[draft_order$slot == x$draft_slot,]$manager
    return(data.frame(player_id = x$player_id, round = x$round, pick = x$draft_slot,
                      picked_by = x$picked_by, original_owner = original_owner, draft_start = draft_start))
  })

  owners <- sleeperAPI:::get_managers(league_id)

  players <- sleeperAPI:::get_players()

  picks$owner <- unlist(lapply(picks$owner, function(x) owners$manager[match(x, owners$owner_id)]))
  picks$player <- unlist(lapply(picks$player_id, function(x) players$full_name[match(x, players$player_id)]))

  return(picks)

}
