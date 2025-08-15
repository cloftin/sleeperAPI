
#' @export
get_draft_ids <- function(league_id = 1204180167983902720) {

  drafts <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/drafts")))
  return(drafts[[length(drafts)]]$draft_id)

}

get_draft_start <- function(draft_id = 1204180168000667648) {

  return(as.POSIXct(httr::content(httr::GET(paste0("https://api.sleeper.app/v1/draft/", draft_id)))$start_time / 1000))

}

#' @export
get_draft_order <- function(draft_id = 1204180168000667648) {
  t <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/draft/", draft_id)))
  t <- data.frame(owner_id = names(t$draft_order), slot = unlist(t$draft_order))
  row.names(t) <- c(1:12)

  managers <- sleeperAPI:::get_managers()

  t <- merge(t, managers, by = "owner_id", all.x = T)

  if(473510706843480064 %in% t$owner_id) {
    t[t$owner_id == 473510706843480064,]$manager <- "chip"
  }

  return(t)
}

#' @export
get_draft_picks <- function(draft_id = 1204180168000667648) {
  picks <- httr::content(httr::GET(paste0("https://api.sleeper.app/v1/draft/", draft_id, "/picks")))
  draft_start <- sleeperAPI:::get_draft_start(draft_id)

  draft_order <- sleeperAPI:::get_draft_order(draft_id)

  return(
    plyr::ldply(picks, function(x) {
      original_owner <- draft_order[draft_order$slot == x$draft_slot,]$manager
      return(data.frame(player_id = x$player_id, round = x$round, pick = x$draft_slot,
                        owner = x$picked_by, original_owner = original_owner, draft_start = draft_start))
    })
  )
}
