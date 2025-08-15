
#' @export
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
