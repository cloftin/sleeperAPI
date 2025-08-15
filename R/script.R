
library(sleeperAPI)
library(data.table)

players <- get_players()

managers <- get_managers()

traded_players <- data.frame()
traded_picks <- data.frame()

for(season in 2020:2025) {
  prev_leagues <- get_leagues(season = season)
  prev_league_id <- prev_leagues[[grep("Raleighwood Fantasy Football League", lapply(prev_leagues, function(x) {x$name}))]]$league_id

  for(i in 1:20) {
    transactions <- get_transactions(league_id = prev_league_id, round = i)
    traded_players <- rbind(traded_players, transactions[[1]])
    traded_picks <- rbind(traded_picks, transactions[[2]])
  }
}

traded_players$player <- unlist(lapply(traded_players$player_id, function(x) players$full_name[match(x, players$player_id)]))
traded_players$owner_to <- unlist(lapply(traded_players$to_owner, function(x) managers$manager[match(x, managers$roster_id)]))
traded_players$owner_from <- unlist(lapply(traded_players$from_owner, function(x) managers$manager[match(x, managers$roster_id)]))
traded_players[, type := "player_trade"]

traded_picks$original_owner <- unlist(lapply(traded_picks$original_owner, function(x) managers$manager[match(x, managers$roster_id)]))
traded_picks$to_owner <- unlist(lapply(traded_picks$to_owner, function(x) managers$manager[match(x, managers$roster_id)]))
traded_picks$from_owner <- unlist(lapply(traded_picks$from_owner, function(x) managers$manager[match(x, managers$roster_id)]))
traded_picks[, asset_id := paste(original_owner, season, round, sep = "_")]
traded_picks[, type := "pick_trade"]

all_trans <- rbind(traded_players[, c("player", "owner_to", "owner_from", "type", "transaction_date", "transaction_id")],
                   traded_picks[, c("asset_id", "to_owner", "from_owner", "type", "transaction_date", "transaction_id")], use.names = F)
setorder(all_trans, transaction_date, transaction_id)
setnames(all_trans, c("player"), c("asset"))






###### Get All Draft Picks #######

all_draft_picks <- data.frame()

for(season in 2020:2025) {

  league <- get_leagues(season = season)
  league_id <- league[[grep("Raleighwood", plyr::laply(league, function(x) {x$name}))]]$league_id

  draft_id <- get_draft_ids(league_id)

  picks <- get_draft_picks(draft_id)
  picks$season <- season

  all_draft_picks <- rbind(all_draft_picks, picks)
}

all_draft_picks <- as.data.table(all_draft_picks)
all_draft_picks$player <- unlist(lapply(all_draft_picks$player_id, function(x) players$full_name[match(x, players$player_id)]))
all_draft_picks$owner <- unlist(lapply(all_draft_picks$owner, function(x) managers$manager[match(x, managers$owner_id)]))
all_draft_picks[is.na(owner) | original_owner == "chip", owner := "JCraft1"]

all_draft_picks[, asset_id := paste(original_owner, season, round, sep = "_")]

all_trans[asset == "Christian McCaffrey",]
all_draft_picks[player == "Christian McCaffrey",]


all_transactions <- combine_transactions(all_trans, all_draft_picks)


asset_search <- "Christian McCaffrey"
all_transactions[asset == asset_search,]

trans_id <- "641044574399209472"
owner_id <- all_transactions[transaction_id == trans_id & asset == asset_search,]$owner_from

all_transactions[transaction_id == trans_id & owner_from == owner_id,]

keep_searching <- T
asset_search <- "Christian McCaffrey"
trans_id <- "641044574399209472"

assets_sent <- c()
assets_received <- c()
assets_to_search <- data.frame(asset = asset_search, transaction_id = trans_id)
sub_trades <- list()

while(keep_searching) {

  current_asset <- assets_to_search[1,1]
  print(current_asset)
  current_trans <- assets_to_search[1,2]

  trans_date <- all_transactions[transaction_id == current_trans & asset == current_asset,]$transaction_date

  owner_id <- all_transactions[transaction_id == current_trans & asset == current_asset,]$owner_from

  sent <- all_transactions[transaction_id == current_trans & owner_from == owner_id,]
  received <- all_transactions[transaction_id == current_trans & owner_to == owner_id,]

  if(length(grep("_", received$asset)) > 0) {
    picks_search <- received[grep("_", received$asset),]
    picked_players <- c()

    for(i in 1:nrow(picks_search)) {
      picked_players <- c(picked_players,
                          all_draft_picks[original_owner == strsplit(picks_search$asset[i], "_")[[1]][1] &
                                            season == strsplit(picks_search$asset[i], "_")[[1]][2] &
                                            round == strsplit(picks_search$asset[i], "_")[[1]][3],]$player)


    }

  }
  received_search <- c(received$asset, picked_players)

  assets_sent <- c(assets_sent, sent$asset)
  assets_received <- c(assets_received, received$asset)

  if(nrow(all_transactions[asset %in% received_search & transaction_date > trans_date & owner_from == owner_id,]) > 0) {
    ## add new assets to search for
    temp <- all_transactions[asset %in% received_search & transaction_date > trans_date & owner_from == owner_id,]
    assets_to_search <- rbind(assets_to_search, temp[, c("asset", "transaction_id")])
    ## remove current asset
    assets_to_search <- unique(assets_to_search[-1,])
    ## get all subsequent trades of assets to search
    sub_trades[[length(sub_trades) + 1]] <- all_transactions[transaction_id %in% temp$transaction_id,]
  } else {
    assets_to_search <- assets_to_search[-1,]
  }

  print(assets_to_search)

  if(nrow(assets_to_search) == 0) {
    keep_searching <- F
  }

}


unique(assets_sent)
unique(assets_received)

gained <- as.data.table(data.frame(asset = unique(assets_received[!(assets_received %in% assets_sent)])))

gained <- merge(gained, all_draft_picks[, c("asset_id", "player")], by.x = "asset", by.y = "asset_id", all.x = T)
gained[!is.na(player), asset := player]
gained$player <- NULL

gained <- gained[!(asset %in% assets_sent),]


