library(rvest)
library(data.table)
library(RSelenium)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(future)
library(future.apply)
plan(multiprocess)

pt <- proc.time()

source("R/align_functions.r")
source("R/scrape_functions.r")
source("R/scrape_matches_fn.r")
source("R/utility_fn.r")

monday_url = "https://www.bet365.com.au/#/AC/B1/C1/D13/E40/F137/"

b365_matches = get_matches_odds(monday_url,"bet365")
bf_matches = get_matches_odds("https://www.betfair.com.au/exchange/plus/football","betfair")

# merge the two together --------------------------------------------------
b365_matches[,bet365id:=1:.N]
bf_matches[,bfid:=1:.N]

perfect_matches = merge(b365_matches, bf_matches, by = c("HomeTeam","AwayTeam"))
perfect_matches

perfect_matches_arbi = perfect_matches[odds_1.x > odds_1_lay | odds_x.x > odds_x_lay | odds_2.x > odds_2_lay]
perfect_matches_arbi


b365_matches_unmatched = b365_matches[!bet365id %in% perfect_matches$bet365id,.(team1_b365=HomeTeam, team2_b365=AwayTeam, odds_1, odds_x, odds_2, bet365id)]
betfair_unmatched = bf_matches[!bfid %in% res4$bfid,.(team1_bf=HomeTeam, team2_bf=AwayTeam, odds_1_lay, odds_x_lay, odds_2_lay, bfid)]

b365_matches_unmatched[,justmerge:=T]
betfair_unmatched[,justmerge:=T]

unmatched_b365_bf = merge(b365_matches_unmatched, betfair_unmatched, by="justmerge", allow.cartesian = T)

unmatched_b365_bf[,dist:= (odds_1-odds_1_lay)^2+(odds_x-odds_x_lay)^2+(odds_2-odds_2_lay)^2]

# find the one with the closest odds --------------------------------------
unmatched_b365_bf[order(dist),.(team1_b365, team1_bf, team2_b365, team2_bf, bet365id, bfid)]

setnames(b365_matches_unmatched,"team1_b365","team1")
setnames(betfair_unmatched,"team1_bf","team1")

team1_merged = merge(
  b365_matches_unmatched
  , betfair_unmatched
  , by="team1")

reject_team1 <- function(bet365id1, bfid1) {
  team1_merged <<- team1_merged[!(bet365id==bet365id1 & bfid==bfid1)]
  print(team1_merged[,.(team1, team2_b365, team2_bf, bet365id, bfid)])
}

team1_merged[,.(team1, team2_b365, team2_bf, bet365id, bfid)]

setnames(b365_matches_unmatched,"team1", "team1_b365")
setnames(betfair_unmatched,"team1", "team1_bf")

setnames(b365_matches_unmatched,"team2_b365","team2")
setnames(betfair_unmatched,"team2_bf","team2")

team2_merged = merge(
  b365_matches_unmatched
  , betfair_unmatched
  , by="team2");team2_merged

reject_team2 <- function(bet365id1, bfid1) {
  team1_merged <<- team1_merged[!(bet365id==bet365id1 & bfid==bfid1)]
  print(team1_merged[,.(team2, team1_b365, team1_bf, bet365id, bfid)])
}

team2_merged[,.(team2, team1_b365, team1_bf, bet365id, bfid)]

fst::write.fst(team1_merged,paste0("team1_merged",Sys.Date(),".fst"))
fst::write.fst(team2_merged,paste0("team2_merged",Sys.Date(),".fst"))

# still unmatched
b365_still_unmatched = b365_matches_unmatched[!bet365id  %in% c(team1_merged$bet365id, team2_merged$bet365id)]
bf_still_unmatched = betfair_unmatched[!bfid  %in% c(team1_merged$bfid, team2_merged$bfid)]

setnames(b365_still_unmatched, "team2","team2_b365")
setnames(bf_still_unmatched, "team2","team2_bf")
# try to match by one word?

b365bf = merge(b365_still_unmatched,bf_still_unmatched, by="justmerge",allow.cartesian = T)

one_in_the_other = function(x, y) {
  x = str_split(x," ",simplify = T) %>% setdiff(c("U20","U23","W","Women","FC","FK"))
  y = str_split(y," ",simplify = T) %>% setdiff(c("U20","U23","W","Women","FC","FK"))
  length(intersect(x,y))
}

b365bf[,ok := mapply(one_in_the_other, team1_b365, team1_bf)]
b365bf[,ok2 := mapply(one_in_the_other, team2_b365, team2_bf)]
one_word_teams1 = b365bf[ok>=1 | ok2>=1,.(team1_b365, team1_bf, team2_b365, team2_bf, bet365id, bfid)]
print(one_word_teams1)

reject_team1word <- function(bet365id1, bfid1) {
  one_word_teams1 <<- one_word_teams1[!(bet365id==bet365id1 & bfid==bfid1)]
  print(one_word_teams1)
}

if(F) {
  reject_team1word(15,45)
  reject_team1word(22,21)
  reject_team1word(32,45)
  reject_team1word(64,13)
  reject_team1word(64,16)
  reject_team1word(7,25)
  reject_team1word(8,45)
  reject_team1word(18,16)
  reject_team1word(33,34)
  reject_team1word(33,39)
  reject_team1word(52,17)
  reject_team1word(52,32)
  reject_team1word(86,13)
  reject_team1word(112,41)

  fst::write.fst(one_word_teams1,paste0("one_word_teams1_",Sys.time() %>% str_replace_all(":","_"),".fst"))
}



