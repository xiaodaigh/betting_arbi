library(rvest)
library(data.table)
library(RSelenium)

dr = RSelenium::rsDriver()

drc = dr$client

drc$navigate("https://www.bet365.com.au/home/")
Sys.sleep(10)
drc$navigate("https://www.bet365.com.au/#/AC/B1/C1/D13/E40/F137/S1/")
Sys.sleep(5)

# odds = drc$executeScript(
#   "
#     elems = document.body.querySelectorAll('span.gl-ParticipantOddsOnly_Odds');
#     var arr = [];
#     for(var i = elems.length; i--; arr.unshift(elems[i].innerText));
#     return arr;
#   
#   ", args=list("dummy")) 

odds2 = drc$executeScript(
"
  res = document.body.querySelectorAll('div.gl-MarketGroupContainer.gl-MarketGroupContainer_HasLabels')
  resa = Array.from(res)
  
  function extract_odds_from_bet365_tbl(x) {
    y = x.querySelectorAll('span.gl-ParticipantOddsOnly_Odds')
    ytext = Array.from(y).map((y) => y.innerText)
    table_row_length = y.length/3

    ytext_copy = ytext.slice(0)

    for(i = 0; i < table_row_length; i++) {
      ytext_copy[i*3] = ytext[i]
      ytext_copy[i*3+1] = ytext[table_row_length+i]
      ytext_copy[i*3+2] = ytext[2*table_row_length+i]
    }
    return ytext_copy
  }

  return resa.reduce((x,y)=>x.concat(extract_odds_from_bet365_tbl(y)),[])
", args=list("dummy"))

odds = odds2 %>% unlist %>% as.numeric()

team_vs = drc$executeScript(
  "
    elems = document.body.querySelectorAll('div.sl-CouponParticipantWithBookCloses_Name');
    var arr = [];
    for(var i = elems.length; i--; arr.unshift(elems[i].innerText));
    return arr;
  "
, args = list("dummy"))

team_vs = team_vs %>% unlist

name.rex <- "(?<team1>.*)\ v\ (?<team2>.*)"
(parsed <- regexpr(name.rex, team_vs, perl = TRUE))

parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  if(identical(dim(m), c(1,1))) { # if it was not able to parse townname or post code then it will have dim=c(1,1)
    colnames(m) <- attr(result, "capture.names")
  }
  
  m
}
res = parse.one(team_vs, parsed)

# every null are in pairs
res1 = res[-which(res[,1]=="")[c(T,F)],]

res2 = data.table(res1)
setnames(res2, names(res2), c("team1","team2"))
res2[,odds_1 := odds[c(T,F,F)]]
res2[,odds_x := odds[c(F,T,F)]]
res2[,odds_2 := odds[c(F,F,T)]]
res2

# scrape from betfair -----------------------------------------------------
drc$navigate("https://www.betfair.com.au/exchange/plus/football")
Sys.sleep(5)
get_betfair_tbl <- function() {
  betfair_tbl = drc$executeScript("return document.body.querySelector('table.coupon-table');", args = list("dummy"))
  
  betfair_tbl = drc$executeScript(
    "
  res = document.body.querySelectorAll('table.coupon-table');
  var arr = [];
  for(var i = res.length; i--; arr.unshift(res[i].outerHTML));
  return arr; 
  ", args = list("dummy"))
  
  betfair_tbl1 = betfair_tbl[[1]] %>% read_html %>% html_node("table") %>% rvest::html_table(header = T)
  
  
  betfair_teams = drc$executeScript(
    "
  res = document.body.querySelectorAll('ul.runners li');
  var arr = [];
  for(var i = res.length; i--; arr.unshift(res[i].innerText));
  return arr; 
  ", args = list("dummy"))
  betfair_teams1 = betfair_teams %>% unlist
  
  
  betfair_odds = drc$executeScript(
    "
  res = document.body.querySelectorAll('div.bf-bet-button-info span.bet-button-price');
  var arr = [];
  for(var i = res.length; i--; arr.unshift(res[i].innerText));
  return arr; 
  ", args = list("dummy"))
  
  
  betfair_odds1 = betfair_odds %>% unlist %>% as.numeric
  
  betfair_tbl = data.table(
    team1 = betfair_teams1[c(T,F)]
    ,team2 = betfair_teams1[c(F,T)]
    ,odds_bf_lay_1 = betfair_odds1[c(F,T,F,F,F,F)]
    ,odds_bf_lay_x = betfair_odds1[c(F,F,F,T,F,F)]
    ,odds_bf_lay_2 = betfair_odds1[c(F,F,F,F,F,T)]
  )
  betfair_tbl
}

# betfair_tbl1 = get_betfair_tbl()
# betfair_tbl2 = get_betfair_tbl()
# betfair_tbl3 = get_betfair_tbl()
# betfair_tbl4 = get_betfair_tbl()
# betfair_tbl5 = get_betfair_tbl()
# 
# betfair_tbl = rbindlist(list(betfair_tbl1, betfair_tbl2,betfair_tbl3,betfair_tbl4,betfair_tbl5))

betfair_tbl = get_betfair_tbl()

# merge the two together --------------------------------------------------
res3 = res2[team1 != ""]
res3[,bet365id:=1:.N]
betfair_tbl[,bfid:=1:.N]

res4 = merge(res3, betfair_tbl, by = c("team1","team2"))
res4

res5 = res4[odds_1 > odds_bf_lay_1 | odds_x > odds_bf_lay_x | odds_2 > odds_bf_lay_2]
res5


res3_unmatched = res3[!bet365id %in% res4$bet365id,.(team1_b365=team1, team2_b365=team2, odds_1, odds_x, odds_2, bet365id)]
betfair_unmatched = betfair_tbl[!bfid %in% res4$bfid,.(team1_bf=team1, team2_bf=team2, odds_bf_lay_1, odds_bf_lay_x, odds_bf_lay_2, bfid)]

res3_unmatched[,justmerge:=T]
betfair_unmatched[,justmerge:=T]

unmatched_b365_bf = merge(res3_unmatched, betfair_unmatched, by="justmerge", allow.cartesian = T)

unmatched_b365_bf[,dist:= (odds_1-odds_bf_lay_1)^2+(odds_x-odds_bf_lay_x)^2+(odds_2-odds_bf_lay_2)^2]

# find the one with the closest odds --------------------------------------
unmatched_b365_bf[order(dist),.(team1_b365, team1_bf, team2_b365, team2_bf, bet365id, bfid)]

setnames(res3_unmatched,"team1_b365","team1")
setnames(betfair_unmatched,"team1_bf","team1")

team1_merged = merge(
  res3_unmatched
  , betfair_unmatched
  , by="team1")

reject_team1 <- function(bet365id1, bfid1) {
  team1_merged <<- team1_merged[!(bet365id==bet365id1 & bfid==bfid1)]
  print(team1_merged[,.(team1, team2_b365, team2_bf, bet365id, bfid)])
}

team1_merged[,.(team1, team2_b365, team2_bf, bet365id, bfid)]

setnames(res3_unmatched,"team1", "team1_b365")
setnames(betfair_unmatched,"team1", "team1_bf")

setnames(res3_unmatched,"team2_b365","team2")
setnames(betfair_unmatched,"team2_bf","team2")

team2_merged = merge(
  res3_unmatched
  , betfair_unmatched
  , by="team2")

reject_team2 <- function(bet365id1, bfid1) {
  team1_merged <<- team1_merged[!(bet365id==bet365id1 & bfid==bfid1)]
  print(team1_merged[,.(team2, team1_b365, team1_bf, bet365id, bfid)])
}

team2_merged[,.(team2, team1_b365, team1_bf, bet365id, bfid)]

fst::write.fst(team1_merged,paste0("team1_merged",Sys.Date(),".fst"))
fst::write.fst(team2_merged,paste0("team2_merged",Sys.Date(),".fst"))



