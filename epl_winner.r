library(rvest)
library(data.table)
library(RSelenium)
library(stringr)
library(dplyr)
library(tidyr)

dr = RSelenium::rsDriver()

drc = dr$client

source("align_functions.r")


# Bet365 B365 -------------------------------------------------------------


drc$navigate("https://www.bet365.com.au/#/AC/B1/C172/D1/E36041244/F2")
drc$navigate("https://www.bet365.com.au/#/AC/B1/C172/D1/E36041244/F2")
Sys.sleep(2)
# typically these are odds that exists as team odds
get_b365_winner_odds <- function() {
  team = drc$executeScript("
  return Array.from(document.body.querySelectorAll('div.cm-CouponMarketGroup_Open span.gl-Participant_Name')).map((x) => x.innerHTML)
", args=list("dummy")) %>% unlist
  
  
  odds = drc$executeScript("
  return Array.from(document.body.querySelectorAll('div.cm-CouponMarketGroup_Open span.gl-Participant_Odds')).map((x) => x.innerHTML)
", args=list("dummy")) %>% unlist %>% as.numeric
  
  data.table(selection_b365 = team, odds_b365 = odds, company_code = "b635", market = "winner", extraction_timestamp = Sys.time())
}

b365_winner_odds = get_b365_winner_odds()


# William Hill WH ---------------------------------------------------------
get_wh_winner_odds <- function() {
  drc$navigate("https://www.williamhill.com.au/sports/soccer/british-futures/82128481/english-premier-league-futures")
  Sys.sleep(2)

  market = drc$executeScript("
    wh_games = Array.from(document.body.querySelectorAll('div.Market_market_2m7'));
    nteams_or_options = Array.from(wh_games.map((x)=>x.querySelectorAll('button').length))
    game_type = wh_games.map((x) => x.querySelector('h3').innerText)
    return {nteams_or_options, game_type}
                                 ", args = list("dummy"))
  
  market = lapply(market, unlist) %>% as.data.table
  # [1] "2018/19 English Premier League - Winner"           
  # [2] "2018/19 English Premier League - Top 4"            
  # [3] "2018/19 English Premier League - Top 6"            
  # [4] "2018/19 English Premier League - Relegation"       
  # [5] "2018/19 English Premier League - Top Half Finish"  
  # [6] "2018/19 English Premier League - Top Goalscorer"   
  # [7] "2018/19 English Premier League - Top Promoted Club"
  
  # for each game type, count how many teams/options are involved
  team = drc$executeScript("
    return Array.from(document.body.querySelectorAll('div.Market_market_2m7 span.BetButton_competitorName_DQT.Outcomes_competitor_3IY')).map((x) => x.innerHTML)
                            ", args=list("dummy")) %>% unlist
  
  odds = drc$executeScript("
  return Array.from(document.body.querySelectorAll('div.Market_market_2m7 span.BetButton_display_3ty')).map((x) => x.innerHTML)
", args=list("dummy")) %>% unlist %>% as.numeric
  
  res = data.table(selection_wh = team, odds_wh = odds, market_company = rep(market$game_type, market$nteams_or_options), company_code = "wh")
  
  
  wh_determine_game_type <- function(wh_game_type) {
    # intermediate is used to extract the last string as WH's tables often start with information like
    # "2018/19 English Premier League - Top Promoted Club"
    intermediate = sapply(wh_game_type, function(wh_game_type) {
      stringr::str_extract(wh_game_type, "(?<=- )[[:alnum:]|[:space:]]+$")
    })
  }
  
  res[,market := wh_determine_game_type(market_company) %>% tolower()]
  res[,extraction_timestamp := Sys.time()]
  
  res[,selection_b365 := align_team2b365(selection_wh)]
  res
}

wh_winner_odds = get_wh_winner_odds()


# Crownbet CB -------------------------------------------------------------
get_cb_winner_odds <- function() {
  drc$navigate("https://crownbet.com.au/sports-betting/soccer/united-kingdom/english-premier-league-futures/outright-markets-20180810-948197-28398867")
  Sys.sleep(2)
  
  # need to click to each market to open them
  # this seems to be a good way to open up all the hidden tables containing
  elems = drc$findElements("div.sports-event-wrap div.middle-section:not(.match-list)", using = "css")
  l = length(elems)
  for(i in 1:length(elems)) {
    elems = drc$findElements("div.sports-event-wrap div.middle-section:not(.match-list)", using = "css")
    elems[[i]]$clickElement()
    Sys.sleep(2)
  }
  
  # this contains all the table in match-form but does not capture tables that contain a team name and multiple odds representing multiple markets
  tmp = drc$executeScript("
    return Array.from(document.body.querySelectorAll('div.sports-event-wrap div.middle-section:not(.match-list) table.hidden-xs')).map(x=>x.outerHTML)
  ", args = list("dummy"))
  
  tmp_market = drc$executeScript('
 return Array.from(document.body.querySelectorAll("div.header div.title.multiple-events")).map(x=>x.querySelector("span").innerText)
                                 ', args=list("dummy")) %>% unlist
  
  # tmp1 = tmp[[4]]
  res1  = mapply(function(tmp1, tmp_market1) {
    res = tmp1 %>% read_html() %>% html_node("table") %>% html_table(fill = T) %>% data.table
    
    nr = names(res)
    lnr = length(nr)
    # this must be one of those top goal scorer tables that is organised in multiple columns: scorer, odds, goal scorer, odds ..
    if(lnr > 2) {
      tmp2 = expand.grid(c("selection","odds"),1:(lnr/2))
      new_names = paste0(tmp2$Var1,tmp2$Var2)
      setnames(res, names(res), new_names)
      
      tmp_rbind_tbl = res[,c("selection1","odds1")]
      setnames(tmp_rbind_tbl, names(tmp_rbind_tbl),c("selection","odds_cb"))
        
      for(i in 2:(lnr/2L)) {
        tmp_rbind_tbl2append = res[,paste0(c("selection","odds"),i), with = F]
        setnames(tmp_rbind_tbl2append, names(tmp_rbind_tbl2append), c("selection","odds_cb"))
        tmp_rbind_tbl = rbindlist(list(tmp_rbind_tbl, tmp_rbind_tbl2append), use.names = T, fill = T)
      }
      warning("ZJ msg: not checked whether removing NAs are safe")
      res = tmp_rbind_tbl[!is.na(odds_cb)]
    } else {
      setnames(res, names(res), c("selection","odds_cb"))
    }
    
    res[,company_code := "cb"]
    res[,market_company := tmp_market1]
    setnames(res, "selection","selection_company")
    
    #res[,selection_b365 := align_team2b365(selection_company)]
  }, tmp, tmp_market, SIMPLIFY = F) %>% rbindlist(use.names = T, fill = T)
  
  
  # now extract the table with one team name and multiple odds columns
  tmp_multi_odds_col = drc$executeScript("
    return Array.from(document.body.querySelectorAll('div.event-summary-table.sport-block.sport-event.hidden-xs')).map(x => x.outerHTML)
  ", args = list("dummy")) %>% lapply(function(tmp_multi_odds_col1) {
    tmp1 = tmp_multi_odds_col1 %>% read_html() %>% html_node("table") %>% html_table(fill = T) %>% data.table
    tmp1 %>% gather(key = market_company, value = odds, -Selection)
  })
  
  # there should only be one table
  stopifnot(length(tmp_multi_odds_col) == 1)
  
  tmp_multi_odds_col_fnl = as.data.table(tmp_multi_odds_col[[1]])
  
  tmp_multi_odds_col_fnl[,market:= align_market(market_company)]
  tmp_multi_odds_col_fnl[startsWith(market_company,"Winner"), market:="winner"]
  
  setnames(tmp_multi_odds_col_fnl,c("Selection","odds"),c("selection_company","odds_cb"))
  tmp_multi_odds_col_fnl
  res2 = rbindlist(list(res1, tmp_multi_odds_col_fnl), use.names = T, fill = T)
  
  res2[,selection_b365 := align_team2b365(selection_company)]
  res2[,extract_timestampe := Sys.time()]
  res2
}

cb_winner_odds = get_cb_winner_odds()

setdiff(cb_winner_odds$selection_company, b365_winner_odds$selection_b365)
setdiff(b365_winner_odds$selection_b365, cb_winner_odds$selection_company)
setdiff(b365_winner_odds$selection_b365, cb_winner_odds$selection_b365)

# Neds NED--------------------------------------------------------------------
get_neds_odds_generic <- function(url, market) {
  drc$navigate(url)
  Sys.sleep(2)
  
  team = drc$executeScript("
                           return Array.from(document.body.querySelectorAll('span.entrant-name')).map(x=>x.innerText)
                           ", args=list("dummy")) %>% unlist
  
  
  odds = drc$executeScript("
                           return Array.from(document.body.querySelectorAll('button.entrant-odds span')).map(x=>x.innerText)
                           ", args=list("dummy")) %>% unlist %>% as.numeric
  
  data.table(selection_company = team, odds_neds = odds, company_code = "neds", market = market, extraction_timestamp = Sys.time())
}

get_neds_winner_odds <- function() {
  res = get_neds_odds_generic("https://www.neds.com.au/sports/future/soccer/2018-19-england-premier-league/678443b0-0b08-4cee-b408-effc56d1b107","winner")
  res[,selection_b365:=align_team2b365(selection_company)]
  res
}

neds_winner_odds = get_neds_winner_odds()

setdiff(neds_winner_odds$selection_company, b365_winner_odds$selection_b365)
setdiff(b365_winner_odds$selection_b365, neds_winner_odds$selection_company)
setdiff(b365_winner_odds$selection_b365, neds_winner_odds$selection_b365)


res = merge(b365_winner_odds, wh_winner_odds, by = c("selection_b365","market")) %>% 
  merge(cb_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>% 
  merge(neds_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>% 
  arrange(odds_b365); res
