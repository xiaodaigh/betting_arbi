
# Bet365 B365 -------------------------------------------------------------
get_b365_odds <- function(url, market) {
  drc$navigate(url)
  drc$navigate(url)
  Sys.sleep(2)
  team = drc$executeScript("
  return Array.from(document.body.querySelectorAll('div.cm-CouponMarketGroup_Open span.gl-Participant_Name')).map((x) => x.innerHTML)
", args=list("dummy")) %>% unlist
  
  
  odds = drc$executeScript("
  return Array.from(document.body.querySelectorAll('div.cm-CouponMarketGroup_Open span.gl-Participant_Odds')).map((x) => x.innerHTML)
", args=list("dummy")) %>% unlist %>% as.numeric
  
  data.table(selection_b365 = team, odds_b365 = odds, company_code = "b635", market = market, extraction_timestamp = Sys.time())
}

get_b365_winner_odds <- function(league = c("epl","csl","sweden1","sweden2")) {
  match.arg(league)
  if(league == "epl") {
    get_b365_odds("https://www.bet365.com.au/#/AC/B1/C172/D1/E36041244/F2", "winner")
  } else if(league == "csl") {
    get_b365_odds("https://www.bet365.com.au/?rn=49941906763&stf=1#/AC/B1/C172/D1/E36320493/F2", "winner")
  } else if(league == "sweden1") {
    get_b365_odds("https://www.bet365.com.au/?rn=49941906763&stf=1#/AC/B1/C172/D1/E36409867/F2", "winner")
  } else if(league == "sweden2") {
    get_b365_odds("https://www.bet365.com.au/?rn=49941906763&stf=1#/AC/B1/C172/D1/E36411048/F2", "winner")
  }
}

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
      if(wh_game_type=="Winner") {
        return("winner")
      }
      #market = stringr::str_extract(wh_game_type, "(?<=- )[[:alnum:]|[:space:]]+$") %>% tolower()
      market = wh_game_type %>% tolower()
      if(market == "top 4") {
        return("top4")
      } else if(market == "top 6") {
        return("top6")
      } else if(market == "top half finish") {
        return("top10")
      } else if(market == "top promoted club") {
        return("top promoted")
      } else {
        return(market)
      }
    })
  }
  
  # browser()
  
  res[,market := wh_determine_game_type(market_company)]
  res[,extraction_timestamp := Sys.time()]
  
  res[,selection_b365 := align_team2b365(selection_wh)]
  res
}

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
  
  res2[,odds_cb:=as.numeric(odds_cb)]
  res2[,selection_b365 := align_team2b365(selection_company)]
  res2[,extract_timestampe := Sys.time()]
  res2
}

# ubet UBET ---------------------------------------------------------------
get_ubet_odds <- function() {
  drc$navigate("https://ubet.com/sports/soccer/england-premier-league/premier-league-futures/england-premier-league-futures-2018-19")
  Sys.sleep(2)
  
  res = drc$executeScript("
  markets = Array.from(document.body.querySelectorAll('div.border.subevent'))
  markets_lbl = markets.map(market => market.querySelector('div.main-event-text.truncate.with-subtitle').innerText)
  
  selection = markets.map(
    market => Array.from(market.querySelectorAll('div.offer-name span.truncate')).map(x=>x.innerText))
  
  odds = markets.map(
    market => Array.from(market.querySelectorAll('div.offer-odds span.odds')).map(x=>x.innerText))
  
  return {markets_lbl, selection, odds}
                           ", args=list("dummy")) 
  
  res1 = mapply(function(markets_lbl, selection, odds) {
    if(length(selection %>% unlist) != length(odds %>% unlist)) {
      warning("ubet: one of the odds is suspended")
    }
    
    data.table(
      selection_company = selection %>% unlist,
      odds_ubet = odds %>% unlist %>% as.numeric,
      market_company = markets_lbl)
    
    
  }, res$markets_lbl, res$selection, res$odds, SIMPLIFY = F) %>% 
    rbindlist(use.names = T, fill = T)
  
  
  res1[,selection_b365 := align_team2b365(selection_company)]
  res1[,market := align_market(market_company)]
  res1[,company_code := "ubet"]
  
  res1
}

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

# Ladbrokes LAD -----------------------------------------------------------
get_lad_winner_odds <- function() {
  lad_epl_url = "https://www.ladbrokes.com.au/sports/soccer/56132030-premier-league-2018-19/56132030-premier-league-2018-19/"
  drc$navigate(lad_epl_url)
  Sys.sleep(2)
  
  tbl = drc$executeScript("
    return document.body.querySelector('table.bettype-group').outerHTML
                          ", args = list("dummy")) %>% 
    unlist %>% 
    read_html %>% 
    html_node("table") %>% 
    html_table %>% 
    as.data.table
  
  # the first two rows are useless info
  tbl = tbl[-(1:2),]
  
  setnames(tbl, names(tbl), c("selection_company","odds_lad"))
  
  tbl[,odds_lad:=as.numeric(odds_lad %>% str_remove_all(","))]
  
  tbl[,market:="winner"]
  tbl[,extraction_timestamp := Sys.time()]
  
  tbl[,selection_b365 := align_team2b365(selection_company)]
  tbl
}

# sportsbet SB ------------------------------------------------------------
get_sb_winner_odds <- function() {
  drc$navigate("https://www.sportsbet.com.au/betting/soccer/united-kingdom/english-premier-league?MyLinks")
  Sys.sleep(2)
  
  outright_tab = drc$findElement("#tab-outrights", using = "css")
  outright_tab$clickElement()
  Sys.sleep(2)
  
  team = drc$executeScript("
    return Array.from(document.body.querySelectorAll('span.team-name.ib')).map(x=>x.innerText)
                          ", args = list("dummy")) %>% unlist
  
  odds = drc$executeScript("
    return Array.from(document.body.querySelectorAll('span.odd-val.ib.right')).map(x=>x.innerText)
                          ", args = list("dummy")) %>% unlist %>% as.numeric
  
  res = data.table(selection_company = team, odds_sb = odds, extraction_timestamp = Sys.time(), market="winner")
  res[,selection_b365 := align_team2b365(selection_company)]
  res[!is.na(odds_sb),]
}

# unibet UNI ------------------------------------------------------------
get_uni_winner_odds <- function(league = c("epl","csl","sweden1","sweden2")) {
  match.arg(league)
  if(league == "epl") {
    drc$navigate("https://www.unibet.com.au/betting#event/1004799423")
  } else if (league == "csl") {
    drc$navigate("https://www.unibet.com.au/betting#event/1004502604")
  } else if (league == "sweden1") {
    drc$navigate("https://www.unibet.com.au/betting#event/1004330016")
  } else if (league == "sweden2") {
    drc$navigate("https://www.unibet.com.au/betting#event/1004426006")
  }
  Sys.sleep(2)
  
  res = drc$executeScript("
                          markets = Array.from(document.body.querySelectorAll('div.KambiBC-bet-offer-subcategory__container'))
                          
                          return markets.map(function(market) {
                          columns = Array.from(market.querySelectorAll('div.KambiBC-outcomes-list__column'))
                          
                          return columns.reduce((accumulator, currentValue, currentIndex) => {
                          if (currentIndex == 0) {
                          
                          teams = Array.from(currentValue.querySelectorAll('span.KambiBC-outcomes-list__label span')).map(x=>x.textContent)
                          accumulator.push({teams})
                          return accumulator
                          } else {
                          
                          odds = Array.from(currentValue.querySelectorAll('span.KambiBC-mod-outcome__odds')).map(x=>x.innerText)
                          
                          accumulator.push({odds})
                          return accumulator
                          }
                          },[])
                          })
                          ", args = list("dummy")) %>% 
    lapply(function(tbl_as_lists) {
      lapply(tbl_as_lists, function(vector_as_list) vector_as_list %>% unlist) %>% as.data.table
    })
  
  
  # this part is a bit manual but doesn't seem to be better way
  if(league == "epl") {
    stopifnot(length(res) == 4)
    setnames(res[[1]],names(res[[1]]), c("selection_company","winner","top4","top6","top10"))
    setnames(res[[2]],names(res[[2]]), c("selection_company","winner_wo_top6"))
    setnames(res[[3]],names(res[[3]]), c("selection_company","winner_wo_city"))
    setnames(res[[4]],names(res[[4]]), c("selection_company","winner_wo_city_liv"))
    
    res_all = res[[1]] %>% 
      merge(res[[2]], by = "selection_company", all =T) %>% 
      merge(res[[3]], by = "selection_company", all =T) %>% 
      merge(res[[4]], by = "selection_company", all =T) %>% 
      gather(key="market", value = "odds_uni", -c("selection_company")) %>% 
      mutate(selection_b365 = align_team2b365(selection_company), extraction_timestamp = Sys.time(),
             odds_uni = as.numeric(odds_uni)) %>% 
      as.data.table
    return(res_all)
  } else if (league%in%c("sweden1","sweden2")) {
    #browser()
    # this one has winner and top 3
    stopifnot(length(res) == 2)
    warning("there are multiple sweden markets")
    res = res[[1]] %>% as.data.table
    setnames(res, names(res), c("selection_company","winner","top3"))
    res = res %>% 
      gather(key = "market", value = "odds_uni", -selection_company) %>% 
      mutate(selection_b365 = align_team2b365(selection_company), extraction_timestamp = Sys.time(),
             odds_uni = as.numeric(odds_uni))
    return(res)
  } else {
    # all other leagues should only have winner, so only one market
    stopifnot(length(res) == 1)
    res = res[[1]] %>% as.data.table
    setnames(res, names(res), c("selection_company","odds_uni"))
    res = res %>% 
      mutate(selection_b365 = align_team2b365(selection_company), extraction_timestamp = Sys.time(),
              odds_uni = as.numeric(odds_uni), market="winner")
    return(res)
  }
}

# TAB tab -----------------------------------------------------------------

get_tab_winner_odds <- function(league=c("epl","csl")) {
  match.arg(league)
  #browser()
  if(league == "epl") {
    drc$navigate("https://www.tab.com.au/sports/betting/Soccer/competitions/English Premier League Futures")
  } else if(league == "csl") {
    drc$navigate("https://www.tab.com.au/sports/betting/Soccer/competitions/China Super League/matches/China Super League Winner")
    match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))[[1]] %>% read_html
    odds = match_odds_html %>% 
      html_nodes("div.animate-odd") %>% 
      html_text %>% 
      as.numeric
    
    selection = match_odds_html %>% 
      html_nodes("div.proposition-title") %>% 
      html_text 
    
    res = data.table(selection_company=selection, odds_tab=odds, market="winner")
    res[,selection_b365 := align_team2b365(selection_company)]
    res[,extraction_timestamp := Sys.time()]
    setkey(res, odds_tab)
    return(res)
  }
  Sys.sleep(2)
  
  res = drc$executeScript("
    return document.body.querySelector('table.propositions-container').outerHTML
                          ", args = list("dummy")) %>% 
    unlist %>% 
    read_html %>% 
    html_node("table") %>% 
    html_table(fill=T) %>% 
    as.data.table
  
  res = res[-(1:2),]
  
  # this part is a bit manual but doesn't seem to be better way
  stopifnot(ncol(res) == 4)
  setnames(res,names(res), c("selection_company","winner","top4","relegation"))
  
  res_all = res %>% 
    gather(key="market",value = "odds_tab", -c(selection_company)) %>% 
    mutate(selection_b365 = align_team2b365(selection_company), extraction_timestamp = Sys.time()) %>% 
    as.data.table
  
  res_all
}

# Betfair BF --------------------------------------------------------------
get_bf_winner_odds <- function(league=c("epl","sweden1")) {
  match.arg(league)
  if(league == "epl") {
    drc$navigate("https://www.betfair.com.au/exchange/plus/football/market/1.142751842")
  } else if (league == "sweden1") {
    drc$navigate("https://www.betfair.com.au/exchange/plus/football/market/1.140511753")
  }
  
  Sys.sleep(4)
  
  selection = drc$executeScript("
    return Array.from(document.body.querySelectorAll('h3.runner-name')).map(x=>x.innerText)
                          ", args = list("dummy")) %>% 
    unlist
  
  odds = drc$executeScript("
    return Array.from(document.body.querySelectorAll('span.bet-button-price')).map(x=>x.innerText)
                          ", args = list("dummy")) %>% 
    unlist %>% as.numeric
  
  res = data.table(
    selection_company = selection,
    odds_back2 =  odds[c(T,F,F,F,F,F)],
    odds_back1 =  odds[c(F,T,F,F,F,F)],
    odds_bf  =  odds[c(F,F,T,F,F,F)],
    odds_lay   =  odds[c(F,F,F,T,F,F)],
    odds_lay1  =  odds[c(F,F,F,F,T,F)],
    odds_lay2  =  odds[c(F,F,F,F,F,T)]
  )
  
  # this part is a bit manual but doesn't seem to be better way
  stopifnot(ncol(res) == 7)
  
  res_all = res %>% 
    mutate(selection_b365 = align_team2b365(selection_company),
           extraction_timestamp = Sys.time(),
           market="winner") %>% 
    as.data.table
  
  res_all
}

# bluebet BB --------------------------------------------------------------
get_bb_winner_odds <- function() {
  drc$navigate("https://www.bluebet.com.au/sports/Soccer/England/Premier-League-Futures/Premier-League-Futures-2018-19/246478/C18471")
  Sys.sleep(2)
  
  res = drc$executeScript("
                          markets = Array.from(document.body.querySelectorAll('div.flag-object.table-grid.push--bottom.border--top.border--left.ng-scope'))
                          
                          odds = markets.map(market => {
                          return Array.from(market.querySelectorAll('span.place-bet__odds')).map(x=>x.innerText)
                          })
                          
                          selection = markets.map(market => {
                          return Array.from(market.querySelectorAll('div.headline-wrap.ng-binding.ng-scope')).map(x=>x.innerText)
                          })
                          
                          return {odds, selection}
                          ", args = list("dummy"))
  
  res = mapply(function(odds, selection) {
    data.table(
      selection_company = selection %>% unlist,
      odds_bb = odds %>% unlist %>% as.numeric)
  }, res$odds, res$selection, SIMPLIFY = F)
  
  # this part is a bit manual but doesn't seem to be better way
  stopifnot(length(res) == 4)

  res[[1]][,market:="winner"]
  res[[2]][,market:="relegation"]
  res[[3]][,market:="top4"]
  res[[4]][,market:="top goalscorer"]
  
  
  
  res_all = res %>% 
    rbindlist(use.names = T, fill = T) %>% 
    mutate(selection_b365 = align_team2b365(selection_company),
           extraction_timestamp = Sys.time()) %>% 
    as.data.table
  
  res_all
}

