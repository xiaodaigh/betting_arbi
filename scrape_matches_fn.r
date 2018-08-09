bf_matches_odds <- function(url) {
  # url = "https://www.betfair.com.au/exchange/plus/football/competition/879931"
  drc$navigate(url)
  Sys.sleep(4)
  #drc$maxWindowSize()
  
  bf_csl_match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))
  
  teams = bf_csl_match_odds_html[[1]] %>%
    read_html %>%   
    html_nodes("ul.runners li") %>% 
    html_text
  
  odds = bf_csl_match_odds_html[[1]] %>%
    read_html %>%   
    html_nodes("span.bet-button-price") %>% 
    html_text %>% 
    as.numeric
  
  bf_csl_next_matches = data.table(
    HomeTeam = teams[c(T,F)] %>% align_team2b365, 
    AwayTeam = teams[c(F,T)] %>% align_team2b365, 
    odds_bf1 = odds[c(T,rep(F,5))],
    odds_bfx = odds[c(F,F,T,F,F,F)],
    odds_bf2 = odds[c(F,F,F,F,T,F)],
    odds_bf1_lay = odds[c(F,T,F,F,F,F)],
    odds_bfx_lay = odds[c(F,F,F,T,F,F)],
    odds_bf2_lay = odds[c(F,F,F,F,F,T)],
    company_code = "bf"
  )
  bf_csl_next_matches
}

b365_matches_odds <- function(url) {
  # url = "https://www.bet365.com.au/?rn=49941906763&stf=1#/AC/B1/C1/D13/E37844384/F2/"
  drc$navigate(url)
  drc$navigate(url)
  Sys.sleep(2)
  #drc$maxWindowSize()
  b365_csl_match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))[[1]] %>% 
    read_html
  
  selection = b365_csl_match_odds_html %>% 
    html_nodes("div.sl-CouponParticipantWithBookCloses_Name") %>% 
    html_text %>% 
    purrr::map(~stringr::str_split_fixed(.x, " v ",2)) %>% 
    unlist
  
  odds2 = b365_csl_match_odds_html %>% 
    html_nodes("div.sl-MarketCouponValuesExplicit33.gl-Market_General.gl-Market_PWidth-12-3333") %>% 
    map(~.x %>% 
          html_nodes("div.gl-ParticipantOddsOnlyDarker.gl-ParticipantOddsOnly.gl-Participant_General span.gl-ParticipantOddsOnly_Odds") %>% 
          html_text )%>% 
    as.data.table
  
  
  
  data.table(
    HomeTeam = selection[c(T,F)],
    AwayTeam = selection[c(F,T)],
    odds_1_b365 = odds2$V1 %>% as.numeric,
    odds_x_b365 = odds2$V2 %>% as.numeric,
    odds_2_b365 = odds2$V3 %>% as.numeric,
    company_code = "b365"
  )
}


sb_matches_odds <- function(url)   {
  # read sportsbet ----------------------------------------------------------
  drc$navigate(url)
  Sys.sleep(2)
  
  team = drc$executeScript("
                           return Array.from(document.body.querySelectorAll('span.team-name.ib')).map(x=>x.innerText)
                           ", args = list("dummy")) %>% unlist
  
  odds = drc$executeScript("
                           return Array.from(document.body.querySelectorAll('span.odd-val.ib.right')).map(x=>x.innerText)
                           ", args = list("dummy")) %>% unlist %>% as.numeric
  
  
  sb_next_match_odds = 
    data.table(
      HomeTeam = team[c(T,F,F)] %>% align_team2b365, 
      AwayTeam = team[c(F,F,T)] %>% align_team2b365,
      odds_1 = odds[c(T,F,F)],
      odds_x = odds[c(F,T,F)],
      odds_2 = odds[c(F,F,T)],
      company_code = "sb"
    )
  
  setkey(sb_next_match_odds,HomeTeam)
  sb_next_match_odds
}

oddsportal_matches_odds <- function(url) {
  drc$navigate(url)
  oddsportal_csl_match_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))
  
  odds = oddsportal_csl_match_html[[1]] %>% 
    read_html %>% 
    html_nodes("td.odds-nowrp a") %>% 
    html_text
  
  teams_vs = oddsportal_csl_match_html[[1]] %>%
    read_html %>% 
    html_nodes("table#tournamentTable tr td.name.table-participant a") %>% 
    html_text %>%
    { Filter(function(x) nchar(x)!=1, .) }
  
  teams_vs1 = teams_vs %>% 
    strsplit("-") %>% 
    sapply(function(x) stringr::str_trim(x, "both"))
  
  csl_next_matches = data.table(
    HomeTeam = teams_vs1[1,] %>% align_team2b365, 
    AwayTeam = teams_vs1[2,] %>% align_team2b365, 
    odds_1 = odds[c(T,F,F)] %>% as.numeric,
    odds_x = odds[c(F,T,F)] %>% as.numeric,
    odds_2 = odds[c(F,F,T)] %>% as.numeric,
    company_code = "op"
  )
  
  setkey(csl_next_matches,HomeTeam)
  csl_next_matches
}

wh_matches_odds <- function(url) {
  #url ="https://www.williamhill.com.au/sports/soccer/asia/chinese-super-league"
  drc$navigate(url)
  Sys.sleep(2)
  match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))[[1]] %>% read_html
  
  selection = match_odds_html %>% 
    html_nodes("div.SoccerEventContent_header_1i4 div") %>% 
    html_text
  
  odds = match_odds_html %>% 
    html_nodes("div.SportEvent_sportEvent_27P button span.BetButton_display_3ty") %>% 
    html_text %>% 
    as.numeric
  
  data.table(
    HomeTeam = selection[c(T,F,F,F)] %>% align_team2b365,
    AwayTeam = selection[c(F,F,T,F)] %>% align_team2b365,
    odds_1 = odds[c(T,F,F,F,F)],
    odds_x = odds[c(F,T,F,F,F)],
    odds_2 = odds[c(F,F,T,F,F)],
    company_code = "wh")
}

ubet_matches_odds <- function(url) {
  drc$navigate(url)
  match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))[[1]] %>% read_html
  
  odds =  match_odds_html %>% 
    html_nodes("span.odds") %>% 
    html_text %>% 
    as.numeric
  
  selection = match_odds_html %>% 
    html_nodes("div.offer-name span") %>% 
    html_text()
  
  data.table(
    HomeTeam = selection[c(T,F,F)] %>% align_team2b365,
    AwayTeam = selection[c(F,F,T)] %>% align_team2b365,
    odds_1 = odds[c(T,F,F)],
    odds_x = odds[c(F,T,F)],
    odds_2 = odds[c(F,F,T)],
    company_code = "ubet")
}

bluebet_matches_odds <- function(url) {
  drc$navigate(url)
  match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))[[1]] %>% read_html
  
  odds = match_odds_html %>% 
    html_nodes("span.place-bet__odds") %>% 
    html_text %>% 
    as.numeric
  
  selection = match_odds_html %>% 
    html_nodes("div.zeta.headline-wrap.ng-binding") %>% 
    html_text
  
  data.table(
    HomeTeam = selection[c(T,F,F)] %>% align_team2b365,
    AwayTeam = selection[c(F,F,T)] %>% align_team2b365,
    odds_1 = odds[c(T,F,F)],
    odds_x = odds[c(F,T,F)],
    odds_2 = odds[c(F,F,T)],
    company_code = "bb")
}

neds_matches_odds <- function(url) {
  drc$navigate(url)
  Sys.sleep(2)
  match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))[[1]] %>% read_html
  
  odds <- match_odds_html %>% 
    html_nodes("div.live-event-detail-entrants button.odds.entrant-odds span") %>% 
    html_text %>% 
    as.numeric
  
  selection = match_odds_html %>% 
    html_nodes("div.entrant-wrapper span.entrant-name") %>% 
    html_text
  
  data.table(
    HomeTeam = selection[c(T,F,F)] %>% align_team2b365,
    AwayTeam = selection[c(F,F,T)] %>% align_team2b365,
    odds_1 = odds[c(T,F,F)],
    odds_x = odds[c(F,T,F)],
    odds_2 = odds[c(F,F,T)],
    company_code = "neds")
}

ladbrokes_matches_odds <- function(url) {
  #url = "https://www.ladbrokes.com.au/sports/soccer/56919181-football-england-premier-league/"
  drc$navigate(url)
  Sys.sleep(2)
  match_odds_html = drc$executeScript("return document.querySelector('html').outerHTML", args = list("dummy"))[[1]] %>% read_html
  
  odds = match_odds_html %>% 
    html_nodes("td.odds div a.odds.quickbet span") %>% 
    html_text %>% 
    as.numeric
  
  selection = match_odds_html %>% 
    html_nodes("tr.row td") %>% 
    html_text
  
  selection = selection[c(1,5) + rep(seq(0,length(selection),10),each=2)]
  selection = selection[!is.na(selection)] %>% str_trim
    
  data.table(
    HomeTeam = selection[c(T,F)] %>% align_team2b365,
    AwayTeam = selection[c(F,T)] %>% align_team2b365,
    odds_1 = odds[c(T,F,F)],
    odds_x = odds[c(F,F,T)],
    odds_2 = odds[c(F,T,F)],
    company_code="ladbrokes")
}
