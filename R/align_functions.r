align_market <- function(market_company) {
  sapply(market_company, function(market_company1) {
    market_company1 = tolower(market_company1)
    if(is.na(market_company1)) {
      return(market_company1)
    } else if(market_company1 %in% "winner") {
      return("winner")
    } else if(market_company1 %in% "top 4") {
      return("top4")
    } else if(market_company1 == "top 6") {
      return("top6")
    } else if(market_company1 == "top 10") {
      return("top10")
    } else if(market_company1 == "relegation") {
      return("relegation")
    } else if(market_company1 == "to finish bottom") {
      return("bottom")
    } else if(market_company1 == "top goal scorer") {
      return("top goal scorer")
    } else {
      return("NOT FOUND")
    }
  })
}


align_team2b365 <- function(team_company) {
  teams_db = fread("data/team_alignment_db.csv")
  
  #browser()
  replace_with = teams_db$team_bet365[match(team_company,teams_db$team1)]
  replace_with[is.na(replace_with)] = team_company[is.na(replace_with)]
  team_company = replace_with
  
  sapply(team_company, function(team_company1) {
    if(is.na(team_company1)) {
      return(team_company1)
    } else if(team_company1 %in% c("West Ham United")) {
      return("West Ham")
    } else if(team_company1 %in% c("Wolves","Wolverhampton Wanderers")) {
      return("Wolverhampton")
    } else if(team_company1 == "C Palace") {
      return("Crystal Palace")
    } else if(team_company1 == "Manchester City") {
      return("Man City")
    } else if(team_company1 %in% c("Manchester Utd","Manchester United")) {
      return("Man Utd")
    } else if(team_company1 == "Tottenham Hotspur") {
      return("Tottenham")
    } else if(team_company1 == "Leicester City") {
      return("Leicester")
    } else if(team_company1 == "Newcastle United") {
      return("Newcastle")
    } else if(team_company1 == "AFC Bournemouth") {
      return("Bournemouth")
    } else if(team_company1 == "Brighton & Hove Albion") {
      return("Brighton")
    } else if(team_company1 == "Cardiff City") {
      return("Cardiff")
    } else if(team_company1 == "Huddersfield Town") {
      return("Huddersfield")
    } else if(team_company1 %in% c("Guangzhou Evergrande Taobao","Guangzhou FC")) {
      return("Guangzhou Evergrande")
    } else if(team_company1 %in% c("Shandong Luneng Taishan","Shandong")) {
      return("Shandong Luneng")
    } else if(team_company1 %in% c("Jiangsu Suning","Jiangsu Suning F.C.")) {
      return("Jiangsu Suning FC")
    } else if(team_company1 == "Guangzhou RF") {
      return("Guangzhou R&F")
    } else if(team_company1 %in% c("Guizhou Zhicheng","Guizhou HFZC","Guizhou HF Zhicheng","Guizhou Hengfeng")) {
      return("Guizhou Hengfeng Zhicheng")
    } else if(team_company1 == "Hammarby IF") {
      return("Hammarby")
    } else if(team_company1 == "IFK Norrköping") {
      return("IFK Norrkoping")
    } else if(team_company1 == "Malmö FF") {
      return("Malmo FF")
    } else if(team_company1 == "Helsingborgs IF") {
      return("Helsingborg")
    }else if(team_company1 == "Halmstads BK") {
      return("Halmstads")
    } else if(team_company1 == "Beijing Guaon") {
      return("Beijing Guoan")
    } else if(team_company1 == "Tianjin Quanjin") {
      return("Tianjin Quanjian")
    } else if(team_company1 == "Shanghai East Asia") {
      return("Shanghai SIPG")
    } else if(team_company1 == "Shenhua") {
      return("Shanghai Shenhua")
    } else if(team_company1 == "Tianjin TEDA FC") {
      return("Tianjin Teda")
    } else if(team_company1 == "Henan") {
      return("Henan Jianye")
    } else if(team_company1 %in% c("Dalian Aerbin FC","Dalian Yifang F.C.","Dalian Aerbin")) {
      return("Dalian Yifang")
    } else if(team_company1 == "Chongqing") {
      return("Chongqing Lifan")
    } else if(team_company1 %in% c("Hebei China Fortune FC", "Hebei","Hebei China Fortune")) {
      return("Hebei CFFC")
    } else {
      return(team_company1)
    }
  })
}

