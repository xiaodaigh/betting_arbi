align_market <- function(market_company) {
  sapply(market_company, function(market_company1) {
    if(is.na(market_company1)) {
      return(market_company1)
    } else if(market_company1 == "Top 4") {
      return("top4")
    } else if(market_company1 == "Top 6") {
      return("top6")
    } else if(market_company1 == "Relegation") {
      return("relegation")
    } else if(market_company1 == "To Finish Bottom") {
      return("bottom")
    } else {
      return("NOT FOUND")
    }
  })
}


align_team2b365 <- function(team_company) {
  sapply(team_company, function(team_company1) {
    if(is.na(team_company1)) {
      return(team_company1)
    } else if(team_company1 %in% c("Wolves","Wolverhampton Wanderers")) {
      return("Wolverhampton")
    } else if(team_company1 == "C Palace") {
      return("Crystal Palace")
    } else if(team_company1 == "Manchester City") {
      return("Man City")
    } else if(team_company1 == "Manchester United") {
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
    } else {
      return(team_company1)
    }
  })
}

