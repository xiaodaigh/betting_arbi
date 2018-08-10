# doing team comparison ---------------------------------------------------
team_cmp = function(x,y) {
  setkey(x, HomeTeam, AwayTeam)
  setkey(y, HomeTeam, AwayTeam)
  xy = cbind(x[,.(ht1=HomeTeam,at1=AwayTeam)],y[,.(ht2=HomeTeam, at2=AwayTeam)])
  setDT(xy)
  xy[ht1 != ht2 | at1 != at2,]
}

team_cmp2 = function(x,y) {
  merge(x,y, by=c("HomeTeam","AwayTeam"), all.x=T,all.y=T)
}

#' Make Chrome
startChrome <- function() {
  #browser()
  eCaps <- list(chromeOptions = list(prefs = list("profile.default_content_settings.popups" = 0L)))
  dr <- RSelenium::rsDriver(browser = "chrome", chromever = "latest", extraCapabilities  = eCaps, geckover = NULL, phantomver = NULL,  iedrver = NULL)
  #drc = dr$client
  #drc$maxWindowSize()
  dr
}
