library(rvest)
library(data.table)
library(RSelenium)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
pt <- proc.time()

eCaps <- list(
  chromeOptions =
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L
    )
    )
)

dr <- RSelenium::rsDriver(browser = "chrome",chromever = "latest", extraCapabilities  = eCaps, geckover = NULL, phantomver = NULL,  iedrver = NULL)


drc = dr$client
#on.exit(drc$closeall())

source("align_functions.r")
source("scrape_functions.r")
source("scrape_matches_fn.r")

csl_bf_matches_odds <- bf_matches_odds("https://www.betfair.com.au/exchange/plus/football/competition/879931")
csl_b365_odds = b365_matches_odds("https://www.bet365.com.au/?rn=49941906763&stf=1#/AC/B1/C1/D13/E37844384/F2/")
csl_sb_matches = sb_matches_odds("https://www.sportsbet.com.au/betting/soccer/asia/chinese-super-league")
csl_oddsportal_match = oddsportal_matches_odds("http://www.oddsportal.com/soccer/china/super-league/")
csl_wh_matches = wh_matches_odds("https://www.williamhill.com.au/sports/soccer/asia/chinese-super-league")
csl_ubet_matches = ubet_matches_odds("https://ubet.com/sports/soccer/china-super-league/chinese-super-league-matches")
csl_bluebet_matches = bluebet_matches_odds("https://www.bluebet.com.au/sports/Soccer/China/Chinese-Super-League/27196")
csl_neds_matches = neds_matches_odds("https://www.neds.com.au/sports/soccer/71955b54-62f6-4ac5-abaa-df88cad0aeef#71955b54-62f6-4ac5-abaa-df88cad0aeef_china")


# doing team comparison ---------------------------------------------------
team_cmp = function(x,y) {
  setkey(x, HomeTeam, AwayTeam)
  setkey(y, HomeTeam, AwayTeam)
  xy = cbind(x[,.(ht1=HomeTeam,at1=AwayTeam)],y[,.(ht2=HomeTeam, at2=AwayTeam)])
  setDT(xy)
  xy[ht1 != ht2 | at1 != at2,]
}

team_cmp(csl_bf_matches_odds, csl_neds_matches)

team_cmp2 = function(x,y) {
  merge(x,y, by=c("HomeTeam","AwayTeam"), all.x=T,all.y=T)
}

team_cmp2(csl_b365_odds, csl_bf_matches_odds)
team_cmp2(csl_b365_odds, csl_sb_matches)
team_cmp2(csl_b365_odds, csl_oddsportal_match)
team_cmp2(csl_b365_odds, csl_wh_matches)
team_cmp2(csl_b365_odds, csl_ubet_matches)
team_cmp2(csl_b365_odds, csl_bluebet_matches)
team_cmp2(csl_b365_odds, csl_neds_matches)


              
csl_b365_odds
csl_sb_matches
csl_wh_matches
csl_ubet_matches
csl_bluebet_matches
csl_neds_matches

drc$closeall()


# find the best odds ------------------------------------------------------
setnames(csl_b365_odds,
         c("odds_1_b365","odds_x_b365","odds_2_b365"),
         c("odds_1","odds_x","odds_2"))

csl_bf_matches_odds_copy = copy(csl_bf_matches_odds)

# epl_bf_matches_odds_copy = epl_bf_matches_odds_copy[,.(HomeTeam,AwayTeam, 
#                                                        odds_bf1=round((odds_bf1-1)*0.95,0)+1, 
#                                                        odds_bfx=round((odds_bfx-1)*0.95,0)+1,
#                                                        odds_bf2=round((odds_bf2-1)*0.95,0)+1, company_code)]


csl_bf_matches_odds_copy = csl_bf_matches_odds_copy[,.(HomeTeam,AwayTeam, 
                                                       odds_bf1, 
                                                       odds_bfx,
                                                       odds_bf2, company_code)]


setnames(csl_bf_matches_odds_copy,
         c("HomeTeam","AwayTeam", "odds_bf1", "odds_bfx", "odds_bf2"),
         c("HomeTeam","AwayTeam", "odds_1", "odds_x", "odds_2"))

odds_all = map(list(
  csl_bf_matches_odds_copy,
  csl_b365_odds,
  csl_sb_matches,
  csl_wh_matches,
  csl_ubet_matches,
  csl_bluebet_matches,
  csl_neds_matches
), ~gather(.x, key=odds_type, value=odds, -c(HomeTeam,AwayTeam, company_code))) %>% rbindlist
  

best_odds = odds_all[,.(best_odds = max(odds)),.(HomeTeam, AwayTeam, odds_type)]


odds_all_w_best = 
  odds_all %>% 
  merge(best_odds, by=c("HomeTeam","AwayTeam","odds_type")) %>% 
  filter(odds >= best_odds) %>% 
  as.data.table

odds_all_w_best1 = odds_all_w_best[,.(odds = mean(odds), company_codes=paste(company_code,collapse = ",")),.(HomeTeam, AwayTeam, odds_type)];odds_all_w_best1

odds_all_w_best2 = merge(
  odds_all_w_best1 %>% 
    select(-company_codes) %>% 
    spread(key=odds_type,value=odds)
  ,
  odds_all_w_best1 %>% 
    select(-odds) %>% 
    mutate(odds_type = paste0(odds_type,"_comp")) %>% 
    spread(key=odds_type,value=company_codes)
);odds_all_w_best2

odds_all_w_best2


# code to calculate odds for maximum return -------------------------------
o1=2.3
ox=3.2
o2=2.8
stake=c(20,20,20)
stake=c(23.57873,16.84194,19.29136)
i = 1:3
best_stake <- function(o1,ox,o2) {
  optimal_bet_size <- function(stake, o1, ox, o2) {
    o1x2 = c(o1,ox,o2)
    stake = pmax(5,stake)
    x = map_dbl(1:3, ~-stake[.x] * (o1x2[.x]-1) + sum(stake[-(.x)])*0.95)
    sum((x - mean(x))^2)
  }
  pmax(round(optim(rep(20,3), optimal_bet_size, o1=o1,ox=ox,o2=o2)$par,0),5)
}

best_stake_dt = t(odds_all_w_best2[,mapply(best_stake, odds_1, odds_x, odds_2, SIMPLIFY = F)]) %>% 
  as.data.table
setnames(best_stake_dt,names(best_stake_dt),c("best_o1","best_ox","best_o2"))

odds_all_w_best3 = cbind(odds_all_w_best2,best_stake_dt)
View(odds_all_w_best3[
  rowSums(data.table(odds_2_comp=="bf",odds_1_comp=="bf",odds_x_comp=="bf"))<=1
  ,.(HomeTeam,AwayTeam,paste(odds_1,best_o1,odds_1_comp,sep=" - "),paste(odds_2,best_o2,odds_2_comp,sep=" - "),paste(odds_x,best_ox,odds_x_comp,sep=" - "),odds_1_comp,odds_x_comp,odds_2_comp)])

odds_all_w_best4 = odds_all_w_best3[,.(HomeTeam,AwayTeam,wp=1/odds_1,dp=1/odds_x,lp=1/odds_2)]

odds_all_w_best4[,wp:=wp/(wp+dp+lp)]
odds_all_w_best4[,dp:=dp/(wp+dp+lp)]
odds_all_w_best4[,lp:=lp/(wp+dp+lp)]

nsim=1000
simres = replicate(nsim, {
  odds_all_w_best4[,rr:=runif(.N)]
  odds_all_w_best4[rr <= wp,hpts:=3]
  odds_all_w_best4[rr <= wp,apts:=0]
  odds_all_w_best4[rr > wp,hpts:=1]
  odds_all_w_best4[rr > wp,apts:=1]
  odds_all_w_best4[rr > wp+dp,hpts:=0]
  odds_all_w_best4[rr > wp+dp,apts:=3]
  
  odds_all_w_best5 = rbindlist(list(
    odds_all_w_best4[,.(pts = sum(hpts)),.(team=HomeTeam)],
    odds_all_w_best4[,.(pts = sum(apts)),.(team=AwayTeam)]))
  
  odds_all_w_best5 = odds_all_w_best5[,.(pts=sum(pts)),team]
  odds_all_w_best5[,r:=runif(.N)]
  odds_all_w_best5[order(pts,r,decreasing = T),rank:=1:.N]
  setkey(odds_all_w_best5,rank)
  odds_all_w_best5
}, simplify = F) %>% rbindlist


simres[pts==3,.N,team][,.(team, rank = N/nsim)][team!="Beijing Guoan"][order(rank,decreasing = T)]
ps = simres[team=="Beijing Guoan",.N/nsim,.(team,pts)]

plot(ps[,.(pts, V1)])

simres[,nsim/.N,.(team, pts)] %>% spread(key=pts, value=V1)

# how much more to bet ----------------------------------------------------
lo=c(3.35,3.95,2.45)
st=c(20,0+16,0+26)

sum(1/lo)
sum(st)-st*(lo-1)

lay_odds=c(3.35,3.7,2.4)
existing_stake=c(20,0,0)


how_much_more_stake <- function(existing_lay_odds, existing_stake, new_lay_odds, min_stake=0) {
  print(sum(1/new_lay_odds))
  existing_ret = sum(existing_stake)-existing_stake*existing_lay_odds
  print(existing_ret)
  
  
  res = optim(rep(5,length(existing_stake)), function(new_stake) {
    new_ret = sum(new_stake)-new_stake*new_lay_odds
    ret = new_ret + existing_ret
    sum((ret - mean(ret))^2)
  }, lower = rep(min_stake,length(existing_stake)), method="L-BFGS-B")
  
  new_ret = sum(res$par)-res$par*(new_lay_odds)
  print(existing_ret + new_ret,0)
  round(res$par,0)
}


how_much_more_stake(c(1.53,7.19,4.7),c(38,20,12), c(0,0,0))

how_much_more_stake(c(12.5,6.6,1.25),c(5,9,51), c(14.5,6.6,1.26))


odds_all_w_best3[HomeTeam == "Chongqing Lifan",]
how_much_more_stake(c(4.2,3.9,1.89),c(13,14,0), c(4.6,4.6,1.94))

how_much_more_stake(c(1.66,4.5,6.2),c(0,10.77,20), c(1.71,4.45,4.6))

odds_all_w_best3[HomeTeam=="Shanghai SIPG",]
how_much_more_stake(c(1.35,5.3,9.2),c(0,0,4.58), c(1.37,5.3,10), 5)
how_much_more_stake(c(1.35,5.3,9.2),c(0,0,4.58), c(1.37,5.3,10), 0)

how_much_more_stake(c(1.37,5.3,9.2),c(29.64,0,4.58), c(1.37,5.3,10), 0)

how_much_more_stake(c(1.37,5.3,9.2),c(35.64,0,4.58), c(1.39,5.4,11), 5)


odds_all_w_best3[HomeTeam == "Beijing Renhe",]
how_much_more_stake(c(2.7,2.8,3.35),c(22.98,0,0), c(2.5,3.40,3.05), 0)
how_much_more_stake(c(2.7,2.8,3.04),c(22.98,0,25), c(2.52,3.5,3.05), 0)

odds_all_w_best3[AwayTeam  == "Dalian Yifang",]
how_much_more_stake(c(2.4,3.3,2.8),c(0,0,0),c(2.4,3.3,2.8), 5)
how_much_more_stake(c(2.42,3.35,2.82),c(0,0,0),c(2.42,3.35,2.82), 20)
save.image(paste("csl_matches",Sys.time(),".RDS") %>% str_remove_all(":"))