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

# load the database
load_league = "epl"
scrape_db <- fread("data/matches_scrape_db.csv")
scrape_db1 = scrape_db[league==load_league,]


# a = get_matches_odds("https://www.betfair.com.au/exchange/plus/football/competition/879931","betfair")
# matches_odds = pmap_dfr(scrape_db1, function(...) {
#   a = list(...)
#   tryCatch({
#     return(get_matches_odds(a$url, a$company))
#     },
#     error = function(e) {
#       print(e)
#     },
#     finally = gc())
# })
res = future.apply::future_lapply(1:nrow(scrape_db1), function(one_row) {
#res = lapply(1:nrow(scrape_db1), function(one_row) {
     a = scrape_db1[one_row,]
     tryCatch({
       return(get_matches_odds(a$url, a$company))
       },
       error = function(e) {
         print(e)
       },
       finally = gc())
 })

failed_i = which(sapply(res, function(x) "error" %in% class(x) || nrow(x)==1 ))
print(scrape_db1[failed_i])

# do it one by one given it has failed
res2 = lapply(failed_i, function(one_row) {
  a = scrape_db1[one_row,]
  tryCatch({
    return(get_matches_odds(a$url, a$company))
  },
  error = function(e) {
    print(e)
  },
  finally = gc())
})

matches_odds = res %>%
  Filter(function(x) "data.frame" %in% class(x),.) %>%
  rbindlist(use.names = T, fill = T) %>%
  list(res2 %>% 
         Filter(function(x) "data.frame" %in% class(x) && nrow(x) >1, .) %>% 
         rbindlist(use.names = T, fill = T)) %>%
  rbindlist(use.names = T, fill = T) %>%
  filter(!is.na(HomeTeam)) %>% 
  mutate(extraction_timestamp  = Sys.time())

setDT(matches_odds)
datetime_char = as.character(Sys.time()) %>% str_replace_all(":","-")
fst::write_fst(matches_odds, sprintf("data/output/matches/%s %s.fst",load_league,datetime_char))

print(timetaken(pt))

# find the best odds ------------------------------------------------------
matches_odds_copy = copy(matches_odds)

odds_all = matches_odds_copy %>% 
  select(HomeTeam,AwayTeam, company_code, odds_1, odds_2, odds_x) %>% 
  filter(!company_code %in% c("op","oddsportal")) %>% 
  gather(key=odds_type, value=odds, -c(HomeTeam,AwayTeam, company_code)) %>% 
  filter(!is.na(odds)) %>% 
  as.data.table()

odds_all[,has_betfair:=sum(company_code == "betfair")>=1,.(HomeTeam,AwayTeam)]

best_odds = odds_all[,.(best_odds = max(odds)),.(HomeTeam, AwayTeam, odds_type)]


odds_all_w_best = 
  odds_all %>% 
  merge(best_odds, by=c("HomeTeam","AwayTeam","odds_type")) %>% 
  filter(odds >= best_odds) %>% 
  as.data.table

odds_all_w_best1 = odds_all_w_best[,.(odds = mean(odds), company_codes=paste(company_code,collapse = ","), has_betfair=any(has_betfair)),.(HomeTeam, AwayTeam, odds_type)];odds_all_w_best1

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
    if(sum(1/o1x2) < 1) return(sum(o1x2^2))
    stake = pmax(20,stake)
    x = map_dbl(1:3, ~-stake[.x] * (o1x2[.x]-1) + sum(stake[-(.x)])*0.95)
    sum((x - mean(x))^2)
  }
  round(optim(rep(20,3), optimal_bet_size, o1=o1,ox=ox,o2=o2,lower=rep(20,3),method="L-BFGS-B")$par,0)
}

best_stake_dt = t(odds_all_w_best2[,mapply(best_stake, odds_1, odds_x, odds_2, SIMPLIFY = F)]) %>% 
  as.data.table
setnames(best_stake_dt,names(best_stake_dt),c("best_o1","best_ox","best_o2"))

odds_all_w_best3 = cbind(odds_all_w_best2,best_stake_dt)

odds_all_w_best3[,.N,.(HomeTeam,AwayTeam)]

View(odds_all_w_best3)

View(odds_all_w_best3[has_betfair==T,
  rowSums(data.table(odds_2_comp=="betfair",odds_1_comp=="betfair",odds_x_comp=="betfair"))==0
  ,.(HomeTeam,AwayTeam,paste(odds_1,best_o1,odds_1_comp,sep=" - "),paste(odds_2,best_o2,odds_2_comp,sep=" - "),paste(odds_x,best_ox,odds_x_comp,sep=" - "),odds_1_comp,odds_x_comp,odds_2_comp)])

View(odds_all_w_best3[has_betfair==T,
  rowSums(data.table(odds_2_comp=="betfair",odds_1_comp=="betfair",odds_x_comp=="betfair"))<=1
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

simres[rank==1,nsim/.N,team][order(V1)]
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

save.image(paste("epl_matches",Sys.time(),".RDS") %>% str_remove_all(":"))
