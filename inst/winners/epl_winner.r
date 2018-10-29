library(rvest)
library(data.table)
library(RSelenium)
library(stringr)
library(dplyr)
library(tidyr)
pt <- proc.time()
dr = RSelenium::rsDriver()

drc = dr$client
#on.exit(drc$closeall())

source("R/align_functions.r")
source("R/scrape_functions.r")

# Bet365 B365 -------------------------------------------------------------
# typically these are odds that exists as team odds
b365_winner_odds = get_b365_winner_odds("epl")
#wh_winner_odds = get_wh_winner_odds()
cb_winner_odds = get_cb_winner_odds()
neds_winner_odds = get_neds_winner_odds()
lad_winner_odds = get_lad_winner_odds()
sb_winner_odds = get_sb_winner_odds()
uni_winner_odds = get_uni_winner_odds("epl")
tab_winner_odds = get_tab_winner_odds("epl")
bf_winner_odds = get_bf_winner_odds("epl")
bb_winner_odds = get_bb_winner_odds()
ubet_winner_odds = get_ubet_odds()

# merge everything together -----------------------------------------------
if(F) {
  get_bf_winner_odds()
}

res_epl = merge(b365_winner_odds, wh_winner_odds, by = c("selection_b365","market")) %>% 
  merge(cb_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>% 
  merge(neds_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  merge(lad_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  merge(sb_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  merge(uni_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  merge(tab_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  merge(bf_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  merge(bb_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  merge(ubet_winner_odds, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>%
  mutate(odds_best_non_bf = pmax(odds_b365, odds_wh, odds_cb, odds_neds, odds_lad, odds_sb, odds_uni, odds_tab, odds_bb, odds_ubet)) %>% 
  arrange(odds_b365) %>% 
  as.data.table; print(res_epl)


res_best_vs_bf = res_epl[,.(selection_b365, odds_best_non_bf, odds_bf, odds_lay)]
res_best_vs_bf[,better_than_bf := odds_best_non_bf > odds_bf]; print(res_best_vs_bf)

res_best_vs_bf[better_than_bf==T & odds_bf!= 1000,]

fst::write_fst(res_epl,
               paste("data/epl_winner_ods",Sys.time(),".fst",sep="_") %>% 
                 stringr::str_replace_all(":","_") %>% 
                 str_replace_all(" ","_")
)

print(data.table::timetaken(pt))

drc$closeall()

View(res_epl)


