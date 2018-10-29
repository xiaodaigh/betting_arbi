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

source("r/align_functions.r")
source("r/scrape_functions.r")

csl_b365_winners = get_b365_winner_odds("csl")
csl_uni_winners = get_uni_winner_odds("csl")
csl_tab_winner = get_tab_winner_odds("csl")

res_csl = csl_b365_winners %>% 
  merge(csl_uni_winners, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>% 
  merge(csl_tab_winner, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>% 
  mutate(odds_best_non_bf = pmax(odds_b365, odds_uni)) %>% 
  arrange(odds_b365) %>% 
  as.data.table; print(res_csl)

drc$closeall()
