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

source("align_functions.r")
source("scrape_functions.r")

sweden1_b365_winners = get_b365_winner_odds("sweden2")
sweden1_uni_winners = get_uni_winner_odds("sweden2")

setdiff(sweden1_b365_winners$selection_b365,sweden1_uni_winners$selection_b365)
setdiff(sweden1_uni_winners$selection_b365,sweden1_b365_winners$selection_b365)

warning("check TAB again, potentially they have a CSL market")

res_csl = sweden1_b365_winners %>% 
  merge(sweden1_uni_winners, by = c("selection_b365","market")) %>% 
  select(selection_b365, market, starts_with("odds")) %>% 
  mutate(odds_best_non_bf = pmax(odds_b365, odds_uni)) %>% 
  arrange(odds_b365) %>% 
  as.data.table; print(res_csl)

drc$closeall()