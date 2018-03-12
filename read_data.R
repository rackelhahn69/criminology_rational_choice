setwd("~/Documents/Uni/criminology_rational_choice")

library(tidyverse)
library(magrittr)

library(labelled)

rckl <- 
  haven::read_dta("./data/Becker_Mehlkop.dta")

tibble(varn = 
         colnames(rckl),
       labl = 
         labelled::var_label(rckl) %>% 
         map(. %>% {if_else(is.null(.), NA_character_, .)}) %>% 
         unlist()
       ) %>% 
  filter(str_detect(labl, regex("schwarzfahr", ignore_case=TRUE)))

rckl %>% 
  transmute(
    hndl_absi = V107,
    kost_wert = V103,
    kost_prob = V104,
    nutz_wert = V105,
    nutz_prob = V106
  ) %T>% 
  {print(var_label(.))} %T>% 
  {print(val_labels(.))} %>% 
  mutate(
    hndl_absi = factor(hndl_absi, 1:2, labels=c("nein", "ja")),
    kost_wert = factor(kost_wert, 1:5, ordered=TRUE),
    kost_prob = factor(kost_prob, 1:5, ordered=TRUE),
    nutz_wert = factor(nutz_wert, 1:5, ordered=TRUE),
    nutz_prob = factor(nutz_prob, 1:5, ordered=TRUE)
  ) %>% 
  mutate_if(is.ordered, . %>% as.integer()) %>% 
  na.omit()
