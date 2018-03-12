setwd("~/Documents/Uni/criminology_rational_choice")

library(tidyverse)
library(magrittr)
library(haven)

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
