setwd("~/Documents/Uni/criminology_rational_choice")

library(tidyverse)
library(magrittr)

library(labelled)

library(mgcv)

theme_bw() %+replace%
  theme(panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title=element_text(size=12)) %>% 
  theme_set()

####
####
####

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

rckl %<>% 
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

# form <-
#   hndl_absi ~ 
#   s(nutz_wert, by=nutz_prob, k=5, bs="cr") + 
#   # s(nutz_prob, k=5, bs="cr") + 
#   s(kost_wert, by=kost_prob, k=5, bs="cr")# + 
#   # s(kost_prob, k=5, bs="cr")

form <-
  hndl_absi ~ 
    s(nutz_wert, k=5, bs="cr") * 
    s(nutz_prob, k=5, bs="cr") + 
    s(kost_wert, k=5, bs="cr") * 
    s(kost_prob, k=5, bs="cr")

form <-
  hndl_absi ~
  s(nutz_wert, by=nutz_prob, k=5, bs="cr") +
  # s(nutz_prob, k=5, bs="cr") +
  s(kost_wert, by=kost_prob, k=5, bs="cr")# +
  # s(kost_prob, k=5, bs="cr")

modl <- 
  gam(form, data=rckl, family="binomial") %T>% 
  {print(summary(.))}

plot_nutz <- 
  modl %>% 
  visreg::visreg2d("nutz_prob", "nutz_wert", plot=FALSE) %$%
  as_tibble(z) %>% 
  rownames_to_column("x") %>% 
  gather(key="y", value="z", -x) %>% 
  mutate(x = as.integer(x),
         y = y %>% str_replace("V", "") %>% as.integer()) %T>%
  {assign("maxz", max(abs(.$z)), pos=1)} %>% 
  ggplot(aes(x=x, y=y, group=z, fill=z)) +
  geom_bin2d() +
  scale_fill_distiller(type="div", palette="Spectral",
                       limits=c(-maxz,maxz), direction=1) +
  labs(title="Nutzen",
       x="Eintrittswahrscheinlichkeit",
       y="Wert") +
  theme(legend.position="none")
  
plot_kost <-
  modl %>% 
  visreg::visreg2d("kost_prob", "kost_wert", plot=FALSE) %$%
  as_tibble(z) %>% 
  rownames_to_column("x") %>% 
  gather(key="y", value="z", -x) %>% 
  mutate(x = as.integer(x),
         y = y %>% str_replace("V", "") %>% as.integer()) %T>%
  {assign("maxz", max(abs(.$z)), pos=1)} %>% 
  ggplot(aes(x=x, y=y, group=z, fill=z)) +
  geom_bin2d() +
  scale_fill_distiller(type="div", palette="Spectral",
                       limits=c(-maxz,maxz), direction=1) +
  labs(title="Kosten",
       x="Eintrittswahrscheinlichkeit",
       y="Wert")

patchwork::wrap_plots(plot_nutz, plot_kost, nrow=1)
