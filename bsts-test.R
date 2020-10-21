library(bsts)     # load the bsts package
library(tidyverse)

## Ladda data
filer <- list.files("data") %>% 
  paste0("data/", .) %>% 
  set_names()

tds2 <- read_delim(filer[[1]], delim = ";")
tds5 <- read_delim(filer[[2]], delim = ";")
tds9 <- read_delim(filer[[3]], delim = ";")

tds2 <- tds2 %>% 
  mutate_all(~str_replace(., ",", ".")) %>% 
  mutate_at(vars(value, consumption_value, temperature), as.numeric)

tds5 <- tds5 %>% 
  mutate_all(~str_replace(., ",", ".")) %>% 
  mutate_at(vars(value, consumption_value, temperature), as.numeric)

tds9 <- tds9 %>% 
  mutate_all(~str_replace(., ",", ".")) %>% 
  mutate_at(vars(value, consumption_value, temperature), as.numeric)

tds9_energy <- tds5 %>%
  filter(property == "flow")

## Bsts

ss <- AddLocalLinearTrend(list(), tds9_energy$consumption_value)
ss <- AddSeasonal(ss, tds9_energy$consumption_value, nseasons = 52, season.duration = 7)

model1 <- bsts(consumption_value ~ temperature,
               state.specification = ss,
               niter = 1000,
               data = tds9_energy)

PlotBstsComponents(model1)















