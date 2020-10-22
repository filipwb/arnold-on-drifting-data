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
  mutate(weekday = weekdays(as.Date(consumption_from))) %>% 
  filter(property == "flow") %>% 
  dplyr::select(consumption_from, consumption_value, temperature, weekday)

library(data.table)
tds9_energy <- as.data.table(tds9_energy)
tds9_energy <- dcast(tds9_energy, consumption_from+consumption_value+temperature ~ weekday, fun = length, fill=0)

## Bsts
ss <- AddLocalLinearTrend(list(), tds9_energy$consumption_value)

model1 <- bsts(consumption_value ~ temperature +
                 #Måndag +
                 Tisdag +
                 Onsdag +
                 Torsdag +
                 Fredag + 
                 Lördag +
                 Söndag,
               state.specification = list(),
               niter = 1000,
               data = tds9_energy)

plot(model1, "coef")

GetInclusionProbabilities <- function(bsts.object) {
  # Pulls code from
  # - BoomSpikeSlab::PlotMarginalInclusionProbabilities
  # - bsts::PlotBstsCoefficients
  burn <- SuggestBurn(0.1, bsts.object)
  beta <- bsts.object$coefficients
  beta <- beta[-(1:burn), , drop = FALSE]
  inclusion.prob <- colMeans(beta != 0)
  index <- order(inclusion.prob)
  inclusion.prob <- inclusion.prob[index]
  # End from BoomSpikeSlab/bsts.
  return(data.frame(predictor = names(inclusion.prob),
                    inclusion.prob = inclusion.prob))
}

GetInclusionProbabilities(model1)














