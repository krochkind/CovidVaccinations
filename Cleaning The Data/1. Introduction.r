library(tidyverse)
library(dplyr)

Vaccinations= read_csv("Original_Data_Sets/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")
presidents= read_csv("Original_Data_Sets/president_by_state.csv")

presidents= select(presidents, "state_abr", "trump_win", "biden_win")

Vaccinations <- Vaccinations %>%
  filter(Date == "07/04/2021")

Vaccinations= select(Vaccinations, "Location", "Series_Complete_Yes")

Vaccinations= Vaccinations %>% 
  rename(
    state_abr = Location
  )

final= presidents %>%
  inner_join(Vaccinations, by = "state_abr")

final1= final %>%
  filter(trump_win == '1') %>%
  summarize(sum(Series_Complete_Yes))

Vaccinations_US= Vaccinations %>%
  filter(state_abr == 'US') %>%
  summarize(sum(Series_Complete_Yes))

RepublicanPercent = final1/Vaccinations_US
DemocratPercent = 1 - RepublicanPercent
