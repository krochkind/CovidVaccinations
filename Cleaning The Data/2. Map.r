library(tidyverse)
devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)
library(usdata)
library(stringr)
#install.packages("strex")
library(strex)

# BEGIN: Create Election-by-State Data
states <- fifty_states
state_data_df <- read_csv("Original_Data_Sets/president_by_state.csv")

election_by_state <- state_data_df %>%
  mutate(
    id=tolower(state), 
    party = if_else(trump_win == 1, "Republican", "Democrat"))

us_states_elec <- merge(states, election_by_state, by="id") %>% 
  select(-c("id", "hole", "order", "piece", "trump_pct", "biden_pct", "trump_vote", "biden_vote", "trump_win", "biden_win"))
# END: Create Election-by-State Data

#------------------------------------------------------

# BEGIN: Create Election-by-County Data
president_county_candidate <- read_csv("Original_Data_Sets/countypres_2000-2020.csv")

president_county_candidate <- president_county_candidate %>%
  filter(year=="2020") %>% 
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  select(-c("year", "office", "candidate", "totalvotes", "version", "mode")) %>%
  mutate(
    party = if_else(party == "REPUBLICAN", "Republican", "Democrat"),
    state = str_to_title(state),
    county_name = str_to_title(county_name)
  )
president_county_candidate$won = ""
president_county_candidate <- rename(president_county_candidate, state_abr = state_po)

president_county_candidate <- na.omit(president_county_candidate)

for (row in 1:nrow(president_county_candidate)) {
  fips = president_county_candidate[row, "county_fips"]$county_fips
  
  d = president_county_candidate$candidatevotes[president_county_candidate$county_fips == fips & president_county_candidate$party == "Democrat"][1]
  r = president_county_candidate$candidatevotes[president_county_candidate$county_fips == fips & president_county_candidate$party == "Republican"][1]
  
  if (d > r) {
    president_county_candidate[row, "won"]$won = "Democrat"
  }
  else {
    president_county_candidate[row, "won"]$won = "Republican"
  }
}

president_county_candidate <- president_county_candidate %>%
  filter(candidatevotes > 0) %>%
  filter(party == won) %>%
  select(-c("won", "candidatevotes"))

counties <- usmap::us_map(regions = "counties")
counties$fips <- as.numeric(counties$fips)

president_county_gps <- left_join(counties, president_county_candidate, by = c("fips" = "county_fips")) %>%
  select(-c("order", "hole", "piece"))

president_county_gps <- rename(president_county_gps, long = x)
president_county_gps <- rename(president_county_gps, lat = y)


# fix invalid data
president_county_gps$state = president_county_gps$full
president_county_gps$state_abr = president_county_gps$abbr
president_county_gps$county_name = president_county_gps$county

# Update Missing Data
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Aleutians East Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Aleutians West Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Anchorage Municipality")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Bethel Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Bristol Bay Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Denali Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Dillingham Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Fairbanks North Star Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Haines Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Hoonah-Angoon Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Juneau City and Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Kenai Peninsula Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Ketchikan Gateway Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Kodiak Island Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Kusilvak Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Lake and Peninsula Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Matanuska-Susitna Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Nome Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "North Slope Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Northwest Arctic Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Petersburg Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Prince of Wales-Hyder Census Area")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Sitka City and Borough")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Skagway Municipality")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Southeast Fairbanks Census Area")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Valdez-Cordova Census Area")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Wrangell City and Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Yakutat City and Borough")] <- "Republican"
president_county_gps$party[which(president_county_gps$state == "Alaska" & president_county_gps$county_name == "Yukon-Koyukuk Census Area")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "District of Columbia" & president_county_gps$county_name == "District of Columbia")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "Hawaii" & president_county_gps$county_name == "Kalawao County")] <- "Democrat"
president_county_gps$party[which(president_county_gps$state == "South Dakota" & president_county_gps$county_name == "Oglala Lakota County")] <- "Democrat"

president_county_gps <- president_county_gps %>%
  select(-c("abbr", "full", "county", "county_name", "state", "state_abr"))
# END: Create Election-by-County Data

#------------------------------------------------------------

# BEGIN: Create COVID State Data
covid_state_df <- read_csv("Original_Data_Sets/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")

covid_state <- covid_state_df %>%
  filter(Date == "07/04/2021") %>%
  filter(Administered_Dose1_Recip_18PlusPop_Pct > 0) %>%
  select(c("Location", "Administered_Dose1_Recip_18PlusPop_Pct", "Administered_Dose1_Recip_18Plus"))

covid_state <- rename(covid_state, state_abr = Location)
covid_state <- rename(covid_state, shot_pct = Administered_Dose1_Recip_18PlusPop_Pct)
covid_state <- rename(covid_state, shots_given = Administered_Dose1_Recip_18Plus)
US_pct <- filter(covid_state, state_abr == 'US')$shot_pct
US_shots <- filter(covid_state, state_abr == 'US')$shots_given

states <- rename(fifty_states, state = id)
states <- states %>%
  mutate(state = tools::toTitleCase(state))

state_data <- state_data_df %>%
  mutate(
    id=tolower(state), 
    party = if_else(trump_win == 1, "Republican", "Democrat")) %>%
  select(-c("id", "trump_pct", "biden_pct", "trump_vote", "biden_vote", "trump_win", "biden_win"))

state_data <- merge(state_data, covid_state, by="state_abr") %>%
  mutate(over_under = if_else(shot_pct > US_pct, "Over", "Under"))

state_data <- state_data %>% add_row(state_abr = "US", state = "United States", party = "Democrat", shot_pct = US_pct, shots_given = US_shots)

state_data_gps <- merge(states, state_data, by="state") %>% 
  mutate(
    party=substr(party, 1, 1),
    over_under=substr(over_under, 1, 1)) %>% 
  select(-c("hole", "order", "piece", "shots_given", "state", "state_abr", "shot_pct"))

state_data_gps$group <- as.character(state_data_gps$group)

state_data_gps <- state_data_gps %>%
  mutate(group = str_replace(group, str_before_last_dot(group), state2abbr(str_before_last_dot(group))))


write_csv(state_data, "Output_CSV/covid_state_data.csv")
write_csv(state_data_gps, "Output_CSV/covid_state_data_gps.csv")
# END: Create COVID State Data

#------------------------------------------------------------

# BEGIN: Create COVID County Data
covid_county_df <- read_csv("Original_Data_Sets/COVID-19_Vaccinations_in_the_United_States_County.csv")

covid_county <- covid_county_df %>%
  filter(Date == "07/04/2021") %>%
  filter(Administered_Dose1_Recip_18PlusPop_Pct > 0) %>%
  select(c("FIPS", "Administered_Dose1_Recip_18PlusPop_Pct", "Administered_Dose1_Recip_18Plus"))

covid_county$FIPS <- as.numeric(covid_county$FIPS)

county_data_gps <- left_join(president_county_gps, covid_county, by = c("fips" = "FIPS")) %>%
  mutate(
    party=substr(party, 1, 1),
    over_under = if_else(Administered_Dose1_Recip_18PlusPop_Pct > US_pct, "O", "U")) %>%
  select(-c("Administered_Dose1_Recip_18PlusPop_Pct", "Administered_Dose1_Recip_18Plus", "fips"))

county_data_gps$group <- as.numeric(county_data_gps$group)

write_csv(county_data_gps, "Output_CSV/covid_county_data_gps.csv")
# END: Create COVID County Data

#------------------------------------------------------------

# BEGIN: Create Flu Data
flu_df <- read_csv("Original_Data_Sets/Influenza_Vaccination_Coverage.csv")

flu <- flu_df %>%
  filter(Year == "2018-19") %>%
  filter(Age == ">=18 Years") %>%
  na.omit(flu) %>%
  mutate(weight = SampleSize / sum(SampleSize),
         weighed_avg = weight * as.numeric(Estimate))

flu <- rename(flu, shot_pct = Estimate)
flu <- rename(flu, state = State)

US_pct = sum(flu$weighed_avg)

states <- rename(fifty_states, state = id)
states <- states %>%
  mutate(state = tools::toTitleCase(state))

flu <- flu %>%
  select(c("state", "shot_pct")) %>%
  mutate(over_under = if_else(shot_pct > US_pct, "O", "U"))


state_data <- state_data_df %>%
  mutate(
    id=tolower(state), 
    party = if_else(trump_win == 1, "R", "D")) %>%
  select(c("state", "party"))

flu_state_data_gps <- left_join(states, flu, by = c("state" = "state")) %>%
  inner_join(state_data, states, by = c("state" = "state")) %>%
  select(c("long", "lat", "group", "party","over_under"))

flu_state_data_gps$group <- as.character(flu_state_data_gps$group)

flu_state_data_gps <- flu_state_data_gps %>%
  mutate(group = str_replace(group, str_before_last_dot(group), state2abbr(str_before_last_dot(group))))           

write_csv(flu_state_data_gps, "Output_CSV/flu_state_data_gps.csv")
# END: Create Flu County Data