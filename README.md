# The Effect of Political Preference on Vaccination Rates

As of President Biden's July 4, 2021 deadline, the national COVID-19 vaccination rate was 67.1%.  Obviously a highly politicized topic, this dashboard uses R to visualize and uncover some of the dynamics and nuances in vaccination rates - 
while states that voted Republican (Red) in the 2020 Presidential Race have a lower percentage of COVID-19 vaccinations than Democrat States (Blue), does that apply to counties as well?  Does that also apply to the Flu vaccine?

The final interactive dashboard can be found [here](https://krochkind.shinyapps.io/Vaccinations/).

## Data
Covid and Flu vaccine data were obtained from the CDC.  The 2020 Election - President by State and by County were obtained from Kaggle and the Harvard Dataverse.  All data sources can be found in the FAQ section of the Shiny App, as well as in [/Cleaning The Data/Original_Data_Sets.zip](https://github.com/krochkind/CovidVaccinations/blob/main/Cleaning%20The%20Data/Original_Data_Sets.zip).

## Preparation
To faciliate loading, the data was pre-processed and cleaned, then split into four .CSV files:
* [*covid_county_data_gps.csv*](https://github.com/krochkind/CovidVaccinations/blob/main/Shiny%20App/covid_county_data_gps.csv) - Latitudinal and longitudinal coordinates to generate the choropleth map of the US by county, as well as if each county went Republican or Democrat, and if that county's COVID vaccination rates were above or below the national average
* [*covid_state_data.csv*](https://github.com/krochkind/CovidVaccinations/blob/main/Shiny%20App/covid_state_data.csv) - Each state, it's political party, percentage of shots given as of July 4, 2021, and if that states's COVID vaccination rates were above or below the national average
* [*covid_state_data_gps.csv*](https://github.com/krochkind/CovidVaccinations/blob/main/Shiny%20App/covid_state_data_gps.csv) - Latitudinal and longitudinal coordinates to generate the choropleth map of the US by state, as well as if each state went Republican or Democrat, and if that county's COVID vaccination rates were above or below the national average
* [*flu_state_data_gps.csv*](https://github.com/krochkind/CovidVaccinations/blob/main/Shiny%20App/google-analytics.html) - Latitudinal and longitudinal coordinates to generate the choropleth map of the US by state, as well as if each state went Republican or Democrat, and if that county's Flu vaccination rates were above or below the national average

The cleaning process can be found in the /Cleaning The Data/ folder.  There are two R scripts:
* [*1. Overview.r*](https://github.com/krochkind/CovidVaccinations/blob/main/Cleaning%20The%20Data/1.%20Overview.r) - Gets the RepublicanPercent (37%) and DemocratPercent (63%) vaccination rates that are hardcoded in the Kuiper-Marshall plot in the Shiny app
* [*2. Map.r*](https://github.com/krochkind/CovidVaccinations/blob/main/Cleaning%20The%20Data/2.%20Map.r) - Outputs cleaned/transformed versions of CSV files used to create the choropleth maps.  This is also the source of the hardcoded US Vaccination Percent (67.1%)

## Shiny App
The data to generate the Shiny App can be found in [/Shiny App/app.r](https://github.com/krochkind/CovidVaccinations/blob/main/Shiny%20App/app.R), as well as a helpers.r file to faciliate the choropleth map, and a Google Analytics file to track visitors.  As mentioned above, the final interactive dashboard can be found [here](https://krochkind.shinyapps.io/Vaccinations/).
