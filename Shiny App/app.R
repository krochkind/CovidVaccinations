library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(faq)
library(waffle)
library(tidyverse)
library(ggplot2)
library(rsconnect)
library(ggthemes)
source("helpers.R")
library(maps)
library(mapproj)
library(personograph)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = "Politics vs. Vaccinations"
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Introduction", tabName = "tab1-intro", icon = icon("info")),
                        menuItem("Overall Percentage", tabName = "tab2-summary", icon = icon("user")),
                        menuItem("Political Map", tabName = "tab3-maps", icon = icon("map-o")),
                        menuItem("State Comparisons", tabName = "tab4-comparisons", icon = icon("bar-chart")),
                        menuItem("FAQs", tabName = "tab5-faq", icon = icon("question"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        includeHTML("google-analytics.html"),
                        tags$style(HTML('
      .main-header .logo { font-weight: bold; font-size: 16px; }
      .tab-pane h3 { margin-top: 0; margin-bottom: -5px }
      #shiny-tab-page2 .row h3 { margin-top: 0; margin-bottom: 10px; padding-left: 15px }
      
    '))),
                      tabItems(
                        tabItem(tabName = "tab1-intro",
                                tags$div(
                                  tags$h3("The Effect of Political Preference on Vaccination Rates"), 
                                  tags$br(),
                                  tags$p("As of President Biden's July 4, 2021 deadline, the national COVID-19 vaccination rate was 67.1%, but how does that break down by political preference?"),
                                  tags$p("States that voted Republican (Red) in the 2020 Presidential Race have a lower percentage of COVID-19 vaccinations than Democrat States (Blue), but does that apply to counties as well?  Does that also apply to the Flu vaccine?"),
                                  tags$p("The following pages help visualize and uncover some of the lesser known dynamics and nuances in vaccination rates.")
                                )
                        ),
                        
                        tabItem(tabName = "tab2-summary",
                                tags$h3("Vaccinated individuals are almost twice as likely to be living in Blue States"), 
                                tags$br(),
                                tags$p("As of July 4, 2021, out of every 100 people vaccinated, 37 were living in Red States and 63 were living in Blue States."),
                                plotOutput("plot1", height = 500)
                        ),
                        
                        
                        tabItem(tabName = "tab3-maps",
                                fluidRow(
                                  tags$h3(textOutput("map_title")),
                                  column(3,
                                         tags$head(
                                           tags$style(type="text/css", "
                   #inline label { padding-right: 10px; display: table-cell; text-align: center; vertical-align: middle; } 
                   #inline .form-group { display: table-row; }
                   #inline .selectize-control { margin-bottom: -6px; width: 115px }
                   #inline .item { padding-right: 20px; }
                   #map_title { padding: 0 0 15px 15px; }")
                                         ),
                                         
                                         tags$div(id = "inline", 
                                                  selectInput("mapType", label = "Map:", 
                                                              choices = list("By State" = "State", "By County" = "County"),
                                                              selected = 1
                                                  )    
                                         ),
                                         
                                         checkboxInput("CheckboxCovid", label = "COVID-19 Vaccine"),
                                         checkboxInput("CheckboxFlu", label = "Flu Vaccine")
                                  ),
                                  column(9,
                                         tags$br(),
                                         plotOutput("plot2", width="100%")
                                  )
                                )
                        ),
                                                
                        tabItem(tabName = "tab4-comparisons",
                                tags$h3("State by State Comparison of COVID-19 Vaccination Rates"),
                                tags$br(),
                                selectInput("sel_state1",
                                            "Select State 1",
                                            "Names", multiple = FALSE),
                                selectInput("sel_state2",
                                            "Select State 2",
                                            "Names", multiple = FALSE),
                                
                                plotOutput("plot3", height = 500)
                        ),
                        
                        tabItem(tabName = "tab5-faq",
                                tabPanel("FAQs",
                                         tags$div(
                                           tags$h3("Frequently Asked Questions"), 
                                           tags$br(),
                                           tags$h4("How was a state's political preference determined?"), 
                                           "Political preference was based on the popular vote's majority in the 2020 Presidential Election.  This was especially important for Main and Nebraska, who split electoral votes.",
                                           tags$br(),tags$br(),tags$h4("Why has the data not been updated since July 4, 2021?"),
                                           "The federal government set a target of 70% of adults having at least one vaccination shot by July 4, 2021. This date represents the ability for all individuals to have received a vaccination if they wanted one, which allows for an adequate analysis of whether political preference influenced an individual on getting a vaccination."),
                                         tags$br(),tags$h4("What is the purpose of showing Flu vaccination data?"),                            
                                         "The Flu vaccination data is used to determine if the correlation between political preference is limited to the COVID-19 vaccination or if it is possible that the correlation is for all vaccinations.",
                                         tags$br(),tags$br(),tags$h4("Why is the Flu data for 2018 - 2019?"),
                                         "Since the COVID-19 pandemic impacted people's willingness to leave their houses to get a Flu shot, 2018 - 2019 data is more representative of Flu shot acceptance than 2019 - 2020 data.",
                                         tags$br(),tags$br(),tags$h4("Where is the Flu data by county?"),
                                         "The CDC only provided Flu vaccination rates by state.  The original data sets can be found below (under \"Sources\").",
                                         tags$br(),tags$br(),tags$h4("Why do some areas on the map show as grey?"),
                                         "Not all states and counties in the US reported their COVID-19 and Flu vaccination data.",
                                         tags$br(),tags$br(),tags$h4("Sources"),
                                         tags$a(href="https://data.cdc.gov/Vaccinations/Covid-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc", "Covid vaccinations by State", target="_blank"),tags$br(),
                                         tags$a(href="https://data.cdc.gov/Vaccinations/Covid-19-Vaccinations-in-the-United-States-County/8xkx-amqh", "Covid Vaccinations by County", target="_blank"),tags$br(),
                                         tags$a(href="https://www.kaggle.com/callummacpherson14/2020-us-presidential-election-results-by-state", "2020 Election - President by State", target="_blank"),tags$br(),
                                         tags$a(href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ", "2020 Election - President by County", target="_blank"),tags$br(),
                                         tags$a(href="https://data.cdc.gov/Flu-Vaccinations/Influenza-Vaccination-Coverage-for-All-Ages-6-Mont/vh55-3he6", "Flu Vaccinations by State", target="_blank")
                                         
                                )
                        )
                      )
                    )
)

server <- function(input, output, session) {
  output$plot1 = renderPlot({
    covid_df <- read_csv("covid_state_data.csv")
    
    plot_data = covid_df %>%
      filter(party == 'Republican') %>%
      summarize(repubAdministered = sum(shots_given))
    
    us_shots_given = covid_df %>%
      filter(state_abr == 'US')
    
    us_shots_given = us_shots_given$shots_given
    
    RepublicanPercent = (plot_data/us_shots_given)$repubAdministered
    DemocratPercent = 1 - RepublicanPercent
    
    data <- list("Living in Republican States"=RepublicanPercent, "Living in Democratic States"=DemocratPercent)
    x <- personograph(data, icon.style=3, colors=list("Living in Democratic States"="#2E74C0", "Living in Republican States"="#CB454A"),
                      draw.legend = T, plot.width = 0.6)
    
    return(x)
  }, bg="#ecf0f5", execOnResize=T)
  
  us_states_elec <- read_csv("covid_state_data_gps.csv")
  us_counties_elec <- read_csv("covid_county_data_gps.csv")
  us_states_flu <- read_csv("flu_state_data_gps.csv")
  
  party_colors <- c("#2E74C0", "#CB454A")
  party_colors_with_shots <- c("#2E74C0", "#A2C4E9", "#CB454A", "#E9B0B2")
  
  text_reactive <- reactiveValues(
    text = c("FALSE", "FALSE", "State")
  )
  
  observeEvent(input$CheckboxCovid, {
    if (input$CheckboxCovid == TRUE) {
      updateCheckboxInput(session=session, "CheckboxFlu", value=FALSE)
      updateCheckboxInput(session=session, "CheckboxCovid", value=TRUE)
      
      mapTypeSelected <- input$mapType 
      
      updateSelectInput(session, "mapType",
                        choices = list("By State" = "State", "By County" = "County"),
                        selected = mapTypeSelected
      )
      
    }
    text_reactive$text <- c(input$CheckboxCovid, input$CheckboxFlu, input$mapType)
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$CheckboxFlu, {
    if (input$CheckboxFlu == TRUE) {
      updateCheckboxInput(session=session, "CheckboxFlu", value=TRUE)
      updateCheckboxInput(session=session, "CheckboxCovid", value=FALSE)
      
      updateSelectInput(session, "mapType",
                        choices = list("By State" = "State")
      )
    }
    else if (input$CheckboxFlu == FALSE) {
      updateSelectInput(session, "mapType",
                        choices = list("By State" = "State", "By County" = "County"))
    }
    text_reactive$text <- c(input$CheckboxCovid, input$CheckboxFlu, input$mapType)
  }, ignoreInit = TRUE)
  
  observeEvent(input$mapType, {
    text_reactive$text <- c(input$CheckboxCovid, input$CheckboxFlu, input$mapType)
  }, ignoreInit = TRUE)
  
  output$text <- renderText({
    # text_reactive$text
  })
  
  output$plot2 = renderPlot({
    # Election 2020: State
    if (text_reactive$text[1] == "FALSE" & text_reactive$text[2] == "FALSE" & text_reactive$text[3] == "State") {
      output$map_title <- renderText({"2020 Presidential Election Results by State"})
      return(us_states_elec %>% ggplot(aes(
        x = long, y = lat,
        fill=party,
        group = group)) +
          geom_polygon(color = "gray90", size = 0.1) +
          coord_equal() + 
          coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
          theme_map() +
          #theme_map(base_size = 22) +
          theme(
            legend.position = 'bottom',
            plot.background = element_rect(fill = '#ecf0f5', color="#ecf0f5"),
            legend.background = element_rect(fill = '#ecf0f5', color="#ecf0f5")
          ) +
          guides(fill = guide_legend(title.position="top", nrow = 2, byrow = FALSE)) + 
          scale_fill_manual(values = party_colors,
                            labels = c("Biden", "Trump"),
                            breaks = c("D", "R"),
                            drop = FALSE,
                            name="States' Results") +
          labs(title = "",
               subtitle = "",
               fill = NULL)
      )
    }
    # Election 2020: County
    else if (text_reactive$text[1] == "FALSE" & text_reactive$text[2] == "FALSE" & text_reactive$text[3] == "County") {
      output$map_title <- renderText({"2020 Presidential Election Results by County"})
      return(us_counties_elec %>% ggplot(aes(
        x = long, y = lat, 
        fill = party,
        group = group)) + 
          geom_polygon(color = "gray90", size = 0.1) + 
          coord_equal() +
          theme_map() +
          theme(
            legend.position = 'bottom',
            plot.background = element_rect(fill = '#ecf0f5', color="#ecf0f5"),
            legend.background = element_rect(fill = '#ecf0f5', color="#ecf0f5")
          ) +
          guides(fill = guide_legend(title.position="top", nrow = 2, byrow = FALSE)) + 
          scale_fill_manual(values = party_colors,
                            labels = c("Biden", "Trump"),
                            breaks = c("D", "R"),
                            drop = FALSE,
                            name="Counties' Results") +
          labs(title = "",
               subtitle = "",
               fill = NULL)
      )
    }
    # Covid: State
    else if (text_reactive$text[1] == "TRUE" & text_reactive$text[3] == "State") {
      output$map_title <- renderText({"No Red States were above the National Average"})
      return(us_states_elec %>% ggplot(aes(
        x = long, y = lat,
        fill=interaction(party, over_under),
        group = group)) +
          geom_polygon(color = "gray90", size = 0.1) +
          coord_equal() + 
          coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
          theme_map() +
          theme(
            legend.position = 'bottom',
            plot.background = element_rect(fill = '#ecf0f5', color="#ecf0f5"),
            legend.background = element_rect(fill = '#ecf0f5', color="#ecf0f5")
          ) +
          guides(fill = guide_legend(title.position="top", nrow = 2, byrow = FALSE)) +
          scale_fill_manual(values = party_colors_with_shots,
                            labels = c("Biden States: Over 67.1%", "Biden States: Under 67.1%", "Trump States: Over 67.1%", "Trump States: Under 67.1%"),
                            breaks = c("D.O", "D.U", "R.O", "R.U"),
                            drop = FALSE,
                            name="States' COVID-19 Vaccination Rates vs US Average: 67.1%") +
          labs(title = "", 
               subtitle = "",
               fill = NULL)
      )
    }
    # COVID: County
    else if (text_reactive$text[1] == "TRUE" & text_reactive$text[3] == "County") {
      output$map_title <- renderText({"Some Red Counties were above the National Average"})
      return(us_counties_elec %>% ggplot(aes(
        x = long, y = lat, 
        fill=interaction(party, over_under),
        group = group)) + 
          geom_polygon(color = "gray90", size = 0.1) + 
          coord_equal() +
          theme_map() + 
          theme(
            legend.position = 'bottom',
            plot.background = element_rect(fill = '#ecf0f5', color="#ecf0f5"),
            legend.background = element_rect(fill = '#ecf0f5', color="#ecf0f5")
          ) +
          guides(fill = guide_legend(title.position="top", nrow = 2, byrow = FALSE)) +
          scale_fill_manual(values = party_colors_with_shots,
                            labels = c("Biden Counties: Over 67.1%", "Biden Counties: Under 67.1%", "Trump Counties: Over 67.1%", "Trump Counties: Under 67.1%"),
                            breaks = c("D.O", "D.U", "R.O", "R.U"),
                            drop = FALSE,
                            name="Counties' COVID-19 Vaccination Rates vs US Average: 67.1%") +
          labs(title = "",
               subtitle = "",
               fill = NULL)
      )
    }
    # Flu: State
    else if (text_reactive$text[2] == "TRUE" & text_reactive$text[3] == "State") {
      output$map_title <- renderText({"Red States are not \"Anti-Vaccine\" as much as they are \"Anti-COVID-19 Vaccine\""})
      show(us_states_flu %>% ggplot(aes(
        x = long, y = lat,
        fill=interaction(party, over_under),
        group = group)) +
          geom_polygon(color = "gray90", size = 0.1) +
          coord_equal() + 
          coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
          theme_map() +
          theme(
            legend.position = 'bottom',
            plot.background = element_rect(fill = '#ecf0f5', color="#ecf0f5"),
            legend.background = element_rect(fill = '#ecf0f5', color="#ecf0f5")
          ) +
          guides(fill = guide_legend(title.position="top", nrow = 2, byrow = FALSE)) + 
          scale_fill_manual(values = party_colors_with_shots,
                            labels = c("Biden States: Over 39.4%", "Biden States: Under 39.4%", "Trump States: Over 39.4%", "Trump States: Under 39.4%"),
                            breaks = c("D.O", "D.U", "R.O", "R.U"),
                            drop = FALSE,
                            name="States' 2018-2019 Flu Vaccination Rates for 18+ vs US Average: 39.4%") +
          labs(title = "",
               subtitle = "",
               fill = NULL)
      )
    }
  }, bg="#ecf0f5", execOnResize=T)
  
  covid <- read_csv("covid_state_data.csv") %>%
    select(-c("over_under"))
  
  #Calculate eligible population in each state
  covid <- mutate(covid, eligible = shots_given/(shot_pct/100))
  
  #Calculate total eligible red population
  Eligible_Red <- covid %>% 
    filter(party == 'Republican') %>%
    summarize(sum(eligible))
  
  #Calculate total eligible blue population
  Eligible_Blue <- covid %>%
    filter(party == 'Democrat') %>%
    summarize(sum(eligible)) 
  
  #Subtract US total which is listed as democrat
  Eligible_Blue <- Eligible_Blue - covid[52,6]
  
  #Calculate total vaccinated red population
  Vaccinated_Red <- covid %>% 
    filter(party == 'Republican') %>%
    summarize(sum(shots_given))
  
  #Calculate total vaccinated blue population
  Vaccinated_Blue <- covid %>% 
    filter(party == 'Democrat') %>%
    summarize(sum(shots_given))
  
  #Subtract US which is listed as democrat
  Vaccinated_Blue <- Vaccinated_Blue - covid[52,5]
  
  #Calculate percentage of red state population vaccinated
  Red_State_Avg <- Vaccinated_Red / Eligible_Red * 100
  
  #Calculate percentage of blue state population vaccinated
  Blue_State_Avg <- Vaccinated_Blue / Eligible_Blue * 100
  
  #Add a row for red state average
  red_row <- data.frame("RD", "Red State Average", "Republican", Red_State_Avg, Vaccinated_Red, Eligible_Red)
  names(red_row) <- c("state_abr", "state", "party", "shot_pct", "shots_given", "eligible")
  covid <- rbind(covid, red_row)
  
  #Add a row for blue state average
  blue_row <- data.frame("BL", "Blue State Average", "Democrat", Blue_State_Avg, Vaccinated_Blue, Eligible_Blue)
  names(blue_row) <- c("state_abr", "state", "party", "shot_pct", "shots_given", "eligible")
  covid <- rbind(covid, blue_row)
  
  #Make the percentages decimals
  covid <- covid %>%
    mutate(shot_pct = shot_pct / 100)
  
  #Build Graph
  
  data <- reactive({
    req(input$sel_state1)
    covid <- covid %>%
      filter(state == input$sel_state1 | state == input$sel_state2)
  })
  observe({
    updateSelectInput(session, "sel_state1", choices = covid$state)
  })
  observe({
    updateSelectInput(session, "sel_state2", choices = covid$state)
  })
  output$plot3 = renderPlot({
    Bar <-  ggplot(data(), aes(x = state, y = shot_pct, fill = party)) + 
      geom_bar(stat = "identity", width = 0.1) +
      scale_fill_manual(values = c("Democrat" = "#2E74C0",
                                   "Republican" = "#CB454A")) +
      geom_text(aes(label = scales::percent(shot_pct)), position = position_nudge(y = 0.03)) +
      scale_x_discrete(limits =  c(input$sel_state1, input$sel_state2)) +
      ggtitle("State Vaccination Rates") + 
      theme(plot.title = element_text(hjust = 0.6)) +
      xlab("State") + ylab("Vaccinated") + labs(fill = "Party") +
      scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,1)) +
      geom_hline(aes(yintercept= 0.671, linetype = "National US Average: 67.1%"), color = "black") +
      geom_hline(aes(yintercept = .591, linetype = "Democratic Average: 71.8%"), color = "#CB454A") +
      geom_hline(aes(yintercept= .718, linetype = "Republican Average: 59.1%"), color = "#2E74C0") +
      scale_linetype_manual(name = "", values = c("dashed", "dashed", "dashed"),
                            guide = guide_legend(override.aes = list(color = c("#2E74C0", "black", "#CB454A"), 
                                                                     guide_legend(reverse = TRUE))))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(color = "black"),
            plot.background = element_rect(fill = '#ecf0f5', color="#ecf0f5"),
            legend.background = element_rect(fill = '#ecf0f5', color="#ecf0f5"),
            legend.key = element_rect(fill = "#ecf0f5"))
    Bar
  })
  
}

shinyApp(ui = ui, server = server)
