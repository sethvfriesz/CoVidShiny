library(shiny)
library(plotly)
library(readxl)
library(tidyverse)
library(ggplot2)
library(anytime)
library(shinyWidgets)

US_counties = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')

Census <- read_excel(paste0(getwd(), "/Census.xlsx"))


x = US_counties[2,1]
as.character(substr(x, 4, 8))
length(US_counties$UID)
for (x in 1:length(US_counties$UID)){
  US_counties[x,5] = as.character(substr(US_counties[x,1], 4, 8))
}

US_counties[2586,6] = "Shannon"
US_counties[87,6] = "Wade Hampton"

US_counties[2586,5] ='46113'
US_counties[87,5]  = '02270'
US_counties_dates <- US_counties[,c(12:ncol(US_counties))]
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('blue')
)

US_states_14_days_final = Census[6:nrow(Census), 'NAME']
US_counties_14_days = US_counties[, seq_len(ncol(US_counties)) %% 14 == 7]

for (x in unique(Census$NAME)){
  for (y in 2:ncol(US_counties_14_days)){
    #print(x)
    #print(y)
    
    US_states_14_days_final[US_states_14_days_final$NAME == x,y] = sum(US_counties_14_days[US_counties_14_days$Province_State == x, y]) / (Census[Census$NAME == x,6]/100000)
    
    
  }
  
}

list_of_names = c('State', '01/31/2020','02/14/2020','02/28/2020','03/13/2020','03/27/2020','04/10/2020','04/24/2020','05/08/2020','05/22/2020','06/05/2020','06/19/2020','07/3/2020'
                  ,'07/17/2020','07/31/2020','08/14/2020','08/28/2020','09/11/2020','09/25/2020','10/9/2020','10/23/2020','11/6/2020','11/20/2020','12/4/2020',
                  '12/18/2020','01/01/2021','1/15/2021','1/29/2021','2/12/2021','2/26/2021','3/12/2021','3/26/2021','4/9/2021','4/23/2021','5/7/2021','5/21/2021'
                  ,'6/4/2021','6/18/2021','7/2/2021','7/16/2021','7/30/2021','8/13/2021','8/27/2021','9/10/2021','9/24/2021', '10/8/2021', '10/22/2021'
                  ,'11/5/2021', '11/19/2021','12/03/2021', '12/17/2021','12/31/2021')


colnames(US_states_14_days_final)<- list_of_names

states_longer = US_states_14_days_final %>% pivot_longer(!State, names_to = "date", values_to = "count")
states_longer$date <- anydate(states_longer$date)

################################################################

ui <- navbarPage(
  "Covid Dashboard",
  tabPanel(
    "Covid since patient zero across the U.S.",
    sidebarLayout(
      sidebarPanel(
        selectInput('date','Date', choices = colnames(US_counties_dates))
      ),
      mainPanel(
        plotlyOutput(outputId = "p")
      )
    )
  ),
  tabPanel("Covid counts over time per state",
           sidebarLayout(
             sidebarPanel(
               pickerInput("state","State", choices=unique(states_longer['State']),selected = 'South Dakota', options = list(`actions-box` = TRUE),multiple = T)
             ),
             mainPanel(
               plotOutput(outputId = 'q')
             )
           )
  )
  
  
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(text = ~paste(US_counties$Combined_Key,'infection count:',US_counties[,input$date])) %>%
      add_trace(
        type="choropleth",
        geojson=counties,
        locations=US_counties$FIPS,
        z=US_counties[,input$date],
        colorscale="heat",
        zmin= min(US_counties[,input$date]),
        #zmax = 6000,
        zmax=(quantile(US_counties[,input$date])[4]) + (6*((quantile(US_counties[,input$date])[4])-(quantile(US_counties[,input$date])[2]))),   # Getting non-binary error here
        marker=list(line=list(
          width=0)
          
        )
      ) %>% 
      colorbar(title = "Total Number of COVID-19 infections") %>%
      layout(
        title = "COVID-19 infections by county on "
      ) %>%
      layout(
        geo = g
      )
    
  })
  output$q <- renderPlot({
    ggplot(
      states_longer[states_longer$State %in% input$state,],
      aes(x = date, y = count, group = State, color = State)
    ) + 
      geom_line() +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_date(date_breaks = 'month' , date_labels = "%b-%y")
  })
  
}

shinyApp(ui, server)

