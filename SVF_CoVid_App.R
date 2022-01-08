library(shiny)
library(plotly)
library(readxl)
library(tidyverse)
library(ggplot2)
library(anytime)
library(shinyWidgets)
library(shinybusy)
library(shinysky)


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
#url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
#counties <- rjson::fromJSON(file=url)
# g <- list(
#   scope = 'usa',
#   projection = list(type = 'albers usa'),
#   showlakes = TRUE,
#   lakecolor = toRGB('blue')
# )

US_states_14_days_final = Census[6:nrow(Census), 'NAME']

###### Change starts here #########
US_counties_14_days = US_counties[, seq_len(ncol(US_counties)) %% 14 == 7]
for (x in unique(Census$NAME)){
  for (y in 2:ncol(US_counties_14_days)){
    #print(x)
    #print(y)
    US_states_14_days_final[US_states_14_days_final$NAME == x,y] = sum(US_counties_14_days[US_counties_14_days$Province_State == x, y])
    
    
  }
  
}


list_of_names = c('State', '01/31/2020','02/14/2020','02/28/2020','03/13/2020','03/27/2020','04/10/2020','04/24/2020','05/08/2020','05/22/2020','06/05/2020','06/19/2020','07/3/2020'
                  ,'07/17/2020','07/31/2020','08/14/2020','08/28/2020','09/11/2020','09/25/2020','10/9/2020','10/23/2020','11/6/2020','11/20/2020','12/4/2020',
                  '12/18/2020','01/01/2021','01/15/2021','01/29/2021','2/12/2021','2/26/2021','3/12/2021','3/26/2021','4/9/2021','4/23/2021','5/7/2021','5/21/2021'
                  ,'6/4/2021','6/18/2021','7/2/2021','7/16/2021','7/30/2021','8/13/2021','8/27/2021','9/10/2021','9/24/2021', '10/8/2021', '10/22/2021'
                  ,'11/5/2021', '11/19/2021','12/3/2021', '12/17/2021','12/31/2021')

colnames(US_states_14_days_final)<- list_of_names

states_longer = US_states_14_days_final %>% pivot_longer(!State, names_to = "date", values_to = "overall")
states_longer$date <- anydate(states_longer$date)

# for loop to get per 100,000
for (x in unique(states_longer$State)){
  states_longer[states_longer$State == x, "state_population"] = Census[Census$NAME == x, "POPESTIMATE2019"]
}

states_longer$per_hundred_thousand = (states_longer$overall / (states_longer$state_population/100000))


# ##############US_country plot Function#######################
testing <- function(dataset, date) {
  plotly::plot_ly(text = ~paste(US_counties$Combined_Key,'infection count:',US_counties[,date])) %>%
    plotly::add_trace(
      type="choropleth",
      geojson=rjson::fromJSON(file='https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'),
      locations=US_counties$FIPS,
     z=US_counties[,date],
      colorscale="heat",
      zmin= min(US_counties[,date]),
      zmax=(quantile(US_counties[,date])[4]) + (6*((quantile(US_counties[,date])[4])-(quantile(US_counties[,date])[2]))),
      marker=list(line=list(
        width=0)

      )
    ) %>%
    colorbar(title = "Total Number of COVID-19 infections") %>%
    layout(
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
       lakecolor = plotly::toRGB('blue')
      )
   )
}




###############################################################
##################### Infections Plot##########################
infections_plot <- function(dataset, plot_choice, states){
  if (plot_choice == "Overall infections over 2 week periods"){
    plotly::ggplotly(ggplot2::ggplot(
      dataset[dataset$State %in% states,],
      ggplot2::aes(x = date, y = overall, group = State, color = State))+
      ggplot2::geom_line() +
      ggplot2::labs(color = "STATES:  ") +
      ggplot2::ylab("Overall CoVid-19 infections") +
      ggplot2::xlab("Date")+
      ggplot2::theme(axis.text.x = element_text(angle = 90, size = 16),
                     axis.title.x = element_text(size = 25, face = "bold"),
                     axis.text.y = element_text(size = 16, angle = 45),
                     axis.title.y = element_text(size = 25, face = "bold"),
                     legend.key.size = unit(1,"cm"),
                     legend.text = element_text(size = 14),
                     legend.position = "bottom",
                     legend.box = "vertical",
                     legend.title = element_text(color = "black", size = 20, face = "bold")) +
      ggplot2::scale_x_date(date_breaks = 'month' , date_labels = "%b-%y") +
      ggplot2::guides(size = "none"))
  }
  else if (plot_choice == "Overall infections per 100,000 population over 2 week periods") {
    plotly::ggplotly(ggplot2::ggplot(
      dataset[dataset$State %in% states,],
      ggplot2::aes(x = date, y = per_hundred_thousand, group = State, color = State))+
      ggplot2::geom_line() +
      ggplot2::ylab("Overall CoVid-19 infections per 100,000 population") +
      ggplot2::xlab("Date")+
      ggplot2::theme(axis.text.x = element_text(angle = 90, size = 16),
                     axis.title.x = element_text(size = 20, face = "bold"),
                     axis.text.y = element_text(size = 16, angle = 45),
                     axis.title.y = element_text(size = 20, face = "bold"),
                     legend.key.size = unit(1,"cm"),
                     legend.text = element_text(size = 14),
                     legend.position = "bottom",
                     legend.box = "vertical",
                     legend.title = element_text(color = "black", size = 20, face = "bold")) +
      ggplot2::scale_x_date(date_breaks = 'month' , date_labels = "%b-%y") +
      ggplot2::guides(size = "none"))
  }
}


################################################################

ui <- navbarPage(
  "CoVid-19 Dashboard",
  tabPanel(
    "CoVid-19 since patient zero across the U.S.",
    sidebarLayout(
      sidebarPanel(
        selectInput('date','Please select a date:', choices = colnames(US_counties_dates))
      ),
      mainPanel(
        shinysky::busyIndicator(text = "Give me a sec brudda", wait = 1000 ),
        #add_busy_spinner(spin = "fading-circle"),
        h1("CoVid-19 since patient zero across the U.S."),
        plotlyOutput(outputId = "p", height = "800px")
      )
    )
  ),
  tabPanel("CoVid-19 infections over time by State",
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "measurement",
                 label = "Please pick a way to measure CoVid-19 infections:",
                 choices = c("Overall infections over 2 week periods", "Overall infections per 100,000 population over 2 week periods")),
               pickerInput(
                 inputId = "state",
                 label = "Please select one or more state(s):",
                 choices=unique(states_longer['State']),
                 selected = 'South Dakota',
                 options = list(`actions-box` = TRUE),
                 multiple = T)
             ),
             mainPanel(
               shinysky::busyIndicator(text = "Give me a sec brudda", wait = 1000 ),
               h1("CoVid-19 infections over time by State"),
               plotlyOutput(
                 outputId = 'state_plot',
                 height = "800px",
                 width = "1150px")
             )
           )
  )
  
  
)

server <- function(input, output, session) {
  
  output$p <- plotly::renderPlotly({
    dataplots = testing(
      dataset = US_counties,
      date = input$date)
  })
  output$state_plot <- plotly::renderPlotly({
    dataplot = infections_plot(
      dataset = states_longer,
      plot_choice = input$measurement,
      states = input$state)
  })
  
}

shinyApp(ui, server)