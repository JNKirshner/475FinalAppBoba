library(fpp3)
library(shiny)
library(seasonal)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinythemes)


# Path where data is
file_path <- 'multiTimeline.csv'

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)
#autoplot(g_trends)


ui <- fluidPage(
  theme = shinytheme('united'),
navbarPage('Analysis of Boba Bubble Tea',
  tabPanel( 'Time Series',
  sidebarLayout(
    sidebarPanel( titlePanel( 'Time Series of Boba Bubble Tea'),
        checkboxInput("checkbox", label = "Forecast?", value = TRUE),
        selectInput(inputId = 'selected_fc',
                    label = 'select forecast method',
                    choices = c('Naive','Seasonal Naive','Mean', 'Drift', 'TSLM')),
        sliderInput('h','Years to Forecast',min = 1,max = 10,value = 3),
        dateRangeInput(
          inputId = 'selected_daterange',
          label = 'Select Date Range',
          start = min(g_trends$Month),
          end = max(g_trends$Month)),),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot', plotOutput('df_plot'))
      )
    )    
        
        )
  ),
  tabPanel("Seasonality, Decomposition, and Autocorrelation",
    sidebarLayout(
      sidebarPanel(  
      selectInput(inputId = "select", label = h3("Select a Plot"), 
                              choices = c("Seasonality", "Decomposition", "Autocorrelation")), 
                  dateRangeInput(
                                inputId = 'selected_daterange',
                                label = 'Select Date Range',
                                start = min(g_trends$Month),
                                end = max(g_trends$Month))),
 mainPanel(
   tabPanel('Plot',plotOutput('three'))
 ) 
)),
tabPanel(
  'Holt-Winters',
  sidebarLayout(
    sidebarPanel(
      titlePanel('Holt-Winters'),
      selectInput(inputId = 'selected_HW',
                  label = 'select Holt or Holt-Winters',
                  choices = c('Holt','Holt-Winters')),
      sliderInput('h2','Years to Forecast',min = 1,max = 10,value = 3),
      dateRangeInput(
        inputId = 'selected_date_range_HW',
        label = "Select date range",
        format = "mm/yyyy",
        start = min(g_trends$Month),
        end = max(g_trends$Month))),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot',plotOutput('Holt_Winters'))
      )
    )
  )
),
tabPanel(
  'Arima',
  sidebarLayout(
    sidebarPanel(
      titlePanel('Arima'),
      selectInput(inputId = 'selected_Arima',
                  label = 'select Arima model',
                  choices = c('Auto','(2,1,3)','(1,1,0)')),
      sliderInput('h3','Years to Forecast',min = 1,max = 6,value = 1),
      dateRangeInput(
        inputId = 'selected_date_range_Arima',
        label = "Select date range",
        format = "mm/yyyy",
        start = min(g_trends$Month),
        end = max(g_trends$Month))),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot',plotOutput('Arima'))
      )
    )
  )
)
))
  
  
  
 

server <- function(input, output, session) {
  
  
  output$ts_plot <- renderPlot({
    min_date <- input$selected_daterange[1]
    max_date <- input$selected_daterange[2]
    df_trends <- g_trends %>%
      filter(Month >= min_date, Month <= max_date)
    if (input$checkbox) {
      df_trends %>%
        model(TSLM(Interest~ trend()+season())) %>% 
        forecast() %>%
        autoplot(df_trends)
    } else {
      autoplot(df_trends)
    }
    
  })
  output$three<- renderPlot({
    min_date <- input$selected_daterange[1]
    max_date <- input$selected_daterange[2]
    df_trends <- g_trends %>%
      filter(Month >= min_date, Month <= max_date)
    if (input$select == 'Decomposition') {
      df_trends %>%
        model(
          classical_decomposition(Interest, type = "additive")
        ) %>%
        components() %>%
        autoplot() +
        labs(title = "Classical additive decomposition of total
                  Boba bubble tea interest")}
    else if (input$select == 'Seasonality') { gg_season(df_trends)}
    else if (input$select == 'Autocorrelation') {autoplot(ACF(df_trends))}
  })
  
  
  output$df_plot <- renderPlot({
    
    min_date <- input$selected_daterange[1]
    max_date <- input$selected_daterange[2]
    
    plot_df <- g_trends[g_trends$Month >= min_date,]
    plot_df <- plot_df[plot_df$Month <= max_date,]
    
    if (input$selected_fc == 'TSLM' & input$checkbox) {
      fit <- plot_df %>% 
        model(TSLM(Interest~trend()+season()))
      
      fit %>% 
        forecast(h=(input$h*12)) %>% 
        autoplot(plot_df, level = NULL)
    }else if(input$selected_fc == 'Naive' & input$checkbox){
      fit <- plot_df %>% 
        model(NAIVE())
      
      fit %>% 
        forecast(h=(input$h*12)) %>% 
        autoplot(plot_df)
    }else if(input$selected_fc == 'Seasonal Naive' & input$checkbox){
      fit <- plot_df %>% 
        model(SNAIVE())
      
      fit %>% 
        forecast(h=(input$h*12)) %>% 
        autoplot(plot_df)
    }else if(input$selected_fc == 'Mean' & input$checkbox){
      fit <- plot_df %>% 
        model(MEAN())
      
      fit %>% 
        forecast(h=(input$h*12)) %>% 
        autoplot(plot_df)
    }else if(input$selected_fc == 'Drift' & input$checkbox){
      fit <- plot_df %>% 
        model(RW(~drift()))
      
      fit %>% 
        forecast(h=(input$h*12)) %>% 
        autoplot(plot_df)
    }else{
      autoplot(plot_df)
    }
  })
  
  
  output$Holt_Winters <- renderPlot({
    min_date <- input$selected_date_range_HW[1]
    max_date <- input$selected_date_range_HW[2]
    
    plot_df <- g_trends[g_trends$Month >= min_date,]
    plot_df <- plot_df[plot_df$Month <= max_date,]
    
    if(input$selected_HW == 'Holt' & input$checkbox){
      fit <- plot_df %>% 
        model(ETS(~error("A") + trend("A") + season("N")))
      
      fit %>% 
        forecast(h=(input$h2*12)) %>% 
        autoplot(g_trends)
    }else if (input$selected_HW == 'Holt-Winters' & input$checkbox){
      fit <- plot_df %>% 
        model(ETS(~ error("A") + trend("A") +
                    season("A")))
      
      fit %>% 
        forecast(h=(input$h2*12)) %>% 
        autoplot(plot_df)
    }else{
      autoplot(plot_df)}
  })
  
  output$Arima <- renderPlot({
    min_date <- input$selected_date_range_Arima[1]
    max_date <- input$selected_date_range_Arima[2]
    
    plot_df <- g_trends[g_trends$Month >= min_date,]
    plot_df <- plot_df[plot_df$Month <= max_date,]
    
    if(input$selected_Arima == 'Auto'){
      fit <- plot_df %>% 
        model(ARIMA())
      
      fit %>% 
        forecast(h=(input$h3*12)) %>% 
        autoplot(plot_df)
    }else if (input$selected_Arima == '(2,1,3)'){
      fit <- plot_df %>% 
        model(ARIMA())
      
      fit %>% 
        forecast(h=(input$h3*12)) %>% 
        autoplot(plot_df)
    }else if (input$selected_Arima == '(1,1,0)'){
      fit <- plot_df %>% 
        model(ARIMA())
      
      fit %>% 
        forecast(h=(input$h3*12)) %>% 
        autoplot(plot_df)
    }
  })
    
}
shinyApp(ui, server)
