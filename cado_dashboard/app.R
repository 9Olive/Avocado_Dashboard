#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(tidyverse)
library(prophet)
library(lubridate)
library(knitr)

# Download and read in data
file <- url('https://drive.google.com/uc?id=1b-K8NChme-5ku26ADdr5OmH0-efoiB7m')
file <- read_csv(file)

# Save these as options for app
regions <- sort(unique(file$region))
cado_type <- sort(unique(file$type))

theme_set(theme_light() + theme(text = element_text(size = 18)))

# Define UI for application that draws a histogram
ui <- fluidPage(
        
    fluidRow(
        column(width = 12,
               h1('Avocados Price Forecast Dashboard',
                  style = "font-weight: 500; 
                            color: #336600; 
                            text-align:center;
                            text-shadow: 1px 1px 0 #444")
               )
    ),
    
    fluidRow(
        column(
            width = 6, offset = 3,
            p(strong('Description:'), ' This dashboard is based on an analysis I performed on a US avacado price dataset. 
                My exploratory data analysis and modeling thought process were shared in the posts linked below. In summary, 
                data on the ', strong('average sale price'), ' of indivdual avocados were culled together from Q1 2015 - Q1 2018 
                for about 50 large US markets. A Q2 2018 forecast is modeled using a general additive regression technique from 
                a time series package called ', em('Prophet.') 
              ),
            p(strong('Link to posts: '), '\n'),
            tags$p(
                HTML(
                    "<link rel = 'stylesheet' href = 'https://www.w3schools.com/w3css/4/w3.css'>
                    <ul class = 'w3-ul w3-card-4'>
                        <li class = 'w3-bar'>
                            <a href = 'https://9olive.github.io/blog/2020/04/20/avocado-pt1.html'
                                style = 'text-decoration:none;display:block;'
                                target = '_blank'
                            >
                                <span class='w3-bar-item w3-button w3-xlarge w3-right'>
                                    &rarr;
                                </span>
                            </a>
                            <img 
                                src='https://raw.githubusercontent.com/9Olive/9Olive.github.io/master/img/avos.png' 
                                class='w3-bar-item w3-circle' style='width:85px'>
                            <div class='w3-bar-item'>
                            <span class='w3-large'>
                                Exploratory Data Analysis
                            </span>
                            <br>
                            <span>
                                EDA thought process and visualizing the dataset through R code
                            </span>
                            </div>
                        </li>
                        <li class = 'w3-bar'>
                            <a href = 'https://9olive.github.io/blog/2020/05/08/avocados-pt2.html'
                                style = 'text-decoration:none;display:block;'
                                target = '_blank'
                            >
                                <span class='w3-bar-item w3-button w3-xlarge w3-right'>
                                    &rarr;
                                </span>
                            </a>
                            <img 
                                src='https://raw.githubusercontent.com/9Olive/9Olive.github.io/master/img/modeling.png' 
                                class='w3-bar-item w3-circle' style='width:85px'>
                            <div class='w3-bar-item'>
                            <span class='w3-large'>
                                Exploratory Data Modeling
                            </span>
                            <br>
                            <span>
                                Explored basic linear model assumption and model selection process
                            </span>
                            </div>
                        </li>
                    </ul>"
                )
            ),
            br(),
            )
        ),
    
    # Main top theme
    sidebarLayout(
        
        sidebarPanel(
            width = 5,
            h3('Criteria Selection:'),
            p('Select a region and avocado type. A model will generated to forecast the price of Q2 2018 avocados.'),
            hr(),
                 selectInput(inputId = 'region_select', label = 'Select Region', choices = regions),
                 br(),
                 selectInput(inputId = 'type_select', label = 'Select Avocado Type', choices = cado_type),
                 br(),
                 downloadButton(outputId = 'download', label = 'Download Forecast')
            ),
        mainPanel(
            width = 7,
            plotOutput(outputId = 'plot_model', width = '100%')
                  )
    ),
    fluidRow(column(width = 7, offset = 4, mainPanel(tableOutput(outputId = 'dt_model'))))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot_model <- renderPlot(
        {
            file_fcst <- (file_mod <- file %>%
                              filter(region == input$region_select & type == input$type_select) %>%
                              select(Date, AveragePrice) %>%
                              rename(ds = Date, y = AveragePrice) %>%
                              prophet()) %>%
                make_future_dataframe(periods = 12, freq = 'week')
            
            file_fcst <- predict(file_mod, file_fcst)
            
            file_mod$history %>%
                select(ds, y) %>% 
                full_join(file_fcst, by = 'ds') %>%
                arrange(ds) %>%
                mutate('Model Data' = ifelse(
                           as_date(ds) > max(as_date(file_mod$history$ds)),
                           'Predicted', 'Fitted'
                       ),
                       `Model Data` = as.factor(`Model Data`)) %>%
                ggplot(aes(x = as_date(ds), y = y)) +
                geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = '#0072B2', alpha = 0.2, na.rm = T) +
                geom_point(na.rm = T, size = .8) +
                geom_line(aes(y = yhat), na.rm = T) +
                geom_line(aes(y = yhat, color = `Model Data`), na.rm = T) +
                labs(title = paste0(input$region_select, "'s Model for ", input$type_select, ' Avocados'), subtitle = 'Forecasted out 12 weeks',
                     x = '', y = 'Average Price') +
                scale_x_date(breaks = pretty_breaks(n = 24)) +
                scale_y_continuous(breaks = pretty_breaks(n = 10), labels = dollar_format()) +
                scale_color_manual(values = c('#0072B2', '#FC4E07')) +
                theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.45))
            })
    
    output$dt_model <- renderTable(
        {
            (
                (
                    file %>%
                        filter(region == input$region_select & type == input$type_select) -> filtered_file) %>%
                    select(Date, AveragePrice) %>%
                    rename(ds = Date, y = AveragePrice) %>%
                    prophet() -> file_mod
                ) %>%
                
                make_future_dataframe(periods = 12, freq = 'week') -> file_fcst
            
            predict(file_mod, file_fcst)  %>% 
                select(ds, yhat_lower, yhat_upper, yhat) %>%
                mutate(ds = as_date(ds)) %>%
                
                left_join(
                    (
                        filtered_file %>% 
                            mutate(Date = as_date(Date))
                        ), by = c('ds' = 'Date')
                    ) %>%
                
                fill(region, type, .direction = 'down') %>%
                rename(Date = ds,
                       'Forecasted Price' = yhat,
                       'Lower Uncerntainty Bound' = yhat_lower,
                       'Upper Uncerntainty Bound' = yhat_upper,
                       Region = region,
                       'Avocado Type' = type) %>%
                select(Date, Region, `Avocado Type`, `Forecasted Price`, `Lower Uncerntainty Bound`, `Upper Uncerntainty Bound`) %>%
                tail(12) %>%
                mutate(Date = as.character(Date)) %>%
                mutate_if(is.numeric, dollar)
            }
        )
    
    output$download <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
          ((file %>%
                filter(region == input$region_select & type == input$type_select) -> filtered_file) %>%
              select(Date, AveragePrice) %>%
              rename(ds = Date, y = AveragePrice) %>%
              prophet() -> file_mod) %>%
        make_future_dataframe(periods = 12, freq = 'week') -> file_fcst
    predict(file_mod, file_fcst)  %>% 
        select(ds, yhat_lower, yhat_upper, yhat) %>%
        mutate(ds = as_date(ds)) %>%
        left_join((filtered_file %>% mutate(Date = as_date(Date))), by = c('ds' = 'Date')) %>%
        fill(region, type, .direction = 'down') %>%
        rename(Date = ds,
               'Forecasted Price' = yhat,
               'Lower Uncerntainty Bound' = yhat_lower,
               'Upper Uncerntainty Bound' = yhat_upper,
               Region = region,
               'Avocado Type' = type) %>%
        select(Date, Region, `Avocado Type`, `Forecasted Price`, `Lower Uncerntainty Bound`, `Upper Uncerntainty Bound`) %>%
        tail(12) %>%
        mutate(Date = as.character(Date)) -> data_dl
        write.csv(data_dl, con, row.names = F)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
