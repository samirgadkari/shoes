library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(vroom)

mens_shoes_summary <- vroom("processed/mens_shoes_summary.csv")
date_updated_min <- min(mens_shoes_summary$dateupdated)
date_updated_max <- max(mens_shoes_summary$dateupdated)
mens_shoes_with_type <- vroom("processed/mens_shoes_with_type.csv")
shoe_types <- unique(mens_shoes_with_type$shoe_type)
mens_shoe_prices_by_type <- 
  vroom("processed/mens_shoe_prices_by_type.csv")
top_bottom_brands_data <-
  vroom("processed/top_bottom_brands.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Mens Shoes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prices for all shoes", tabName = "dataplot1", 
               icon = icon("tags")),
      menuItem("Prices by shoe type", tabName = "dataplot2",
               icon = icon("tags")),
      menuItem("Brands", tabName = "dataplot3",
               icon = icon("copyright")),
      menuItem("About", tabName = "about",
               icon = icon("address-card"))
      # menuItem("Modeling", tabName = "modeling", 
      #          icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dataplot1",
              fluidRow(
                h3("Number of pricing changes (for all mens shoes)"),
                box(plotOutput("data1", height = "250px")),
                box(
                  dateRangeInput(inputId = "daterange1", 
                                 label = "Date Range",
                                 start = date_updated_min,
                                 end = date_updated_max)
                )
              )
      ),
      tabItem(tabName = "dataplot2",
              fluidRow(
                h3("Number of pricing changes (for different shoe types)"),
                box(
                  dateRangeInput(inputId = "daterange2", 
                                 label = "Date Range",
                                 start = date_updated_min,
                                 end = date_updated_max)
                ),
                box(
                  checkboxGroupInput("shoe_type", 
                                     "Select shoe types to display", 
                                     choices = shoe_types,
                                     selected = shoe_types)
                ),
                box(plotOutput("data2", height = "300px")),
                box(plotOutput("data3", height = "300px"))
              )
      ),
      tabItem(tabName = "dataplot3",
              fluidRow(
                h3("High and low brands by prices"),
                box(plotOutput("data4", width = "800px", height = "500px"))
              ),
      ),
      tabItem(tabName = "about",
              fluidRow(
                h3("Data and source code locations")
              ),
              fluidRow(
                HTML('&nbsp;'), HTML('&nbsp;'),
                HTML('&nbsp;'), HTML('&nbsp;'),
                a("Data obtained from Datafiniti", 
                  href = "https://datafiniti.co/products/product-data/")
              ),
              fluidRow(
                HTML('&nbsp;'), HTML('&nbsp;'),
                HTML('&nbsp;'), HTML('&nbsp;'),
                a("Source code at Github ", 
                  href = "https://github.com/samirgadkari/shoes")
              )
      )
      # tabItem(tabName = "modeling",
      #         fluidRow(
      #           h3("Number of pricing changes (mens shoes by type)"),
      #           box(
      #             title = "Controls",
      #             selectInput("model", "Model type", 
      #                         c("Linear Regression", "SVM", "Random Forest")),
      #           ),
      #         )
      # )
    ) # tabItems
  ) # dashboardBody
) # dashboardPage


server <- function(input, output, session) {
  output$data1 <- renderPlot({
    
    date_range <- input$daterange1
    
    mens_shoes_summary %>%
      filter(dateupdated >= date_range[1] & 
               dateupdated <= date_range[2]) %>%
      ggplot(aes(dateupdated, rolling_mean, color = season)) +
      geom_path(aes(group = 1), size = 1) +
      labs(x = "Date price changed", 
           y = "Rolling mean of\nnumber of price changes")
  }, res = 96)

  output$data2 <- renderPlot({
    
    date_range <- input$daterange2
    shoe_types_selected <- input$shoe_type
    
    mens_shoes_with_type %>%
      filter(dateupdated >= date_range[1] & 
               dateupdated <= date_range[2] &
               (shoe_type %in% shoe_types_selected)) %>%
      ggplot(aes(dateupdated, n, color = shoe_type)) +
      geom_point() +
      labs(x = "Date price changed", 
           y = "Number of price changes")
  }, res = 96)
  
  output$data3 <- renderPlot({
    
    date_range <- input$daterange2
    shoe_types_selected <- input$shoe_type
    
    mens_shoe_prices_by_type %>%
      filter(dateupdated >= date_range[1] & 
               dateupdated <= date_range[2] &
               (shoe_type %in% shoe_types_selected)) %>%
      ggplot(aes(dateupdated, avg_price, color = shoe_type)) +
      geom_boxplot() +
      labs(x = "Average price", y = "Shoe type")
  }, res = 96)

  output$data4 <- renderPlot({
    
    top_bottom_brands_data %>%
      ggplot(aes(brand, price, color = brand)) +
      geom_boxplot() +
      labs(x = "Brand", y = "Price") +
      coord_flip()
  }, res = 96)
}

shinyApp(ui, server)