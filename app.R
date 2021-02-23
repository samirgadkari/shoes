library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(vroom)

mens_shoes_summary <- vroom("processed/mens_shoes_summary.csv")
date_updated_min <- min(mens_shoes_summary$dateupdated)
date_updated_max <- max(mens_shoes_summary$dateupdated)

ui <- dashboardPage(
  dashboardHeader(title = "Mens Shoe Prices"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("DataPlot", tabName = "dataplot", 
               icon = icon("dashboard")),
      menuItem("Modeling", tabName = "modeling", 
               icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dataplot",
              fluidRow(
                h2("Number of pricing changes (for all mens shoes)"),
                box(plotOutput("data", height = 250)),
                box(
                  dateRangeInput(inputId = "daterange", 
                                 label = "Date Range",
                                 start = date_updated_min,
                                 end = date_updated_max)
                )
              ),
      ),
      tabItem(tabName = "modeling",
              fluidRow(
                h2("Number of pricing changes (mens shoes by type)"),
                box(
                  title = "Controls",
                  selectInput("model", "Model type", 
                              c("Linear Regression", "SVM", "Random Forest")),
                ),
              )
      )
    ) # tabItems
  ) # dashboardBody
) # dashboardPage


server <- function(input, output, session) {
  output$data <- renderPlot({
    
    date_range <- input$daterange
    
    mens_shoes_summary %>%
      filter(dateupdated >= date_range[1] & 
               dateupdated <= date_range[2]) %>%
      ggplot(aes(dateupdated, rolling_mean, color = season)) +
      geom_path(aes(group = 1), size = 1) +
      labs(x = "Date price changed", 
           y = "Rolling mean of\nnumber of price changes")
  })
}

shinyApp(ui, server)