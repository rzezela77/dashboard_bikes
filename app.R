#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# library(shiny)
# library(shinydashboard)
# library(plotly)
# library(highcharter)

source("global.R")

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Executive Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        
        # 1.0 tab content
        menuItem("Dashboards", tabName = "dashboard", icon = icon("dashboard"),
                 menuSubItem("Sales", tabName = "bikes_menu"),
                 # menuSubItem("Traffic", tabName = "traffic_menu"),
                 menuSubItem("Finance", tabName = "finance_menu")
        ),
        
        # 2.0 tab content
        menuItem("Business Analytics", icon = icon("business-time"),
                 menuSubItem("Customer Segmentation", tabName = "custSegmentation_menu"),
                 menuSubItem("Predictive Analytics", tabName = "predictiveAnalytics_menu")
        ),
        
        # # 3.0 tab content
        # menuItem("Reporting", icon = icon("bar-chart-o"),
        #          menuSubItem("Create Report", tabName = "createReport_menu"),
        #          menuSubItem("Data Report", tabName = "dataReport_menu")
        # ),
        
        menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
                 href = "https://analytica-moz.weebly.com/")
    )
)




# body -----------------------------------------------

body <- dashboardBody(
    tabItems(
        
        # # First tab content: Dashboards
        source(file = "ui/01_ui_dashboard_bikes.R", local = TRUE)$value,
        # source("ui/01_ui_dashboard_Traffic.R", local = TRUE)$value,
        # 
        # # Second tab content: Business Analytics
        source("ui/02_ui_customer_segmentation.R", local = TRUE)$value
        # 
        # # Third tab content: Reporting
        # # source("ui/02_ui_customer_segmentation.R", local = TRUE)$value
        # 
        
    )
    
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Bikes Sales', 
                    header, sidebar, body)



# 2.0 Server side ---------------------------------------------------------



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    source(file = "server/01_srv_dashboard_bikes.R", local = TRUE)
    
    source(file = "server/02_srv_customer_segmentation.R", local = TRUE)
    
    
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)