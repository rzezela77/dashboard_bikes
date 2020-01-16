frow_valueBox <- fluidRow(
    valueBoxOutput(outputId = "vb_Accumulated_ThisYear", width = 4),
    valueBoxOutput(outputId = "vb_Accumulated_LastYear", width = 4),
    valueBoxOutput(outputId = "vb_GrowthRate", width = 4)
    # ,valueBoxOutput(outputId = "vb_Total_LastYear", width = 3)
)


frow2 <- fluidRow( 
    box(
        title = "Annual Revenue"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,highchartOutput("hc_plotAnnualAmount", height = "300px")
        #,plotOutput("revenuebyYear", height = "300px")
        # ,plotlyOutput("revenuebyYear", height = "300px")
    )
    ,box(
        title = "Comparison: Month over Month"
        ,status = "primary"
        ,solidHeader = TRUE
        ,collapsible = TRUE
        ,highchartOutput("hc_plotComparison_MoM", height = "300px")
        # ,plotlyOutput("compareMonthByMonth", height = "300px")
        #,plotOutput("compareMonthByMonth", height = "300px")
        
    )
    
)


frow_trendView <- fluidRow(
    
    box(
        width = 12,
        highchartOutput(outputId = "hc_plotTrend",
                        width = "100%",
                        height = "400px")
    ),
    
    box(
        width = 12,
        highchartOutput(outputId = "hc_plotDetails_trend",
                        width = "100%",
                        height = "600px")
    )
    
    
)


frow_inputControls <- fluidRow(
    
     box(
        width = 12,
        title = "Time Series",
        status = "primary",

        radioGroupButtons(
            inputId = "rb_viewType",
            label = "Time View",
            choices = c("Daily", "Weekly", "Monthly"),
            selected = "Daily",
            justified = TRUE,
            status = "primary",
            checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"))
        ),

         strong("Forecast Mode"),
         # switchInput(inputId = "switch_forecast_mode_id"),
         # verbatimTextOutput("value"),

         prettySwitch(inputId = "switch1", 
                      label = "ON:",
                      status = "primary"),
         # verbatimTextOutput(outputId = "res1"),
    
         br(),
         submitButton("Apply Changes", icon("refresh"))
     ),
    
     
    
     box(
        width = 12,
        title = "Input Controls",
        status = "warning",

        dateRangeInput(inputId = 'dateRange',
                       label = 'Date range',
                       start = as.Date('2015-06-01'), end = as.Date('2015-12-01'))
    ),

    box(
        width = 12,
        title = "Top Product",
        status = "primary",
        highchartOutput(outputId = "hc_plotTopProducts",
                        width = "100%",
                        height = "400px")

    )

)

tabItem(
    tabName = "bikes_menu",
    frow_valueBox,
    frow2,
    fluidRow(
        column(9, frow_trendView),
        column(3, frow_inputControls)
    )
    # # Output: HTML table with requested number of observations ----
    # tableOutput("view")
)