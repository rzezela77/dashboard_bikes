frow_boxOutput <- fluidRow(
    valueBoxOutput("boxOut_Champions", width = 3)
    ,valueBoxOutput("boxOut_NewCustomer90", width = 3)
    ,valueBoxOutput("boxOut_ActiveCustomers", width = 3)
    ,valueBoxOutput("boxOut_InactiveCustomers", width = 3)
)


frow_overview <- fluidRow(
    
    box(width = 12,
        title = "Customer Segmentation using RFM Analysis",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        # plotlyOutput("plot_CustomerFreqTable", height = "300px")
        helpText("RFM (Recency, Frequency & Monetary) analysis is a behavior based technique
used to segment customers by examining their transaction history such as:
        a) how recently a customer has purchased?
        b) how often do they purchase?
        c) how much the customer spends?
        It is based on the marketing axiom that 80% of your business comes from 20%
of your customers. RFM helps to identify customers who are more likely to
respond to promotions by segmenting them into various categories.")
    )
    
)



frow_Treemap <- fluidRow(
    
    box(
        width = 12,
        highchartOutput(outputId = "hc_treemapOutput",
                        width = "100%",
                        height = "400px")
    ),
    
    box(
        width = 12,
        highchartOutput(outputId = "hc_plotSegment_size",
                        width = "100%",
                        height = "400px")
    )
    
)

frow_tabItem22 <- fluidRow(
    
    # column(width = 12,
    
    box(
        width = 12,
        title = "Customer Segmentation",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("plot_CustomerFreqTable", height = "300px")
    ),
    
    box(
        width = 12,
        title = "Segments Size",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("plot_CustSegmentSize", height = "300px")
    )
    
    # )
    
    
)




frow_datatable <- fluidRow(
    # Create a new row for the table.
    DT::dataTableOutput("table")
)




tabItem(
    tabName = "custSegmentation_menu",
    frow_boxOutput,
    frow_overview,
    fluidRow(
        column(
            9,
            frow_Treemap
            # frow_tabItem22,
            # frow_datatable
        ),
        column(
            3,
            fluidRow(
                valueBoxOutput("info_totalCustomer", width = 12),
                # box(
                #     width = 12,
                #     title = "Groups",
                #     status = "warning",
                #     div(
                #         class = "text-center",
                #         checkboxGroupInput(
                #             "groups",
                #             NULL,
                #             choices = list("high value" = 1, "low value" = 2),
                #             selected = 1,
                #             inline = TRUE
                #         )
                #     ),
                #     # Select whether to overlay smooth trend line
                #     checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                #     
                #     # Display only if the smoother is checked
                #     conditionalPanel(condition = "input.smoother == true",
                #                      sliderInput(inputId = "f", label = "Smoother span:",
                #                                  min = 0.01, max = 1, value = 0.67, step = 0.01,
                #                                  animate = animationOptions(interval = 100)),
                #                      HTML("Higher values give more smoothness.")
                #     )
                # ),
                
                
                box(
                    width = 12,
                    title = "Input controls",
                    status = "warning",
                    sliderInput(inputId = "slider_monetary_id",
                                label = strong("Average Monetary"),
                                min = 10,
                                max = 2000,
                                value = c(10, 2000),
                                step = 10),
                    sliderInput(inputId = "slider_recency_id",
                                label = strong("Recency: no. of days"),
                                min = 0,
                                max = 2*365,
                                value = c(0, 2*365),
                                step = 5),
                    submitButton("Update View", icon("refresh"))
                )
            )
        )
    )
    
    
    # Output: Verbatim text for data summary ----
    # ,verbatimTextOutput("summary_customerSegmentation"),
    
    # Output: HTML table with requested number of observations ----
    # tableOutput("view")
)