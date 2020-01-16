# # 1.0 Loading libraries ---------------------------------------------------
# 
# library(highcharter)
# library(tidyverse)
# library(lubridate)
# 
# # 2.0 Connecting to database ----
# library(rJava)
# library(RJDBC)
# 
# source(file = "Functions/getFunctions.R")
# 
# # 2.2 ReactivePoll Topup History ------------------------------------------
# 
# topupHist_dat <- reactivePoll(24*60*60*1000, session = NULL,
#                               checkFunc = function(){
#                                   drv <- JDBC("oracle.jdbc.OracleDriver",
#                                               classPath="ojdbc14.jar"," ")
#                                   
#                                   con <- dbConnect(drv, "jdbc:oracle:thin:@10.100.55.195:1521:CDRX03", "report", "report")
#                                   
#                                   max_date_Hist <- dbGetQuery(con, "select max(AIR.DATESTARTOFCHARGING) max_date from cdr.airdb air")
#                                   
#                                   dbDisconnect(con)
#                                   return(max_date_Hist)
#                                   
#                               },
#                               
#                               valueFunc = function(){
#                                   
#                                   drv <- JDBC("oracle.jdbc.OracleDriver",
#                                               classPath="ojdbc14.jar"," ")
#                                   
#                                   con <- dbConnect(drv, "jdbc:oracle:thin:@10.100.55.195:1521:CDRX03", "report", "report")
#                                   
#                                   topupHist_records <- dbGetQuery(con, "select * from topup_hist")
#                                   
#                                   dbDisconnect(con)
#                                   
#                                   return(topupHist_records)
#                                   
#                               }
# )


voucher_refill_dat <- reactive({
    
    dataset <- topupHist_dat
    
    dataset <- dataset[, -1]
    
    dataset$DATE1 <- as.Date(dataset$DATE1)
    
    dataset$VOUCHER_GROUP <- as.factor(dataset$VOUCHER_GROUP)
    
    dataset
    
})



# 3.0 creating the valueBoxOutput content ----

getAccumulatedLastYear <- reactive({
    
    get_lastSales <- 
        sales_by_year_tbl %>%
        filter(year== '2014') %>%
        ungroup()
    
    return(get_lastSales$sales)
    
})


getAccumulatedThisYear <- reactive({
    
    get_actualSales <- 
        sales_by_year_tbl %>%
        filter(year== '2015') %>%
        ungroup()
    
    return(get_actualSales$sales)
})



output$vb_Accumulated_ThisYear <- renderValueBox({
    
    v_amount <- getAccumulatedThisYear()
    
    valueBox(
        formatC(scales::comma(v_amount), format="d", big.mark=',')
        # formatC(v_amount, format="d", big.mark=',')
        ,paste('Montante Acumulado: Presente Ano')
        ,icon = icon("dollar-sign",lib='font-awesome')
        ,color = "purple")  
})





output$vb_Accumulated_LastYear <- renderValueBox({ 
    
   v_amount <- getAccumulatedLastYear()
    
    valueBox(
        formatC(scales::comma(v_amount), format="d", big.mark=',')
        # formatC(v_amount, format="d", big.mark=',')
        ,'Montante Acumulado: Ano Passado'
        ,icon = icon("dollar-sign",lib='font-awesome')
        ,color = "green")  
})

output$vb_GrowthRate <- renderValueBox({
    
    getActualAmount <- getAccumulatedThisYear()
    
    getLastAmount <- getAccumulatedLastYear()
    
    get_growth_rate <- calc_growth_rate(getActualAmount, getLastAmount)
    
    valueBox(
        formatC(paste(round(get_growth_rate,2)," %"), format="d", big.mark=',')
        ,paste('Taxa Crescimento:')
        ,icon = icon("stats",lib='glyphicon')
        ,color = "yellow")   
})





output$hc_plotAnnualAmount <- renderHighchart({
    
    # Plotting Annual Revenue ---------------------------------------------
    
    v_categories <- sales_by_year_tbl$year
    
    # v_categories <- c("2014", "2015")
    
    plot_annualRevenue <- highchart() %>% 
        hc_title(text = "Annual Amount") %>% 
        hc_chart(type = "column") %>% 
        hc_xAxis(categories = v_categories) %>% 
        hc_add_series(name = "Total Amount",
                      data = sales_by_year_tbl$sales
                      ,dataLabels = list(align = "center", enabled = TRUE)
        ) 
    
    plot_annualRevenue
})



MonthOverMonth_tbl <- reactive({
    
    # Comparing Month over Month ------------------------------------------
    
    voucher_refill_dat() %>% 
        group_by(Year = year(DATE1),
                 Month = lubridate::floor_date(as.Date(DATE1), "month")) %>%
        ungroup() %>% 
        group_by(year = as.factor(Year),
                 month = as.integer(month(Month)),
                 month_abb = as.factor(format(Month, "%b"))) %>% 
        summarise(amount = sum(TOTAL_AMOUNT)) %>% 
        group_by(month) %>% 
        arrange(month, year) %>% 
        mutate(MoM_rate = growth_rate(amount)) %>% 
        ungroup()
    
})


MonthOverMonth_tbl_lastYear <- reactive({
    
    dataset <- sales_by_MoM %>% 
        filter(Year == c('2014'))
    
    dataset
})

MonthOverMonth_tbl_ThisYear <- reactive({
    
    dataset <- sales_by_MoM %>% 
        filter(Year == c('2015'))
    
    dataset
})


output$hc_plotComparison_MoM <- renderHighchart({
    
    dataset <- MonthOverMonth_tbl_lastYear()

    highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Monthly Amount") %>%
        hc_xAxis(categories = reorder(dataset$Month_Abb, dataset$Month)) %>%
        hc_add_series(data = round(dataset$Amount/1e3,0),
                      # name = format(Sys.Date()-365,"%Y"),
                      name = "2014"
                      ,dataLabels = list(align = "center", enabled = TRUE, color = "#f7a35c")
                      # ,color = "#1FA67A"
                      ) %>%
        hc_add_series(data = round(MonthOverMonth_tbl_ThisYear()$Amount/1e3, 0),
                      # name = format(Sys.Date(),"%Y")
                      name = "2015"
                      ,dataLabels = list(align = "center", enabled = TRUE)
        )

    # dataset <- MonthOverMonth_tbl()
    #
    # dataset %>%
    #     hchart(.,
    #         type = "column",
    #            # dataLabels = list(align = "center", enabled = TRUE,  format ='{point.y:,.0f}'),
    #            hcaes(
    #                 # x = reorder(dataset$month_abb, dataset$month),
    #                 x = dataset$month,
    #                  y = dataset$amount,
    #                  group = dataset$year)) %>%
    #     hc_title(text = "Comparison Month Over Month") %>%
    #     hc_xAxis(title = list(text = "Months"), minorTickInterval = "auto") %>%
    #     hc_yAxis(title = list(text = "Amount"))
    # # %>%
    # # hc_tooltip(headerFormat = "<b>{series.name}</b><br>",
    # #            pointFormat = "{point.x}, {point.y}")
    #

    # # plot_MonthOverMonth
    
     # dataset1 <- sales_by_MoM %>%
     #     filter(Year %in% c('2014', '2015'))
     # 
     # dataset1 %>%
     #     hchart(type = "column",
     #            hcaes(
     #                # x = reorder(dataset1$Month_Abb, dataset1$Month),
     #                x = dataset1$Month,
     #                  y= round(Amount/1e3,2),
     #                  group = Year)) %>%
     #     hc_title(text = "Details daily trend") %>%
     #     hc_subtitle(text ="by product type") %>%
     #     hc_exporting(
     #         enabled = TRUE
     #     )

})


output$compareMonthByMonth <- renderPlotly({
    
    dataset <- MonthOverMonth_tbl()
    
    
    plot_ly(data = dataset,
            x = ~reorder(dataset$month_abb, dataset$month) ,
            y = ~dataset$amount/1e6,
            text = scales::comma(dataset$amount/1e6),
            textposition = 'outside',
            hovertemplate = paste('Year:', "<b>", dataset$year,"</b>",
                                  '<br>',
                                  'Amount:', "<b>", scales::comma(dataset$amount), "</b>",
                                  '<br>',
                                  'Growth rate:', "<b>", paste0(round(dataset$MoM_rate,1),"%"), "</b>"),
            type = 'bar',
            color = ~dataset$year,
            colors = 'Set2',
            textfont = list(
                color = '#000000', 
                bold = TRUE,
                size = 16
            )
    ) %>% 
        layout(
            title = 'Month over Month',
            xaxis = list(
                title = 'month'
            ),
            yaxis = list(
                title = 'amount in million (MZN)'
            )
        ) 
})



output$view <- renderTable({
    
    MonthOverMonth_tbl() %>% 
        head(10)
})



# 4.0 Ploting Trend using Highchart ---------------------------------------


filtered_dat <- reactive({
    
    dataset <- voucher_refill_dat() %>%
        filter(between(DATE1, input$dateRange[1], input$dateRange[2]))
    
    
    dataset$product_type <- 'NA'
    dataset$product_type[which(dataset$voucher_group == "V10")] <- "Product1"
    dataset$product_type[which(dataset$voucher_group == "V20")] <- "Product2"
    dataset$product_type[which(dataset$voucher_group == "V30")] <- "Product3"
    dataset$product_type[which(dataset$voucher_group == "V50")] <- "Product5"
    dataset$product_type[which(dataset$voucher_group == "V80")] <- "Product8"
    dataset$product_type[which(dataset$voucher_group == "V100")] <- "Product10"
    dataset$product_type[which(dataset$voucher_group == "V150")] <- "Product15"
    dataset$product_type[which(dataset$voucher_group == "V200")] <- "Product20"
    dataset$product_type[which(dataset$voucher_group == "V300")] <- "Product30"
    dataset$product_type[which(dataset$voucher_group == "V600")] <- "Product60"
    dataset$product_type[which(dataset$voucher_group == "V2000")] <- "Product200"
    
    
    dataset
    
})







refill_daily_tbl <- reactive({
    
    dataset <- filtered_dat()
    
    dataset <- dataset %>%  
        group_by(DATE1) %>%
        summarise(TOTAL_AMOUNT = sum(TOTAL_AMOUNT)) %>%
        ungroup()
    
    dataset
    
    
})


output$hc_plotTrend <- renderHighchart({
    
    if (input$rb_viewType == "Daily"){
        
        if (isTRUE(input$switch1)){
            
            v_title <-  "Forecasting Daily Revenue"
            
            original_tbl <- daily_sales
                
            forecast_tbl <- daily_forecast_tbl
            
            hc_out <- plot_hc_forecasted(original_tbl, forecast_tbl)
            
        } else {
         
            # variables for Highchart
            v_title <-  "Daily Revenue"
            
            hc_out <- highchart(type = 'stock') %>% 
                hc_add_series(data = daily_sales, type = 'line',
                              hcaes(x = order_date, y = Amount),
                              name = 'daily amount')    
            
        }
        
        } else if (input$rb_viewType == "Weekly"){
            
            # Weekly View ----
            
            if (isTRUE(input$switch1)){
                
                v_title <-  "Forecasting Weekly Revenue"
                
                original_tbl <- weekly_sales
                
                forecast_tbl <- weekly_forecast_tbl
                
                hc_out <- plot_hc_forecasted(original_tbl, forecast_tbl)
                
            } else {
                
                # variables for Highchart
                v_title <-  "Weekly Revenue"
                
                hc_out <- highchart(type = 'stock') %>% 
                    hc_add_series(data = weekly_sales, type = 'line',
                                  hcaes(x = Week, y = Amount),
                                  name = 'weekly amount')
            }
            
        } else {
        
        # Monthly View ----
            
            if (isTRUE(input$switch1)){
                
                v_title <-  "Forecasting Monthly Revenue"
                
                original_tbl <- monthly_sales
                
                forecast_tbl <- monthly_forecast_tbl
                
                hc_out <- plot_hc_forecasted(original_tbl, forecast_tbl)
                
            } else {
                
                # variables for Highchart
                v_title <-  "Monthly Revenue"
                
                hc_out <- highchart(type = 'stock') %>% 
                    hc_add_series(data = sales_by_month, type = 'line',
                                  hcaes(x = Month, y = Amount),
                                  name = 'monthly amount')
            }
        }
     
    hc_out %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_title(text = v_title)
        
})



output$hc_plotTopProducts <- renderHighchart({
    
    dataset <- daily_sales_category %>%
        filter(between(order_date, input$dateRange[1], input$dateRange[2]))


    top_product <-
        dataset %>%
        group_by(category) %>%
        summarise(total_amount = sum(Amount)) %>%
        arrange(desc(total_amount))
    
    v_categories <- top_product$category
    
    highchart() %>% 
        hc_title(text = "Top Products") %>% 
        hc_subtitle(text ="Sales amount by product type") %>% 
        hc_chart(type = "bar") %>% 
        hc_xAxis(categories = v_categories) %>% 
        hc_add_series(name = "sales amount",
                      data = top_product$total_amount
                      # ,dataLabels = list(align = "center", enabled = TRUE)
        ) %>% 
        hc_exporting(
            enabled = TRUE
        )
    
    
})



# 4.1 Plotting details trend ----------------------------------------------

output$hc_plotDetails_trend <- renderHighchart({
    
    
    if (input$rb_viewType == "Daily"){
        
        v_title = "Daily sales"
        
        hc_out <- highchart(type = "stock") %>% 
            hc_add_series(data = daily_sales_category, type = "line",
                          hcaes(x = order_date, y = Amount, group = category)
                          )
        
        } else if (input$rb_viewType == "Weekly"){
            
            v_title = "Weekly sales"
            
            # weekly view
            hc_out <- highchart(type = "stock") %>% 
                hc_add_series(data = weekly_sales_category, type = "line",
                              hcaes(x = Week, y = Amount, group = category)
                              ) 
            
        } else {
        
            v_title = "Monthly sales"
            
        # monthly view
        
            hc_out <- highchart(type = "stock") %>% 
                hc_add_series(data = monthly_sales_category, type = "line",
                              hcaes(x = Month, y = Amount, group = category)
                              ) 
    
        }
    
    hc_out %>% 
        hc_title(text = v_title ) %>% 
        hc_subtitle(text ="by product type") %>% 
        hc_exporting(enabled = TRUE)
    
})