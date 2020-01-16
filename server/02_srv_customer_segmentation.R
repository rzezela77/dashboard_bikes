output$summary_customerSegmentation <- renderPrint({
    
    
    
    dataset <- customer_dat()
    
    summary(dataset[,2:5])
})


output$summary <- renderPrint({
    
    customer_dat() %>% 
        glimpse()
    
})

output$view <- renderTable({
    
    dataset <- customerSegmentation()
    
    table(dataset$segment)
    
    # get_freqTable()
    
    # customer_dat() %>% 
    #     head(10)
})




# 4.0 Customer Segmentation ---------------------------------------------------

# customerSegmentation <- reactive({
# 
#     dataset <- customer_dat()
# 
#     dataset$segment <- 'NA'
#     dataset$segment[which(dataset$RECENCY > 365)] <- 'inactive'
#     dataset$segment[which(dataset$RECENCY <= 365 & dataset$RECENCY > 180)] <- 'cold'
#     dataset$segment[which(dataset$RECENCY <= 180 & dataset$RECENCY > 90)] <- 'warm'
#     dataset$segment[which(dataset$RECENCY <= 90)] <- 'active'
#     # condition based on segment, warm
#     dataset$segment[which(dataset$segment == 'warm' & dataset$FIRST_PURCHASE <= 120)] <- 'new warm'
#     dataset$segment[which(dataset$segment == 'warm' & dataset$AMOUNT < 100)] <- 'warm low value'
#     dataset$segment[which(dataset$segment == 'warm' & dataset$AMOUNT >= 100)] <- 'warm high value'
#     # active
#     dataset$segment[which(dataset$segment == 'active' & dataset$FIRST_PURCHASE <= 30)] <- 'new active'
#     dataset$segment[which(dataset$segment == 'active' & dataset$AMOUNT < 100)] <- 'active low value'
#     dataset$segment[which(dataset$segment == 'active' & dataset$AMOUNT >= 100)] <- 'active high value'
# 
# 
#     dataset$segment <- factor(x = dataset$segment,
#                               levels = c('inactive', 'cold', 'warm high value', 'warm low value', 'new warm', 'active high value', 'active low value', 'new active'))
# 
# 
#     dataset
# 
# })


# 5.0 creating segments ---------------------------------------------------

customer_dat <- reactive({
    
    dataset <-  cust_RFM_dat
    
    dataset <-  dataset[, -1]

    # dataset$MSISDN <- as.factor(dataset$MSISDN)
    
    dataset$CUSTOMER_ID <- as.factor(dataset$CUSTOMER_ID)
    
    dataset
    
})


# 5.1 RFM Score --------------------------------------------------

rfm_score_data <- reactive({
    
    rfm_data <- customer_dat()
    
    rfm_data <-
        rfm_data %>%
        mutate(R = ntile(desc(RECENCY), 5),
               F = ntile(FREQUENCY, 5),
               M = ntile(MONETARY, 5))
    
    
    
    
    # applying the formula
    rfm_data$RFM <- rfm_data$R * 100 + rfm_data$F * 10 + rfm_data$M
    
    return(rfm_data)
    
})


# # 5.1 segment the customer based on RFM scores ----

customerSegmentation <- reactive({
    
    rfm_data <- rfm_score_data()
    
    
    # High value and low value
    rfm_data$segment <- 'NA'
    rfm_data$group <- 'NA'
    
    rfm_data$group[which(rfm_data$M >= 4)] <- 'high value'
    rfm_data$group[which(rfm_data$M < 4)] <- 'low value'
    
    
    # high value - segment
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F == 5)] <- 'Champions'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F == 4)] <- 'Loyal Customers'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F == 5)] <- 'Loyal Customers'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F == 4)] <- 'Loyal Customers'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F == 3)] <- 'Potencial Loyalists'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F == 3)] <- 'Potencial Loyalists'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F < 3)] <- 'Need Attention'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F < 3)] <- 'Need Attention'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 3 & rfm_data$F == 5)] <- 'About to Sleep'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 3 & rfm_data$F == 4)] <- 'About to Sleep'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 3 & rfm_data$F <= 3)] <- 'About to Sleep'
    # rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 3 & rfm_data$F < 3)] <- 'About to Sleep'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 2 & rfm_data$F == 5)] <- 'At Risk'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 2 & rfm_data$F == 4)] <- 'At Risk'
    rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 2 & rfm_data$F <= 3)] <- 'Hibernating'
    # rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 2 & rfm_data$F < 3)] <- 'hibernating'
    
    
    
    # low value - segment
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F == 5)] <- 'Champions'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F == 4)] <- 'Loyal Customers'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F == 5)] <- 'Loyal Customers'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F == 4)] <- 'Loyal Customers'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F == 3)] <- 'Potencial Loyalists'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F == 3)] <- 'Potencial Loyalists'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F < 3)] <- 'Need Attention'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F < 3)] <- 'Need Attention'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 3 & rfm_data$F == 5)] <- 'About to Sleep'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 3 & rfm_data$F == 4)] <- 'About to Sleep'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 3 & rfm_data$F <= 3)] <- 'About to Sleep'
    # rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 3 & rfm_data$F < 3)] <- 'About to Sleep'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 2 & rfm_data$F == 5)] <- 'At Risk'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 2 & rfm_data$F == 4)] <- 'At Risk'
    rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 2 & rfm_data$F <= 3)] <- 'Hibernating'
    
    # without group
    rfm_data$segment[which(rfm_data$R == 1)] <- 'Lost'
    rfm_data$segment[which(rfm_data$FIRST_PURCHASE <= 90)] <- 'New Customers'
    
    
    rfm_data$segment <- factor(x = rfm_data$segment,
                               levels = c('Lost', 'Hibernating', 'At Risk', 'About to Sleep', 'Need Attention', 'New Customers', 'Potencial Loyalists', 'Loyal Customers', 'Champions'))
    
    rfm_data$segment <- fct_explicit_na(rfm_data$segment)
    
    rfm_data
    
})



get_freqTable <- reactive({
    
    # freq_tbl <- count(customerSegmentation(), c('segment'))
    
    freq_tbl <- 
        # customerSegmentation() %>% 
        base_rfm_data() %>% 
        count(segment) %>% 
        rename(Segment = segment, Count = n)
    
    return(freq_tbl)
})


get_freqTable_Group <- reactive({
    
    # freq_tbl <- count(customerSegmentation(), c('segment'))
    
    freq_tbl <- 
        # customerSegmentation() %>% 
        base_rfm_data() %>% 
        count(segment, group) %>% 
        rename(Segment = segment, Count = n)
    
    return(freq_tbl)
})



get_NewCustomers <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'New Customers')
    
    return(get_value$Count)
})



# 6.0 Getting valueBox ----------------------------------------------------

get_NewCustomers <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'New Customers')
    
    return(get_value$Count)
})


get_Champions <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'Champions')
    
    return(get_value$Count)
})


get_LoyalCustomers <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'Loyal Customers')
    
    return(get_value$Count)
})


get_PotencialLoyalists <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'Potencial Loyalists')
    
    return(get_value$Count)
})


get_NeedAttention <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'Need Attention')
    
    return(get_value$Count)
})


get_About_toSleep <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'About to Sleep')
    
    return(get_value$Count)
})


get_AtRisk <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'At Risk')
    
    return(get_value$Count)
})

get_Hibernating <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'Hibernating')
    
    return(get_value$Count)
})


get_Lost <- reactive({
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'Lost')
    
    return(get_value$Count)
})


# 1.0 creating the valueBoxOutput content --------------

output$boxOut_Champions <- renderValueBox({ 
    
    
    dataset <- get_freqTable()
    
    get_value <- 
        dataset %>% 
        filter(Segment == 'Champions')
    
    get_Champions_value <- get_value$Count
    
    valueBox(value = prettyNum(get_Champions_value, big.mark = ","),
             subtitle = "Champions Customers",
             icon = icon("fas fa-trophy",lib='font-awesome')
             ,color = "purple")
    
    
    
    # valueBox(
    #     formatC(scales::comma(get_Champions_value), format="d", big.mark=',')
    #     ,'Champions'
    #     ,icon = icon("user-o",lib='font-awesome')
    #     ,color = "green")  
})


output$boxOut_NewCustomer90 <- renderValueBox({
    
    
    result <- base_rfm_data() %>% 
        filter(FIRST_PURCHASE <= 90) %>% 
        tally %>% 
        pull %>% 
        as.integer()
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "New Users: 0 to 03 months",
             icon = icon("user-o",lib='font-awesome')
             ,color = "green")
    
    
})



output$boxOut_ActiveCustomers <- renderValueBox({
    
    
    # Active Customers: last 365 days
    result <- base_rfm_data() %>% 
        filter(RECENCY <= 90) %>% 
        tally %>% 
        pull %>% 
        as.integer()
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "Active Users: last 90 days",
             icon = icon("user-o",lib='font-awesome')
             ,color = "green")
    
    
})


output$boxOut_InactiveCustomers <- renderValueBox({
    
    
    # Inactive Customers: more than 365 days
    result <- base_rfm_data() %>% 
        filter(RECENCY > 90 & RECENCY <= 365 ) %>% 
        tally %>% 
        pull %>% 
        as.integer()
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "Dormant: 04 to 12 months",
             icon = icon("user-o",lib='font-awesome')
             ,color = "yellow")
    
    
})





# Input Controls ----------------------------------------------------------

output$info_totalCustomer <- renderValueBox({
    
    # result <- customer_dat() %>% 
    #     # filter(FIRST_PURCHASE <= 90) %>% 
    #     tally %>% 
    #     pull %>% 
    #     as.integer()
    
    
    result <- base_rfm_data() %>%
        # filter(FIRST_PURCHASE <= 90) %>%
        tally %>%
        pull %>%
        as.integer()
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "Total Users",
             icon = icon("glyphicon-user", lib = "glyphicon"),
             color = "aqua")
    
}) 


# 3.0 Treemap Output ------------------------------------------------------

output$hc_treemapOutput <- renderHighchart({
    
    
    # in use - 20191108
    hctreemap2(data = get_freqTable(),
               group_vars = "Segment",
               size_var = "Count",
               color_var = "Count"
    ) 
    # %>% 
    #     hc_colorAxis(minColor = brewer.pal(7, "Greens")[1],
    #                  maxColor = brewer.pal(7, "Greens")[7])
    
})




output$hc_plotSegment_size <- renderHighchart({
    
    dataset <- get_freqTable_Group()
    
    lowValue_data <- subset(dataset, select = "Count", group == "low value") %>% 
        pull %>% 
        as.integer()
    
    highValue_data <- subset(dataset, select = "Count", group == "high value") %>% 
        pull %>% 
        as.integer()
    
    
    highchart() %>% 
        hc_title(text = "Segment Size") %>% 
        hc_subtitle(text ="customer segmentation") %>% 
        hc_chart(type = "column") %>% 
        hc_xAxis(categories = unique(dataset$Segment)) %>% 
        hc_add_series(data = lowValue_data,
                      name = "low value"
                      # dataLabels = list(align = "center", enabled = TRUE)
        ) %>%
        hc_add_series(data = highValue_data,
                      name = "high value"
                      # type = "line",
                      # color = "#1FA67A",
                      # dataLabels = list(align = "center", enabled = TRUE)
        ) %>% 
        hc_plotOptions(
            column = list(
                stacking = "normal"
            )
        ) %>%
        hc_exporting(
            enabled = TRUE
            # buttons = tychobratools::hc_btn_options()
        ) %>% 
        hc_yAxis(
            title = list(text = "No of subscribers"),
            stackLabels = list(
                enabled = TRUE,
                style = list(
                    fontWeight = "bold",
                    color = "#f7a35c",
                    textOutline = NULL
                ),
                format = "{total:,.0f}"
            )
        )
    
})





# output$value_NewCustomer <- renderValueBox({
#     
#     valueBox(
#         formatC(scales::comma(get_NewCustomers()), format="d", big.mark=',')
#         ,'New customer'
#         ,icon = icon("user-o",lib='font-awesome')
#         ,color = "green")
# })







output$plot_CustomerFreqTable <- renderPlotly({
    
    dataset <- get_freqTable()
    
    plot_ly(data = dataset,
            labels = dataset$Segment,
            values = dataset$Count,
            type = 'pie')
})


output$plot_CustSegmentSize <- renderPlotly({
    
    dataset <- get_freqTable_Group()
    
    plot_ly(data = dataset,
            x = dataset$Segment,
            y = dataset$Count,
            color = dataset$group,
            colors = "Set2",
            # hoverinfo = 'text',
            # text = ~paste(
            #               '</br> Segment: ', '<b>', Segment, '</b>',
            #               '</br> Group: ', '<b>', group, '</b>',
            #               '</br> Count: ', '<b>', scales::comma(dataset$Count), '</b>'
            #              ),
            textposition = 'outside',
            text = ~scales::comma(dataset$Count/1e3),
            type = 'bar') %>% 
        layout(yaxis = list(title = 'Count'), 
               barmode = 'stack'
               # , barmode = 'group'
        )
})


getCustAgg_dat <- reactive({
    
    rfm_data <- customerSegmentation()
    
    cust_aggr_dat <-  aggregate(x = rfm_data[, 2:5], by = list(rfm_data$segment, rfm_data$group), mean)
    
    # cust_aggr_dat
    format(cust_aggr_dat, digits = 2)
})

output$table <- DT::renderDataTable(
    
    DT::datatable({
        data <- getCustAgg_dat()
        
        data
    }))


# # Frequency Table ---------------------------------------------------------
# 
# library(plyr)
# 
# customer_dat %>% 
#     glimpse()
# 
# customer_dat$segment <- factor(x = customer_dat$segment, 
#                                levels = c('inactive', 'cold', 'warm high value', 'warm low value', 'new warm', 'active high value', 'active low value', 'new active'))
# 
# 
# freq_tbl <- count(customer_dat, c('segment'))
# 
# freq_tbl
# 
# plot_CustFreqTable <- 
#     plot_ly(data = freq_tbl,
#             x = ~ reorder(segment, freq),
#             y = ~freq) %>% 
#     add_bars() %>% 
#     layout(
#         title = 'Customer Segmentation',
#         xaxis = list(title = 'segment'),
#         yaxis = list(title = 'count')
#     )
# 
# plot_CustFreqTable
# 
# cust_aggr_dat <-  aggregate(x = customer_dat[, 2:5], by = list(customer_dat$segment), mean)
# 
# cust_aggr_dat
# 


# 7.0 Customer Base -------------------------------------------------------

base_rfm_data <- reactive({
    
    result <- customerSegmentation() %>% 
        filter(between(MONETARY, input$slider_monetary_id[1],input$slider_monetary_id[2]),
               between(RECENCY, input$slider_recency_id[1], input$slider_recency_id[2]))
    
    result
})
