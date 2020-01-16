# 1.0 Loading libraries ---------------------------------------------------

library(shiny)
library(shinydashboard)
library(plotly)
# library(highcharter)

library(highcharter)
library(tidyverse)
library(lubridate)


library(treemap)
library(viridisLite)
library(RColorBrewer)


# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
#library(writexl)


# 1.1 Connecting to database 
library(rJava)
library(RJDBC)


# shinyWidgets
library(shinyWidgets)

library(prophet) # for forecasting


source(file = "functions/getFunctions.R")


 # topupHist_dat <- read.csv("data/refill_data.csv")
 # 
 cust_RFM_dat <- read.csv("data/cust_RFM_data.csv")

# 1.0 Load libraries ----




# 2.0 Importing Files ----

bikes_tbl <- read_excel("data/bikes.xlsx")

bikeshops_tbl <- read_excel("data/bikeshops.xlsx")

orderlines_tbl <- read_excel("data/orderlines.xlsx")




# 3.0 Examining Data ----

# bikes_tbl
#
# glimpse(bikes_tbl)
#
# bikeshops_tbl %>%
#     glimpse()
#
# orderlines_tbl %>%
#     glimpse()
#

# 4.0 Joining Data ----

# left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>%
    glimpse()

# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%

    # Separate description into category.1, category.2, and frame.material
    separate(description,
             into = c("category.1", "category.2", "frame.material"),
             sep = " - ",
             remove = TRUE) %>%

    # Separate location into city and state
    separate(location,
             into = c("city", "state"),
             sep = ",",
             remove = FALSE) %>%

    # price extended
    mutate(total.price = quantity * price) %>%

    # Reorganize
    select(-...1, -location, -ends_with(".id"), order.id) %>%
    ##bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% --other option

    # Reorder columns
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price,
           everything()) %>%

    # Renaming columns
    rename(order_date = order.date) %>%
    set_names(names(.) %>% str_replace_all("\\.", "_") )


# bike_orderlines_wrangled_tbl %>% glimpse()


# 6.0 Business Insights ----

# Calculate growth rate
growth_rate <- function(x)(x/lag(x)-1)*100

# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%

    # Selecting columns to focus on and adding a year column
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%

    # Grouping by year, and summarizing sales
    group_by(year) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%

    # $ Format Text
    mutate(sales_text = scales::dollar(sales),
           rate = growth_rate(sales) )


# sales_by_year_tbl



# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%

    # Selecting columns and add a year
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>%

    # Groupby and Summarize year and category 2
    group_by(year, category_2) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%

    # Format $ Text
    mutate(sales_text = scales::dollar(sales))


# sales_by_year_cat_2_tbl

# # Step 2 - Visualize
#
# sales_by_year_cat_2_tbl %>%
#
#     ggplot(aes(x = year, y = sales, fill = category_2)) +
#     geom_col() +
#     geom_smooth(method = "lm", se = FALSE) +
#     facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
#     theme_tq() +
#     scale_fill_tq() +
#     #theme_classic() +
#     #scale_fill_discrete()
#     scale_y_continuous(labels = scales::dollar) +
#     labs(
#         title = "Revenue by Year and Category 2",
#         subtitle = "Each product category has an upward trend",
#         x = "",
#         y = "Revenue",
#         fill = "Product Secondary Category"
#     )
#

# Creating table for sales by months -----

sales_by_month_category <-
    bike_orderlines_wrangled_tbl %>%

    # Selecting columns and add a year
    select(order_date, total_price, category_2) %>%
    group_by(Month = lubridate::floor_date(as.Date(order_date), unit = 'month'),
             category_2) %>%
    summarise(amount = sum(total_price)) %>%
    ungroup()

# min(sales_by_month_category$Month)
#
# max(sales_by_month_category$Month)

# Sales by weekday ------

sales_by_weekday <-
    bike_orderlines_wrangled_tbl %>%

    # Selecting columns and add a year
    select(order_date, total_price, category_2) %>%
    group_by(Year = year(as.Date(order_date)),
             weekday = wday(as.Date(order_date), label = TRUE)) %>%
    summarise(amount = sum(total_price)) %>%
    ungroup()


# Total sales by months to compare Month over Month -----

sales_by_MoM <-
    bike_orderlines_wrangled_tbl %>%

    # Selecting columns and add a year
    #select(order_date, total_price) %>%
    group_by(Year = as.factor(year(as.Date(order_date))),
             Month = as.integer(month(as.Date(order_date))),
             Month_Abb = as.factor(format(as.Date(order_date), "%b"))) %>%
    summarise(Amount = sum(total_price)) %>%
    # grouping by month and year
    group_by(Month) %>%
    arrange(Month, Year) %>%
    # calculate growth rate Month over Month
    mutate(MoM_rate = growth_rate(Amount)) %>%
    ungroup()

sales_by_MoM_last_actual <-
    sales_by_MoM %>%
    filter(Year %in% c('2014', '2015'))


plot_Sales_by_MoM <-
    plot_ly(data = sales_by_MoM_last_actual,
            x = ~reorder(Month_Abb, Month),
            y = ~Amount,
            color = ~Year,
            # text = ~paste0(scales::comma(Amount/1e3),'k'),
            text = ~scales::comma(Amount/1e3),
            textposition = 'outside',
            hovertemplate = paste('Year:', "<b>", sales_by_MoM_last_actual$Year,"</b>",
                                  '<br>',
                                  'Amount:', "<b>", scales::comma(sales_by_MoM_last_actual$Amount), "</b>",
                                  '<br>',
                                  'Growth rate:', "<b>", paste0(round(sales_by_MoM_last_actual$MoM_rate,1),"%"), "</b>")
    ) %>%
    add_bars() %>%
    layout(
        title = 'Month over Month',
        xaxis = list(
            title = 'month')
    )

# plot_Sales_by_MoM

# Performance by Product category -----------------------------------------



performance_by_product <-
    bike_orderlines_wrangled_tbl %>%
    filter(year(as.Date(order_date)) == '2015') %>%
    group_by(Year = year(as.Date(order_date)),
             category_2) %>%
    summarise(Amount = sum(total_price)) %>%
    mutate(proportion_rate = (Amount/sum(Amount))*100) %>%
    ungroup()

plot_performance_by_product <-
    plot_ly(data = performance_by_product,
            # change the order x and y
            y = ~reorder(category_2, performance_by_product$proportion_rate),
            x = ~proportion_rate,
            # type = 'bar',
            orientation = 'h',
            text = ~paste0(round(proportion_rate,1),'%'),
            textposition = 'auto',
            hovertemplate = paste('Category:', "<b>", performance_by_product$category_2,"</b>",
                                  '<br>',
                                  'Amount:', "<b>", scales::comma(performance_by_product$Amount), "</b>",
                                  '<br>',
                                  'Percent:', "<b>", paste0(round(performance_by_product$proportion_rate,1),"%"), "</b>")
    )%>%
    add_bars() %>%
    layout(
        title = 'Proportion of Product',
        yaxis = list(
            title = 'Category')
    )




# 7.0 Global trend ------------------------------------------------------------


sales_by_month <-
    bike_orderlines_wrangled_tbl %>%

    # Selecting columns and add a year
    select(order_date, total_price) %>%
    group_by(Month = lubridate::floor_date(as.Date(order_date), unit = 'month')) %>%
    summarise(Amount = sum(total_price)) %>%
    ungroup()

plot_sales_by_month <-
    plot_ly(data = sales_by_month,
            x = ~Month,
            y = ~Amount,
            type = 'scatter',
            # mode = 'markers'
            mode = 'lines+markers'
    )
# %>%
#      add_lines()
#


# daily summary ---
daily_sales <- 
    bike_orderlines_wrangled_tbl %>%
    
    # Selecting columns and add a year
    select(order_date, total_price) %>% 
    group_by(order_date = as.Date(order_date)) %>% 
    summarise(Amount = sum(total_price)) %>% 
    ungroup()


# Weekly sales ----

weekly_sales <-
    bike_orderlines_wrangled_tbl %>%
    
    # Selecting columns and add a year
    select(order_date, total_price) %>%
    group_by(Week = lubridate::ceiling_date(as.Date(order_date), unit = 'week')) %>%
    summarise(Amount = sum(total_price)) %>%
    ungroup()

# 7.1 Sales by Category ---------------------------------------------------

# daily sales by category
daily_sales_category <- 
    bike_orderlines_wrangled_tbl %>%
    
    # Selecting columns and add a year
    select(order_date, total_price, category_2) %>% 
    group_by(order_date = as.Date(order_date),
             category = category_2) %>% 
    summarise(Amount = sum(total_price)) %>% 
    ungroup()


# monthly sales by category
monthly_sales_category <- 
    bike_orderlines_wrangled_tbl %>%
    
    # Selecting columns and add a year
    select(order_date, total_price,category_2) %>% 
    group_by(Month = lubridate::floor_date(as.Date(order_date), unit = 'month'),
             category = category_2) %>% 
    summarise(Amount = sum(total_price)) %>% 
    ungroup()


# weekly sales by category
weekly_sales_category <- 
    bike_orderlines_wrangled_tbl %>%
    
    # Selecting columns and add a year
    select(order_date, total_price,category_2) %>% 
    group_by(Week = lubridate::ceiling_date(as.Date(order_date), unit = 'week'),
             category = category_2) %>% 
    summarise(Amount = sum(total_price)) %>% 
    ungroup()

# 7.2 Top product category ------------------------------------------------

top_product <- 
    daily_sales_category %>% 
    group_by(category) %>% 
    summarise(total_amount = sum(Amount)) %>% 
    arrange(desc(total_amount))


# 8.0 Foresting Demand ----------------------------------------------------


daily_forecast_tbl <- forecast_mode(daily_sales, periods = 365, freq = "day")

weekly_forecast_tbl <- forecast_mode(weekly_sales, periods = 52, freq = "week")

monthly_sales <- sales_by_month

monthly_forecast_tbl <- forecast_mode(monthly_sales, periods = 12, freq = "month")




