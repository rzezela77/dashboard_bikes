# Functions



# 1.0 Calculate Growth Rate -----------------------------------------------

calc_growth_rate <- function(x1, x0){
    ((x1 - x0)/x0)*100
}


# Calculate Growth rate Time Series ---------------------------------------

# Calculate growth rate
growth_rate <- function(x)(x/lag(x)-1)*100


more_colors <- c(
    # highcharts colors
    "#7cb5ec",
    "#434348",
    "#90ed7d",
    "#f7a35c",
    "#8085e9",
    "#f15c80",
    "#e4d354",
    "#2b908f",
    "#f45b5b",
    "#91e8e1",
    
    # from color brewer
    "#ffffd9",
    "#7fcdbb",
    "#41b6c4",
    "#1d91c0",
    "#edf8b1",
    "#225ea8",
    "#253494",
    "#c7e9b4",
    "#081d58"
)

# 2.0 apply function for forecasting -------------------------------------------

forecast_mode <- function(result_tbl, periods, freq){
    
    colnames(result_tbl) <- c("ds", "y")
    
    result_tbl$y <- log(result_tbl$y)
    
    
    m <- prophet(result_tbl, seasonality.mode = "multiplicative")
    
    future <- make_future_dataframe(m, periods = periods, freq = freq)
    
    forecast <- predict(m, future)
    
    forecast_tbl <- 
        forecast %>%
        select(ds, yhat, yhat_lower, yhat_upper)
    
    return (forecast_tbl)
}


# 3.0 Plot Forecast Demand ------------------------------------------------


plot_hc_forecasted <- function(original_tbl, forecast_tbl){
    
    colnames(original_tbl) <- c("x_var", "y_var")
    
    highchart(type = "stock") %>% 
        hc_add_series(data = original_tbl, 
                      type = "line", 
                      hcaes(x = x_var, y = y_var), 
                      name = "Actual") %>% 
        hc_add_series(data = forecast_tbl, 
                      type = "spline", 
                      hcaes(x = as.Date(ds), y = round(exp(yhat),0)), 
                      name = "Predicted",
                      id = "fit", # this is for link the arearange series to this one and have one legend
                      lineWidth = 1) %>% 
        hc_add_series(
            data = forecast_tbl,
            type = "arearange",
            hcaes(x = as.Date(ds), low = round(exp(yhat_lower),0), high = round(exp(yhat_upper),0)),
            name = "Range",
            linkedTo = "fit", # here we link the legends in one.
            color = hex_to_rgba("gray", 0.2),  # put a semi transparent color
            zIndex = -3 # this is for put the series in a back so the points are showed first
        )
    
}



