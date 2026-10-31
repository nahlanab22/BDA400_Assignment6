# ===== STEP 1: Data collection and setup =====
# Packages: shiny (app), ggplot2 (charts), quantmod (Yahoo Finance data)
library(shiny)
library(ggplot2)
library(quantmod)

stock_symbol <- "MSFT"

# Download MSFT daily prices from Yahoo Finance for Jan to Jul 2023
stock_data <- getSymbols(stock_symbol, src = "yahoo",
                         from = "2023-01-01", to = "2023-07-01",
                         auto.assign = FALSE)

# Safe SMA: returns NA instead of crashing when there is not enough data
safe_sma <- function(x, n) {
  if (length(x) >= n) SMA(x, n = n) else rep(NA_real_, length(x))
}
# ===== STEP 2: User interface (widgets) =====
# Date range, time frame, chart type and technical indicator checkboxes
ui <- fluidPage(
  titlePanel(paste(stock_symbol, "Portfolio Dashboard")),
  dateRangeInput("date_range", "Select Date Range:",
                 start = "2023-01-01", end = "2023-07-01"),
  selectInput("time_frame", "Select Time Frame:",
              choices = c("Daily", "Weekly", "Monthly")),
  selectInput("chart_type", "Chart Type:",
              choices = c("Line", "Area", "Candlestick")),
  checkboxGroupInput("technical_indicators", "Technical Indicators:",
                     choices = c("Moving Averages", "RSI", "MACD",  "Trading Signals")), 
  plotOutput("stock_chart"),
  conditionalPanel(
    "input.technical_indicators && input.technical_indicators.indexOf('RSI') > -1",
    plotOutput("rsi_chart", height = "200px")
  ),
  conditionalPanel(
    "input.technical_indicators && input.technical_indicators.indexOf('MACD') > -1",
    plotOutput("macd_chart", height = "200px")
  )
)

# ===== STEP 2: Server logic =====
server <- function(input, output) {
  output$stock_chart <- renderPlot({
    # ===== STEP 2: Visualizing stock data (server) =====
    # Filter data by the selected date range, then convert to weekly or monthly if chosen
    filtered_data <- subset(stock_data,
                            index(stock_data) >= input$date_range[1] &
                              index(stock_data) <= input$date_range[2])
    
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data, name = stock_symbol)
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data, name = stock_symbol)
    }
    
    df <- data.frame(Date = as.Date(index(filtered_data)),
                     Open = as.numeric(Op(filtered_data)),
                     High = as.numeric(Hi(filtered_data)),
                     Low = as.numeric(Lo(filtered_data)),
                     Close = as.numeric(Cl(filtered_data)))
    
    p <- ggplot(df, aes(x = Date))
    # Candlestick bar width depends on the time frame (daily bars are narrower than monthly)
    bar_w <- switch(input$time_frame, "Daily" = 0.3, "Weekly" = 2, "Monthly" = 10)
    
    # Draw the chart type chosen by the user: Line, Area or Candlestick
    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), color = "steelblue")
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "steelblue", alpha = 0.4)
    } else {
      p <- p +
        geom_linerange(aes(ymin = Low, ymax = High)) +
        geom_rect(aes(xmin = Date - bar_w, xmax = Date + bar_w,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                      fill = Close >= Open)) +
        scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red"),
                          guide = "none")
    }
    # ===== STEP 3: Overlay technical indicators on the price chart =====
    # SMA 20 and SMA 50 are drawn as extra lines when the checkbox is ticked
    # safe_sma() is used so the app does not crash with too few data points
    if ("Moving Averages" %in% input$technical_indicators) {
      df$SMA20 <- safe_sma(df$Close, n = 20)
      df$SMA50 <- safe_sma(df$Close, n = 50)
      p <- p +
        geom_line(data = df, aes(y = SMA20, color = "SMA 20"), na.rm = TRUE) +
        geom_line(data = df, aes(y = SMA50, color = "SMA 50"), na.rm = TRUE) +
        scale_color_manual(name = "", values = c("SMA 20" = "orange", "SMA 50" = "purple"))
    }
    # ===== STEP 4: Trading rules and annotations =====
    # Rule: short MA (20) crosses above long MA (50) = Buy
    #       short MA (20) crosses below long MA (50) = Sell
    #       otherwise = Hold
    # Buy and Sell labels are drawn on the price chart
    if ("Trading Signals" %in% input$technical_indicators) {
      short_ma <- safe_sma(df$Close, n = 20)
      long_ma  <- safe_sma(df$Close, n = 50)
      prev_short <- c(NA, head(short_ma, -1))
      prev_long  <- c(NA, head(long_ma, -1))
      
      # Buy: short MA crosses above long MA. Sell: crosses below. Otherwise Hold.
      df$Signal <- ifelse(short_ma > long_ma & prev_short <= prev_long, "Buy",
                          ifelse(short_ma < long_ma & prev_short >= prev_long, "Sell", "Hold"))
      df$Signal[is.na(df$Signal)] <- "Hold"
      
      # Annotate the bars with Buy / Sell labels
      buys  <- df[df$Signal == "Buy", ]
      sells <- df[df$Signal == "Sell", ]
      p <- p +
        geom_text(data = buys, aes(x = Date, y = Close, label = Signal),
                  vjust = -1, color = "darkgreen", fontface = "bold") +
        geom_text(data = sells, aes(x = Date, y = Close, label = Signal),
                  vjust = 1.5, color = "red", fontface = "bold")
    }
    p + labs(title = paste(stock_symbol, input$time_frame, input$chart_type),
             y = "Price")
  })
  output$rsi_chart <- renderPlot({
    d <- subset(stock_data,
                index(stock_data) >= input$date_range[1] &
                  index(stock_data) <= input$date_range[2])
    req(nrow(d) > 35)
    rsi_df <- data.frame(Date = as.Date(index(d)),
                         RSI = as.numeric(RSI(Cl(d), n = 14)))
    ggplot(rsi_df, aes(Date, RSI)) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "red") +
      geom_line(color = "darkblue", na.rm = TRUE) +
      ylim(0, 100) +
      labs(title = "RSI (14)", y = "RSI")
  })
  # ===== STEP 3 (continued): RSI and MACD indicator panels =====
  # Each panel only appears when its checkbox is ticked (see conditionalPanel in the UI)
  # req() stops the plot if there is too little data to calculate the indicator
  output$macd_chart <- renderPlot({
    d <- subset(stock_data,
                index(stock_data) >= input$date_range[1] &
                  index(stock_data) <= input$date_range[2])
    req(nrow(d) > 35)
    m <- MACD(as.numeric(Cl(d)), nFast = 12, nSlow = 26, nSig = 9)
    macd_df <- data.frame(Date = as.Date(index(d)),
                          MACD = m[, "macd"],
                          Signal = m[, "signal"])
    macd_df$Hist <- macd_df$MACD - macd_df$Signal
    ggplot(macd_df, aes(Date)) +
      geom_col(aes(y = Hist), fill = "grey60", na.rm = TRUE) +
      geom_line(aes(y = MACD, color = "MACD"), na.rm = TRUE) +
      geom_line(aes(y = Signal, color = "Signal"), na.rm = TRUE) +
      scale_color_manual(name = "", values = c("MACD" = "blue", "Signal" = "red")) +
      labs(title = "MACD (12, 26, 9)", y = "MACD")
  })
}
# ===== RUN THE APP =====
# Combines the UI and server into one Shiny application
shinyApp(ui, server)