# Step 1: Setup and data collection
# Run this line once, then you can delete it or comment it out
#install.packages(c("shiny", "ggplot2", "quantmod"))

library(shiny)
library(ggplot2)
library(quantmod)

stock_symbol <- "MSFT"
start_date   <- "2023-01-01"
end_date     <- "2023-07-01"

stock_data <- getSymbols(stock_symbol, src = "yahoo",
                         from = start_date, to = end_date,
                         auto.assign = FALSE)

head(stock_data)
df <- data.frame(Date = index(stock_data), coredata(stock_data))
colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
str(df)

ggplot(df, aes(Date, Close)) +
  geom_line(color = "steelblue") +
  labs(title = paste(stock_symbol, "Closing Price"))