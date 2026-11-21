library(tidyverse)
library(fpp3)
library(BVAR)

## DATA PULL
# Pulls data from CSV in data/. Defines column 1 and 2 as date and price_usd. Formats date to YYYY-MM-DD and prices_usd as "double" (numeric, not text)
card_raw <- read_csv(
  "data/tolarian_academy_usd.csv",
  na = ".",
  col_types = cols(
    date = col_date(format = "%Y-%m-%d"),
    price_usd = col_double()
  )
)

## ADJUSTING DATA
# Transforms the data to include a week column and a price column (combines days into weeks and finds and gives mean price for each week)
card <- card_raw |> 
  as_tsibble(index = date) |>
  index_by(Week = yearweek(date)) |>
  summarise(price = mean(price_usd))
# could add filter() to specify a date range, but it is not necessary for this data set

has_gaps(card) # checks for gaps in the data. Returns FALSE, no gaps exist

## PLOTS
# Plots Prices, not adjusted
card |>
  autoplot(price) +
  labs(title = "Sales price of Tolarian Academy #330 MTG Card",
       x = "Week", y = "USD Price")

# Transforms the price data to log(price) (this makes makes changes proportional, card moving 40 -> 44 and 180 -> 198 are both 10% increases but without log it doesnt look like that, steadies variance/swing,
# curved growth becomes linear which is easier for models to fit, and makes sure all values are positive) and plots a seasonal plot of log(price)
card |>
  mutate(log_price = log(price)) |>
  gg_season(log_price) +
  labs(title = "Season Plot of log(Tolarian Academy Prices)",
       x = "Week", y = "log(Price)")
