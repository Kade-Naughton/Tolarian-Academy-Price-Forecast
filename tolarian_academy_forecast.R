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