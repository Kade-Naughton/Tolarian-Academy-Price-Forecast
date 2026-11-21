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

