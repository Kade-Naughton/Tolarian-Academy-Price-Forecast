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

# Transforms the price data to log(price) (this makes makes changes proportional, card moving 40 -> 44 and 180 -> 198 are both 10% increases but without log it doesn't look like that, steadies variance/swing,
# curved growth becomes linear which is easier for models to fit, and makes sure all values are positive) and plots a seasonal plot of log(price)
card |>
  mutate(log_price = log(price)) |>
  gg_season(log_price) +
  labs(title = "Season Plot of log(Tolarian Academy Prices)",
       x = "Week", y = "log(Price)")

## FITTING MODELS
#
card_fit <- card |>
  model(arima = ARIMA(log(price)),
        ets   = ETS(log(price)))

card_fit |> select(arima) |> report()
card_fit |> select(ets) |> report()

## RESIDUAL CHECKS
# pval > 0.05 means residuals are white noise, model is good
card_fit |> select(arima) |> gg_tsresiduals()

card_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 10, dof = 1)

## TS-CV
# Fit values and runs TS-CV
library(future)                               # multicore processing
library(progressr)                            # progress bar
handlers("cli")

card_cv <- card |>
  stretch_tsibble(.init = 104, .step = 2) |> # first training set = 104 weeks: each later set adds 2 weeks
  filter(.id < max(.id))                     # drops the last window (no future data left to test against)

plan(multisession)                           # use multiple cores

with_progress({
  card_cv_fit <- card_cv |>
    model(naive = NAIVE(price),              # random walk
          ets   = ETS(log(price)),
          arima = ARIMA(log(price)))
})

plan(sequential)                             # back to single-core
saveRDS(card_cv_fit, "results/card_cv_fit.rds")

## FUTURE FORECAST & ACCURACY
#
card_cv_fc <- readRDS("results/card_cv_fit.rds") |>
  forecast(h = 4) |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |>                     # label each forecast's horizon: 1, 2, 3, 4 weeks ahead
  ungroup() |>
  as_fable(response = "price", distribution = price)

# Accuracy at each horizon, for each model
card_cv_acc <- card_cv_fc |>
  accuracy(card, by = c("h", ".model"),
           measures = list(RMSE = RMSE, MAE = MAE, MAPE = MAPE, MASE = MASE))

# 1 week ahead
card_cv_acc |>
  filter(h == 1) |>
  arrange(RMSE) |>
  knitr::kable(digits = 2)

# 4 weeks ahead
card_cv_acc |>
  filter(h == 4) |>
  arrange(RMSE) |>
  knitr::kable(digits = 2)

# How RMSE grows with the horizon for each model
card_cv_acc |>
  ggplot(aes(x = h, y = RMSE, colour = .model)) +
  geom_line() +
  geom_point() +
  labs(x = "Weeks ahead", y = "RMSE (USD)", title = "TS-CV forecast accuracy by horizon")

saveRDS(card_cv_acc, "results/card_cv_acc.rds")   # save the accuracy table

## DIEBOLD-MARIANO TEST
# Testing if the difference in accuracy between the two models is statistically significant
card_errs <- card_cv_fc |>
  as_tibble() |>
  filter(h == 4) |>                                    # 4-week-ahead forecasts only
  left_join(as_tibble(card) |> select(Week, actual = price),
            by = "Week") |>
  mutate(e = actual - .mean) |>                        # forecast error in USD
  select(.id, Week, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  drop_na()                                            # remove forecasts with no actual yet

#p<0.05 means the difference in accuracy is statistically significant, one model is better than the other
forecast::dm.test(card_errs$arima, card_errs$ets,   alternative = "two.sided", h = 4, power = 2)
forecast::dm.test(card_errs$arima, card_errs$naive, alternative = "two.sided", h = 4, power = 2)

## FINAL MODELS: ARIMA & RANDOM WALK
# Fit both models on the full dataset
card_final_fit <- card |>
  model(naive = NAIVE(price),                     # random walk
        arima = ARIMA(log(price)))                # ARIMA on log price

card_final_fit |> select(arima) |> report()       # check the ARIMA order chosen on the full data

# 4-week-ahead forecasts 
card_final_fc <- card_final_fit |>
  forecast(h = 4)

# Plot: recent history & forecasts with 80% and 95% intervals, one panel per model
card_final_fc |>
  autoplot(card |> filter(Week >= yearweek("2025 W01")), level = c(80, 95)) +
  facet_wrap(~ .model, ncol = 1) +
  labs(y = "USD", x = NULL,
       title = "Tolarian Academy: 4-week forecasts",
       subtitle = "Random walk vs ARIMA on log price")

# Table: point forecasts with 80% and 95% intervals
card_final_fc |>
  hilo(level = c(80, 95)) |>
  unpack_hilo(c("80%", "95%")) |>
  as_tibble() |>
  select(.model, Week, .mean, `80%_lower`, `80%_upper`, `95%_lower`, `95%_upper`) |>
  knitr::kable(digits = 2)

saveRDS(card_final_fc, "results/card_final_fc.rds")
