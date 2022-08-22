train <- aus_production %>%
  filter(Quarter < yearquarter("2006 Q1"))
test <- aus_production %>%
  filter(Quarter >= yearquarter("2006 Q1"))

fit <- train %>%
  model(ets = ARIMA(Beer))
sigma2 <- glance(fit) %>% pull(sigma2)
fit %>%
  refit(test) %>%
  fitted() %>%
  mutate(
    lo = .fitted - 1.96*sqrt(sigma2),
    hi = .fitted + 1.96*sqrt(sigma2)
  )

aus_production %>%
  autoplot(Beer)
