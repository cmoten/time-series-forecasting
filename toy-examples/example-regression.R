head(us_change)
us_change %>%
  select(-Consumption, -Income) %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")
class(us_change)

fit_conMR <- us_change %>%
  model(tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))

report(fit_conMR)

augment(fit_conMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL, title = "Percent change in US consumption expenditure") +
  scale_colour_manual(values = c(Data="black", Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))
