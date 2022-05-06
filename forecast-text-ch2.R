#' Basic Time Series
#'Frequency argument:
#'Annual-1; Quarterly-4; Monthly-12; Weekly-52
library(fpp3)

#Basic Data-----
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

#tsibble objects advanced-----
data(PBS)
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6) -> a10

#Time Plots-----
data("ansett")
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")


autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")


#Scatterplot Matrices-----
data("tourism")
visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)

#Lag Plots-----
data("aus_production")
recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)

recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

recent_production %>%
  gg_lag(Gas, geom = "point") +
  labs(x = "lab(Gas, k)")

#Auto-correlation-----
#'This plot shows a seasonal peak every four quarters starting at Q4
#'This plot also shows a seasonal trough ever four quarters starting at Q2
recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title="Australian beer production")

#'This plot shows seasonality and an increasing trend
a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot() +
  labs(title="Australian antidiabetic drug sales")
