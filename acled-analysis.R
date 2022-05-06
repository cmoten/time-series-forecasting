library(readxl)
library(fpp3)

#'Tidy Data
acled_data <- read_excel("data/Africa_1997-2022_Apr22.xlsx")

test_data <- acled_data %>%
  select(EVENT_DATE, EVENT_TYPE, SUB_EVENT_TYPE, COUNTRY) %>%
  mutate(Quarter = yearquarter(EVENT_DATE)) %>%
  select(-EVENT_DATE) %>%
  group_by(Quarter, EVENT_TYPE, SUB_EVENT_TYPE, COUNTRY) %>%
  count() %>%
  as_tsibble(key = c(EVENT_TYPE, SUB_EVENT_TYPE, COUNTRY),
             index = Quarter)
  

algeria_battles <- test_data %>%
  filter(COUNTRY == "Algeria", EVENT_TYPE == "Battles", SUB_EVENT_TYPE == "Armed clash")

algeria_battles %>%
  autoplot(n) +
  labs(title = "Armed Clashes 1997-2022",
       subtitle = "Algeria",
       y = "Total")

algeria_battles %>%
  fill_gaps(n = 0) %>%
  ACF(n) %>%
  autoplot() + labs(title="Algerian Clashes")

algeria_battles %>%
  gg_lag(n, geom = "point") +
  labs(x = "lag(Total, k)")
