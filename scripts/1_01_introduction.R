# Foundations of Modern Finance
# Module 1: Introduction to Finance ----

# Load packages
library(tidyverse)

# Set options
options(digits = 15)

# Checks ----

## 1.4 Projects ----

projects <- tibble(
  project = c("A", "B", "A2"),
  investment = c(100, 100, 100),
  future_cf = c(110, 110, 110),
  rf = c(0.15, 0.05, 0.08)
)

projects <- projects %>%
  mutate(
    project_return = future_cf / investment - 1,
    pv_future_cf = future_cf / (1 + rf),
    npv = pv_future_cf - investment
  )

projects

# Recitation ----

# Problem Set ----
