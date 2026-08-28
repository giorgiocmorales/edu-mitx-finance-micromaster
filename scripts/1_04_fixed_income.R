# Foundations of Modern Finance
# Module 4: Fixed Income ----

# Load packages
library(tidyverse)

# Set options
options(digits = 15)

# Checks ----

# Recitation ----

rm(list = ls())

## R4.1 ----

principal <- 100

price_a <- 97.50
price_b <- 96.00
price_c <- 98.00

coupon_b <- 0.03*principal
coupon_c <- 0.035*principal

rt_1 <- principal/(price_a)^(1/1) - 1

rt_2 <- ((principal + coupon_b)/(price_b - coupon_b/(1 + rt_1)^1))^(1/2) - 1

rt_3 <- ((principal + coupon_c)/(price_c - coupon_c/(1 + rt_1)^1 - coupon_c/(1 + rt_2)^2))^(1/3) - 1

c(rt_1, rt_2, rt_3)

# Alternative

cash_flows <- rbind(
  a = c(principal, 0, 0),
  b = c(coupon_b, coupon_b + principal, 0),
  c = c(coupon_c, coupon_c, coupon_c + principal)
)

colnames(cash_flows) <- c("P1", "P2", "P3")

prices <- c(
  a = price_a,
  b = price_b,
  c = price_c
)

discount_prices <- solve(cash_flows, prices)

names(discount_prices) <- colnames(cash_flows)

discount_prices

t <- seq_along(discount_prices)

spot_rates <- 1/discount_prices^(1/t) - 1

names(spot_rates) <- paste0("r", t)

spot_rates*100

rm(list = ls())

## R4.2 ----

### A

principal <- 100

price_a <- 96.90
price_b <- 99.00
price_c <- 88.55
price_d <- 99.50

coupon_b <- principal * 0.0275
coupon_d <- principal * 0.03

cash_flows <- rbind(
  a = c(principal, 0, 0),
  b = c(coupon_b, coupon_b + principal, 0),
  c = c(0, 0, principal)
)

colnames(cash_flows) <- c("t1", "t2", "t3")

prices <- c(
  a = price_a,
  b = price_b,
  c = price_c
)

discount_prices <- solve(cash_flows, prices)

names(discount_prices) <- c("P1", "P2", "P3")

discount_prices

t <- seq_along(discount_prices)

spot_rates <- 1/discount_prices^(1/t) - 1

names(spot_rates) <- paste0("r", t)

spot_rates*100

# Implied fair price for bond D ----

cash_flow_d <- c(coupon_d, coupon_d, coupon_d + principal)

price_d_implied <- sum(cash_flow_d/(1 + spot_rates)^t)

price_d_implied
price_d

price_d != price_d_implied

price_d - price_d_implied

### B

cash_flows_all <- rbind(
  cash_flows,
  d = cash_flow_d
)

prices_all <- c(prices, d = price_d)

A <- rbind(
  t0 = -prices_all,
  t(cash_flows_all)
)

A

b <- c(t0 = 100,
       t1 = 0,
       t2 = 0,
       t3 = 0)

positions <- solve(A, b)

positions

A %*% positions

rm(list = ls())

## R4.3 ----

rm(list = ls())

## R4.4 ----

rm(list = ls())

## R4.5 ----

rm(list = ls())

## R4.6 ----

# Problem Set ----

rm(list = ls())

## Q1 ----

principal <- 100

price_a <- 97.0139

coupon_b <- 0.045*principal

price_b <- 100.5226

cash_flows <- rbind(
  a = c(principal, 0),
  b = c(coupon_b, coupon_b + principal)
)

colnames(cash_flows) <- c("t1", "t2")

prices <- c(a = price_a,
            b = price_b)

discount_prices <- solve(cash_flows, prices)

names(discount_prices) <- c("P1", "P2")

t <- seq_along(discount_prices)

spot_rates <- 1/discount_prices^(1/t) - 1

names(spot_rates) <- paste0("r", t)

round(spot_rates*100, 6)

rm(list = ls())

## Q2 ----

face <- 100

price_a <- 96.7717
price_b <- 99.2705
price_c <- 105.2064

price_d <- 93.8343

coupon_a <- 0 * face
coupon_b <- 0.02396 * face
coupon_c <- 0.03412 * face

coupon_d <- 0

cash_flows <- rbind(
  a = c(face, 0, 0),
  b = c(coupon_b, coupon_b + face, 0),
  c = c(coupon_c, coupon_c, coupon_c + face)
)

colnames(cash_flows) <- c("t1", "t2", "t3")

prices <- c(a = price_a,
            b = price_b,
            c = price_c)

discount_prices <- solve(cash_flows, prices)

names(discount_prices) <- c("P1", "P2", "P3")

t <- seq_along(discount_prices)

spot_rates <- 1/discount_prices^(1/t) - 1

names(spot_rates) <- paste0("r", t)

spot_rates

cash_flow_d <- c(0, 0, face)

price_d_fair <- sum(cash_flow_d/(1 + spot_rates)^t)

price_d

price_d_fair

mispricing <- price_d - price_d_fair

### Arbitrage

cash_flows_all <- rbind(cash_flows, d = cash_flow_d)

prices_all <- c(prices, d = price_d)

A <- rbind(
  t0 = - prices_all,
  t(cash_flows_all)
)

b <- c(100, 0, 0, 0)

positions <- solve(A, b)

round(positions, 6)


# Checks

c(mispricing = mispricing, position_d = positions["d"])

A %*% positions

rm(list = ls())

## Q3 ----

rm(list = ls())

## Q4 ----

rm(list = ls())

## Q5 ----

rm(list = ls())

## Q6 ----

rm(list = ls())
