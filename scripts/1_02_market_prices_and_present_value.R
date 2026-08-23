# Foundations of Modern Finance
# Module 2: Market Prices and Present Value ----

# Load packages
library(tidyverse)

# Set options
options(digits = 15)

# Checks ----

# Recitation ----

## R2.1 ----

prob <- c(0.3, 0.4, 0.3)

bond_payoff <- c(100, 100, 100)
stock_payoff <- c(120, 75, 130)

bond_price <- 96
stock_price <- 80

# Expected payoffs
E_bond <- sum(prob * bond_payoff)
E_stock <- sum(prob * stock_payoff)

# Expected returns
r_bond <- (E_bond - bond_price) / bond_price
r_stock <- (E_stock - stock_price) / stock_price

E_bond
r_bond

E_stock
r_stock

## R2.2 ----

phi <- c(0.35, 0.55)

bond_payoff <- c(100, 100)
stock_payoff <- c(150, 40)

bond_price <- sum(phi * bond_payoff)
stock_price <- sum(phi * stock_payoff)

bond_price
stock_price

## R2.3 ----

X <- matrix(
  c(
    100, 100, 100,  # Bond
    250,   0, 500,  # Stock 1
    0,   0, 500   # Stock 2
  ),
  nrow = 3,
  byrow = TRUE
)

prices <- c(95, 150, 100)

phi <- solve(X, prices)

phi


## R2.4 ----

phi <- c(0.2, 0.55, 0.2)
p <- c(1/3, 1/3, 1/3)

asset_x_payoff <- c(100, 80, 100)
asset_y_payoff <- c(100, 100, 80)

price_x <- sum(phi*asset_x_payoff)
price_y <- sum(phi*asset_y_payoff)

E_x <- sum(p*asset_x_payoff)
E_y <- sum(p*asset_y_payoff)

r_x <- E_x/price_x - 1
r_y <- E_y/price_y - 1

price_x
E_x
r_x

price_y
E_y
r_y

## R2.5 ----

### A ----
pv_a <- 10000
cf_b1 <- 11500
t <- 1
r <- 0.08

pv_b <- cf_b1/(1+r)^t

pv_a
pv_b # Pick B

### B ----

pv_a <- 10000
cf_b1 <- 18000
t <- 10
r <- 0.08

pv_b <- cf_b1/(1+r)^t

pv_a # Pick A
pv_b

## R2.6 ----

t <- 20

### A: Annual return from tripling in value ----

growth_multiple <- 3

r_apartment <- growth_multiple^(1/t) - 1

r_apartment
r_apartment * 100


### B: Wealth growth at 10.2% annual return ----

r_market <- 0.102

wealth_multiple <- (1 + r_market)^t

wealth_multiple


### C: Return earned on previous returns ----
principal_multiple <- 1
simple_return_multiple <- r_market * t

return_on_return <- wealth_multiple - principal_multiple - simple_return_multiple

principal_multiple
simple_return_multiple
return_on_return

# Check
principal_multiple + simple_return_multiple + return_on_return

## R2.7 ----

cf <- 650000

pi <- 0.025
g <- 0.03
r_real <- 0.06

### A ----

cf1_real <- cf*(1 + g)
cf1_real

### B ----

pv_cf1_real <- cf1_real/(1 + r_real)
pv_cf1_real

### C ----
cf1_nominal <- cf*(1 + g)*(1 + pi)
cf1_nominal 

### D ----
r_nominal <- (1 + r_real)*(1 + pi) - 1
r_nominal

pv_cf1_nominal <- cf1_nominal/(1 + r_nominal)
pv_cf1_nominal

#Check
identical(pv_cf1_real, pv_cf1_nominal)

### E ----
t <- 1:4

cf_real <- cf*(1 + g)^t
cf_real

### F ----
pv_cf_real <- sum(cf_real/(1 + r_real)^t)
pv_cf_real

### G ----
cf_nominal <- cf*((1 + g)^t)*((1 + pi)^t)
cf_nominal

### H ----
pv_cf_nominal <- sum((cf_nominal)/(1 + r_nominal)^t)
pv_cf_nominal

##Check
identical(pv_cf_real, pv_cf_nominal)

# Problem Set ----

## Q1 ----
prob <- c(0.30, 0.50, 0.20)
phi <- c(0.60, 0.20, 0.10)

bond_payoff <- c(100, 100, 100)
stock_payoff <- c(90, 105, 120)

### A ----
p_bond <- sum(bond_payoff*phi)
round(p_bond, 5) 

### B ----
p_stock <- sum(stock_payoff*phi)
round(p_stock, 5)

### C ----
E_bond <- sum(bond_payoff*prob)

return_bond <- E_bond/p_bond-1
return_bond

round(return_bond*100, 6)

### D ----
E_stock <- sum(stock_payoff*prob)

return_stock <- E_stock/p_stock - 1
return_stock

round(return_stock*100, 6)

## Q2 ----

### A ----
a_payoff <- c(2.40, 0.60)
b_payoff <- c(0.60, 2.40)

prob <- c(0.50, 0.50)

p_a <- 1.40
p_b <- 1.60

prices <- c(p_a, p_b)

x <- matrix(
  c(a_payoff, b_payoff),
  nrow = 2,
  byrow = TRUE
)

x

phi <- solve(x, prices)

round(phi, 6)

### B ----
c_payoff <- c(5.50, 9.50)

c_price <- sum(c_payoff*phi)

round(c_price, 6)

### C ----

d_payoff <- c(100, 100)
p_d <- sum(d_payoff*phi)
p_d

E_d <- sum(d_payoff*prob)

rf_return <- E_d/p_d - 1

round(rf_return, 6)

sum(phi)

## Q3 ----

age_0 <- 5
age_college <- 18

t <- age_college - age_0

r <- 0.029

final <- 100000

cf0 <- final/(1 + r)^t

round(cf0, 6)

## Q4 ----

### A ----
t <- c(0, 5, 10, 50)
cf <- 1500000
r <- 0.07

pv_cf <- cf/(1 + r)^t

round(pv_cf/1000000, 6)


### B ----
r_2 <- 0.10

pv_cf <- cf/(1 + r_2)^t

round(pv_cf/1000000, 6)

## Q5 ----

### A ----

cf0 <- 24

t <- 2020 - 1626

r <- 0.05

cf_terminal <- cf0*(1 + r)^t

round(cf_terminal/10^9, 6)

### B ----
r2 <- 0.06

cf_terminal <- cf0*(1 + r2)^t

round(cf_terminal/10^9, 6)

### C ----
r3 <- 0.07

cf_terminal <- cf0*(1 + r3)^t

round(cf_terminal/10^12, 6)


## Q6 ----
sales_0 <- 15000000
g_real <- 0.05
t <- 1:3
pi <- 0.02
r_nominal <- 0.12

### A ----
sales_nominal <- sales_0*((1 + g_real)^t)*((1 + pi)^t)
round(sales_nominal/10^6, 6)

### B ----
pv_nominal <- sum(sales_nominal/(1 + r_nominal)^t)
round(pv_nominal/10^6, 6)

### C ----
sales_real <- sales_0*(1 + g_real)^t
round(sales_real/10^6, 6)

### D ----
r_real <- (1 + r_nominal)/(1 + pi) - 1

pv_real <- sum(sales_real/(1 + r_real)^t)

round(pv_real/10^6, 6)

## Check
identical(pv_nominal, pv_real)
