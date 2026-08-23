# Foundations of Modern Finance
# Module 3: Discounting and Compounding ----

# Load packages
library(tidyverse)

# Set options
options(digits = 15)

# Checks ----

## 2.2 ----
t <- 15
a <- 100
g <- 0.05
r <- 0.10

pv_a <- a/(r - g)*(1 - ((1 + g)/(1 + r))^t)
pv_a

# Recitation ----

## R3.1 ----

### A ----

t <- 10
r <- 0.05
a <- 2300

pv_a <- a/r*(1 - 1/(1 + r)^t)
pv_a

### B ----
tab <- tibble(
  year = 1:t,
  beginning = NA_real_,
  interest = NA_real_,
  payment = a,
  ending = NA_real_
)

tab$beginning[1] <- pv_a

for(i in 1:t){
  tab$interest[i] <- tab$beginning[i]*r
  tab$ending[i] <- tab$beginning[i] + tab$interest[i] - tab$payment[i]
  
  if(i < t)
    tab$beginning[i + 1] <- tab$ending[i]
  
}

tab

## R3.2 ----
t <- 15
t_first <- 5
a <- 2300
r <- 0.05

t_gap <- t - t_first

pv_t5 <- a/r*(1 - 1/(1 + r)^t_gap)
pv_t5

pv_t0 <- pv_t5/(1 + r)^(t - t_gap)
pv_t0


## R3.3 ----

t <- 15
a <- 5000000
g <- 0.04
r <- 0.09

pv_t0 <- a/(r - g)*(1 - ((1 + g)/(1 + r))^t)
pv_t0

## R3.4 ----

### A ----

r <- 0.07
p <- 650000

pv_t0 <- p/r
pv_t0

### B ----

pv_redevelop_t0 <- 1000000
rental_prof <- 950000
t <- 4

pv_t4 <- rental_prof/r
pv_t0 <- pv_t4/(1 + r)^t

pv_t0


pv_all_t0 <- pv_t0 - pv_redevelop_t0
pv_all_t0

## R3.5 ----

### A ----
r <- 0.08

pv <- 10^10

ca <- pv*r

round(ca/10^6, 6)


### B ----

cb <- pv*(1 + 1/r)^(-1)

cb

### C ----
g <- 0.02

cb <- pv*(r - g)/(1 + g)

round(cb, 6)

# Problem Set ----

## Q1 ----
r <- 0.07
a <- 5000

t_last <- 10
t_first <- 3
t_annuity <- 8
t_gap <- 2


pv_t2 <- a/r*(1 - 1/(1 + r)^t_annuity)

pv_t2


pv_t0 <- pv_t2/(1 + r)^t_gap
round(pv_t0, digits = 6)

## Q2 ----
r <- 0.03
p <- 800000

pv_t0 <- p/r
pv_all <- p + pv_t0
round(pv_all, digits = 6)


## Q3 ----

t <- 65 - 29 - 1
t_mba <- 2

tuition <- 65000
salary <- 50000

r <- 0.09
rf <- 0.04
g <- 0.04

### A ----
pv_salary_t0 <- salary/r*(1 - 1/(1 + r)^t)

pv_salary_all <- salary + pv_salary_t0

round(pv_salary_all, digits = 6)

### B ----

pv_mba_t0 <- tuition/r*(1 - 1/(1 + r)^t_mba)

round(pv_mba_t0, digits = 6)


### C ----
pv <- pv_mba_t0 + pv_salary


## Q4 ----
## Q5 ----
## Q6 ----
## Q7 ----
## Q8 ----
