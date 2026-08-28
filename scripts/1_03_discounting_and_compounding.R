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

rm(list = ls())

## R3.1 ----

### A

t <- 10
r <- 0.05
a <- 2300

pv_a <- a/r*(1 - 1/(1 + r)^t)
pv_a

### B
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


rm(list = ls())

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



rm(list = ls())

## R3.3 ----

t <- 15
a <- 5000000
g <- 0.04
r <- 0.09

pv_t0 <- a/(r - g)*(1 - ((1 + g)/(1 + r))^t)
pv_t0


rm(list = ls())

## R3.4 ----

### A

r <- 0.07
p <- 650000

pv_t0 <- p/r
pv_t0

### B

pv_redevelop_t0 <- 1000000
rental_prof <- 950000
t <- 4

pv_t4 <- rental_prof/r
pv_t0 <- pv_t4/(1 + r)^t

pv_t0


pv_all_t0 <- pv_t0 - pv_redevelop_t0
pv_all_t0


rm(list = ls())

## R3.5 ----

### A
r <- 0.08

pv <- 10^10

ca <- pv*r

round(ca/10^6, 6)


### B

cb <- pv*(1 + 1/r)^(-1)

cb


### C
g <- 0.02

cb <- pv*(r - g)/(1 + g)

round(cb, 6)


rm(list = ls())

## R3.6 ----

r_apr <- 0.20
k <- 12

### A

r_ear <- (1 + r_apr/k)^k - 1

round(r_ear, 6)

### B

r_ear <- 0.25

k <- c(1, 2, 4, 12, 24, 58, 365)

r_apr <- k*((r_ear + 1)^(1/k) - 1)

r_apr


rm(list = ls())

## R3.7 ----

### A

# Old loan rate
r_apr_old <- 0.06

r_emr_old <- (1 + r_apr_old)^(1/12) - 1
r_emr_old

# Old loan payment

pv_t0 <- 300000

k_old <- 12*20

c_old <- pv_t0*(r_emr_old)*((1 - 1/(1 + r_emr_old)^k_old))^(-1)

c_old

# New loan rate
r_apr_new <- 0.045

k <- 12

r_ear_new <- (1 + r_apr_new/k)^k -1

r_emr_new <- (1 + r_ear_new)^(1/12) - 1

r_emr_new

# New loan payment

k_new <- 12*20

c_new <- pv_t0*(r_emr_new)*((1 - 1/(1 + r_emr_new)^k_new))^(-1)

c_new

### B
pv_b <- c_old/r_emr_new*(1 - 1/(1 + r_emr_new)^k_new)

pv_b - pv_t0


  
# Problem Set ----

rm(list = ls())

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


rm(list = ls())

## Q2 ----

r <- 0.03
p <- 800000

pv_t0 <- p/r
pv_all <- p + pv_t0
round(pv_all, digits = 6)



rm(list = ls())

## Q3 ----

t <- 65 - 29
t_mba <- 2

tuition <- 65000
salary <- 50000

r <- 0.09
rf <- 0.04
g <- 0.04

### A

pv_salary_t0 <- salary/r*(1 - 1/(1 + r)^t)

pv_salary_t0

round(pv_salary_t0, digits = 6)

### B

pv_mba_t0 <- tuition/rf*(1 - 1/(1 + rf)^t_mba)

round(pv_mba_t0, digits = 6)


### C
pv_t2 <- (pv_mba_t0 + pv_salary_t0)*(1 + r)^2

c <- pv_t2*(r - g)*(1 - ((1 + g)/(1 + r))^(t - t_mba))^(-1)

round(c, 6)

# Alternative

pv_t0 <- (pv_mba_t0 + pv_salary_t0)

c_2 <- pv_t0*((1 + r)^2)*(r - g)*(1 - ((1 + g)/(1 + r))^(t - t_mba))^(-1)

round(c_2, 6)

rm(list = ls())

## Q4 ----

c <- 1250
r <- 0.15
g <- 0.05

### A
pv_t0 <- c/(r - g)

round(pv_t0, 6)

### B
pv_t1 <- c*(1 + g)/(r - g)

round(pv_t1, 6)

# Check

pv_t0*(1 + r) - c

rm(list = ls())

## Q5 ----
r_apr <- 0.13
k <- c(1, 4, 365)

r_ear <- (1 + r_apr/k)^k - 1

round(r_ear*100, 6)

rm(list = ls())

## Q6 ----
t <- 48

r_ear <- 0.02

r_emr <- (1 + r_ear)^(1/12) - 1

cd2 <- 958.33

pv_da_t0 <- 40000

pv_db_t0 <- cd2/r_emr*(1 - 1/(1 + r_emr)^t)


### A
round(pv_db_t0, 6)

### B
pv_da_t0 < pv_db_t0


### C

r_ear_2 <- 0.15

r_emr_2 <- (1 + r_ear_2)^(1/12) - 1

pv_db2_t0 <- cd2/r_emr_2*(1 - 1/(1 + r_emr_2)^t)

round(pv_db2_t0, 6)

### D
pv_da_t0 < pv_db2_t0

rm(list = ls())

## Q7 ----


rm(list = ls())

## Q8 ----

rm(list = ls())
