# libraries
library(tidyverse)
library(corrplot)
library(foreign)
library(rethinking)
library(ggdag)
library(ggplot2)
data <- read.csv("nasa93_subset.csv")
# transforming data as a tibble
data <- as_tibble(data)
# removing columns
data <- data[-c(1:2)]
# defining factors
data$cat2 <- as.factor(data$cat2)
data$cplx <- as.factor(data$cplx)
data$acap <- as.factor(data$acap)
data$pcap <- as.factor(data$pcap)
# change the names for the factor levels
data$cplx <- fct_recode(data$cplx, "1" = "vl", "2" = "l", "3" = "n", "4" = "h", "5" = "vh", "6" = "xh")
data$acap <- fct_recode(data$acap, "1" = "vl", "2" = "l", "3" = "n", "4" = "h", "5" = "vh", "6" = "xh")
data$pcap <- fct_recode(data$pcap, "1" = "vl", "2" = "l", "3" = "n", "4" = "h", "5" = "vh", "6" = "xh")
# column transformation to numeric for later analysis
data$cat2 <- as.numeric(data$cat2)
data$cplx <- as.numeric(data$cplx)
data$acap <- as.numeric(data$acap)
data$pcap <- as.numeric(data$pcap)
# defining models
m4 <- ulam(
  alist(
    act_effort ~ exponential(lambda),
    log(lambda) <- a + bC * a_cplx[cplx] + bA * a_acap[acap] + bP * a_pcap[pcap],
    a ~ normal(0, 0.5),
    bC ~ normal(0, 1),
    bA ~ normal(0, 1),
    bP ~ normal(0, 1),
    a_cplx[cplx] ~ normal(0,3),
    a_acap[acap] ~ normal(0,3),
    a_pcap[pcap] ~ normal(0,3)
  ), data = data, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE
)
m0 <- ulam(
  alist(
    act_effort ~ dgampois(lambda, phi),
    log(lambda) <- alpha,
    phi ~ exponential(1),
    alpha ~ normal(0,2)
  ), data = data, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE
)
m1 <- ulam(
  alist(
    act_effort ~ dgampois(lambda, phi),
    log(lambda) <- a_cplx[cplx],
    phi ~ exponential(1),
    a_cplx[cplx] ~ normal(0,3)
  ), data = data, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE
)
m2 <- ulam(
  alist(
    act_effort ~ dgampois(lambda, phi),
    log(lambda) <- a_acap[acap],
    phi ~ exponential(1),
    a_acap[acap] ~ normal(0,3)
  ), data = data, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE
)
m3 <- ulam(
  alist(
    act_effort ~ dgampois(lambda, phi),
    log(lambda) <- a_pcap[pcap],
    phi ~ exponential(1),
    a_pcap[pcap] ~ normal(0,3)
  ), data = data, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE
)
# sampling diagnostics
precis(m4, depth = 2)
#trankplot(m0)
#trankplot(m1) 
# posterior predictive checks
postcheck(m4)

plot(precis(m1, depth=2))
