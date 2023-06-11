# libraries
library(tidyverse)
library(corrplot)
library(foreign)
library(rethinking)
library(ggdag)
library(ggplot2)
# Graph
dag <- dagitty::dagitty("dag {
    year -> acap -> a_eff <- pcap
    year -> cat2 -> cplx -> a_eff
    year -> pcap
  }")
tidy_dag <- tidy_dagitty(dag)
ggdag(tidy_dag) +
  theme_dag()
# reading the data
#data <- read.arff("nasa93.arff")
data <- read.csv("nasa93_subset.csv")
# transforming data as a tibble
data <- as_tibble(data)
# detecting missing values
sum(is.na(data))
sum(is.na(data))
# removing columns
data <- data[-c(1:2)]
# defining factors
data$cat2 <- as.factor(data$cat2)
data$cplx <- as.factor(data$cplx)
data$acap <- as.factor(data$acap)
data$pcap <- as.factor(data$pcap)
# change the names for the factor levels
data$cplx <- fct_recode(data$cplx, "vl" = "1", "2" = "l", "3" = "n", "4" = "h", "5" = "vh", "6" = "xh")
data$acap <- fct_recode(data$acap, "vl" = "1", "2" = "l", "3" = "n", "4" = "h", "5" = "vh", "6" = "xh")
data$pcap <- fct_recode(data$pcap, "vl" = "1", "2" = "l", "3" = "n", "4" = "h", "5" = "vh", "6" = "xh")
# column transformation to numeric for later analysis
data$cat2 <- as.numeric(data$cat2)
data$cplx <- as.numeric(data$cplx)
data$acap <- as.numeric(data$acap)
data$pcap <- as.numeric(data$pcap)
# distribution for act_effort
ggplot(data, aes(x=act_effort)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + labs(title="Distribution for variable act_effort", caption="Source: Menzies et al. dataset")
# boxplot for act_effort
ggplot(data = data, aes(y = act_effort)) + geom_boxplot() + scale_x_discrete() + labs(title = "Boxplot for variable act_effort", , caption="Source: Menzies et al. dataset", y = "act_effort")
# detecting outliers
upper_bound <- function(column) {
  q1 <- quantile(column, 0.25)
  q3 <- quantile(column, 0.75)
  interquartile_range <- q3 - q1
  ub <- q3 + 1.5 * interquartile_range
  return(ub)
}
#data <- subset(data, act_effort <= upper_bound(data$act_effort))
# boxplot after removing outliers
#ggplot(data = data, aes(y = act_effort)) + geom_boxplot() + scale_x_discrete() + labs(title = "Boxplot for variable act_effort", caption="Source: Menzies et al. dataset", y = "act_effort")
# cheking the mean and the variance
var(data$act_effort)
mean(data$act_effort)
# random sample for prior predictive check
max(rlnorm(1e5, 0, 2))
# defining models
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
    log(lambda) <- a_pcap[pcap],
    phi ~ exponential(1),
    a_pcap[pcap] ~ normal(0,3)
  ), data = data, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE
)
# sampling diagnostics
precis(m0)
trankplot(m0)
precis(m1, depth = 2)
trankplot(m1) 
# posterior predictive checks
postcheck(m1)
# compare models
compare(m0,m1,func=LOO)
# plot
plot(precis(m1, depth=2, pars = "a_pcap"))
