# -------------------------------------------------------------------------
# preliminary.R
#
# Title: Some preliminary code
# Author: Zaw Myo Tun
# Date: 8 Sept 2020
# -------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)

set.seed(2020)
n <- 10000
prev <- 0.3
case <- rnorm(n*prev, mean = 30, sd = 5)
non_case <- rexp(n*(1 - prev), 0.15)

data.frame(value = c(case, non_case),
           status = rep(c("Case", "Non-case"), times = c(n*prev, n*(1 - prev)))) %>%
  ggplot(aes(x = value, fill = status, colour = status)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 20, linetype = "dashed", size = 1) +
  labs(x = "\nLab test values", y = NULL, colour = NULL, fill = NULL) +
  theme_light()



data.frame(value = case) %>%
  ggplot(aes(x = value)) +
  geom_freqpoly(binwidth = 1, size = 1.3) +
  geom_vline(xintercept = 20, linetype = "dashed", size = 1) +
  labs(x = "\nLab test values", y = NULL, colour = NULL) +
  theme_light()



create_2by2 <- function(cutoff, prevalence, n) {
  case <- rnorm(n*prevalence, mean = 30, sd = 5.5)
  non_case <- rexp(n*(1 - prevalence), 0.15)

  ac <- n*prevalence
  bd <- n - ac
  a <- length(case[case >= cutoff])
  b <- length(non_case[non_case >= cutoff])
  c <- ac - a
  d <- bd - b
  sensi <- round(a/ac, 3)
  speci <- round(d/bd, 3)
  ppv <- round(a/(a + b), 3)

  data.frame(cutoff = cutoff,
             prev = prevalence,
             a, b, c, d, ac, bd, sensi, speci, ppv)
}

dat <- crossing(cutoff = seq(15, 30, 1),
         prev = seq(0.1, 0.9, 0.1))

res <- map2_dfr(dat$cutoff, dat$prev, create_2by2, n = 10000)
