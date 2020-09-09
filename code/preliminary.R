# -------------------------------------------------------------------------
# preliminary.R
#
# Title: Some preliminary code
# Author: Zaw Myo Tun
# Date: 8 Sept 2020
# -------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)

# Data density plot -------------------------------------------------------

sim <- function(prevalence, n) {
  case <- rnorm(n*prevalence, mean = 35, sd = 6.5)
  non_case <- rexp(n*(1 - prevalence), 0.13)
  list(case = case, non_case = non_case)
}

set.seed(2020)
n <- 10000
prev <- 0.1
dat <- sim(prevalence = prev, n = n)

data.frame(value = c(dat$case, dat$non_case),
           status = rep(c("Case", "Non-case"), times = c(n*prev, n*(1 - prev)))) %>%
  ggplot(aes(x = value, fill = status, colour = status)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 20, linetype = "dashed", size = 1) +
  labs(x = "\nLab test values", y = NULL, colour = NULL, fill = NULL,
       title = "Density of laboratory test values") +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  theme_light() +
  theme(panel.grid.minor = element_blank())


# Test properties ---------------------------------------------------------

create_2by2 <- function(cutoff, prevalence, ...) {
  dat <- sim(prevalence = prevalence, ...)

  ac <- n*prevalence
  bd <- n - ac
  a <- length(dat$case[dat$case >= cutoff])
  b <- length(dat$non_case[dat$non_case >= cutoff])
  c <- ac - a
  d <- bd - b
  sensi <- round(a/ac, 3)
  speci <- round(d/bd, 3)
  ppv <- round(a/(a + b), 3)
  npv <- round(d/(c + d), 3)

  data.frame(cutoff = cutoff,
             prev = prevalence,
             a, b, c, d, ac, bd, sensi, speci, ppv, npv)
}

set.seed(2020)
n <- 10000
param <- crossing(cutoff = seq(10, 30, 1),
                  prev = seq(0.1, 0.9, 0.1))
res <- map2_dfr(param$cutoff, param$prev, create_2by2, n = n)

res %>%
  pivot_longer(c(ppv, npv)) %>%
  ggplot(aes(x = prev, y = value, colour = name)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0.1, 0.9), breaks = seq(0, 1, 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~cutoff) +
  labs(x = "Disease prevalence", y = NULL, colour = NULL) +
  theme_light() +
  theme(panel.grid.minor = element_blank())

res %>%
  pivot_longer(c(sensi, speci)) %>%
  ggplot(aes(x = cutoff, y = value, colour = name)) +
  geom_line(size = 1) +
  facet_wrap(~prev) +
  labs(x = "Cutoff to define a positive test", y = NULL, colour = NULL) +
  theme_light()
