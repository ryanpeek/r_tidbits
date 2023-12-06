# quick brms intro

library(brms)
library(tidyverse)
data("Orange")


# core arguments:
# formula
# data
# family

# additional arguments
# prior
# warmup
# iter
# chains
# cores (can check how many with parallel::detectCores())
# control
# file


# Test a Model ------------------------------------------------------------

## Explore shape of our data
ggplot(Orange, aes(circumference)) +
  geom_histogram()

ggplot(Orange, aes(age, circumference)) +
  geom_smooth()

## Pick a model
set.seed(1992)
m1 <- brm(circumference ~  age, data = Orange)

## check output
summary(m1)

# make it more specific
set.seed(1992)
m2 <- brm(circumference ~ age + (1|Tree),
          data = Orange,
          warmup = 1000, #burn in period
          iter = 2000, # actual samples
          chains = 4,
          prior = c(prior(normal(0,1), class = b), # specify your mean and variance, weakly informative prior
                    prior(normal(0,1), class = Intercept)))

## Assess model
summary(m2)
loo(m2)

## Posterior predictive check:
pp_check(m2, type = "dens_overlay", ndraws = 100) # observed data density and the posterior predictive distribution, ndraws is how many samples from the posterior distribution
pp_check(m2, type = "stat", stat = 'median', ndraws = 100) # calc median of the posterior predictive distribution

pp_check(m2,type = "intervals_grouped", group = "Tree") #grouped by Tree

## Trace plot
mcmc_plot(m2)

## Conditional effects
conditional_effects(m2) #no group level effects without re_formula = NULL

loo_compare(loo(m1), loo(m2))
