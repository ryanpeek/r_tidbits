# stan test

# create data
schools_data <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

# Convert to a data frame for brms
schools_df <- data.frame(
  school = factor(1:schools_data$J),
  y = schools_data$y,
  sigma = schools_data$sigma
)

library(rstan)

system.time(
  fit1 <- stan(
    file = "schools.stan",  # Stan program
    data = schools_data,    # named list of data
    chains = 4,             # number of Markov chains
    warmup = 1000,          # number of warmup iterations per chain
    iter = 2000,            # total number of iterations per chain
    cores = 1,              # number of cores (could use one per chain)
    refresh = 1             # no progress shown
  )
)

print(fit1, pars=c("theta", "mu", "tau", "lp__"), probs=c(.1,.5,.9))
plot(fit1)
traceplot(fit1, pars = c("mu", "tau"), inc_warmup = TRUE, nrow = 2)

#  user  system elapsed 
# 2.77    0.42  111.94 


# in BRMS

library(brms)

# Fit the hierarchical model
system.time(
  fit <- brm(
    y | se(sigma) ~ 1 + (1 | school),
    data = schools_df,
    family = gaussian(),
    prior = c(
      prior(normal(0, 10), class = "Intercept"),
      prior(student_t(3, 0, 2.5), class = "sd")
    )
  )
)

# user  system elapsed 
# 1.36    0.30  149.87
summary(fit)
posterior_summary(fit, variable = c("b_Intercept", "sd_school__Intercept"))
ranef(fit) # school specific random effects
plot(fit)
pp_check(fit)

library(tidybayes)
(model_fit <- schools_df %>%
  add_predicted_draws(fit) %>%  # adding the posterior distribution
  ggplot(aes(x = school, y = y)) +  
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                  alpha = 0.5, colour = "black") +
  geom_point(data = schools_df, colour = "darkseagreen4", size = 3) +   # raw data
  scale_fill_brewer(palette = "Greys") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.85)))

# Bayesian Point Process ----------------------------------------------------


stan_code <- "

data {
  int<lower=1> N;
  int<lower=1> K;
  int<lower=0,upper=1> y[N];
  matrix[N, K] X;
  vector<lower=0>[N] w;
}

parameters {
  real alpha;
  vector[K] beta;
}

model {

  // Priors
  alpha ~ normal(-3, 2);
  beta ~ normal(0, 1);

  // Poisson point-process likelihood (MaxEnt equivalent)
  for (n in 1:N) {
    target += poisson_log_lpmf(
      y[n] |
      alpha + X[n] * beta + log(w[n])
    );
  }
}

generated quantities {
  vector[N] log_lambda;

  for (n in 1:N) {
    log_lambda[n] = alpha + X[n] * beta;
  }
}
"

## BRMS?
pacman::p_load(brms)

library(brms)

fit <- brm(
  # this is the same
  formula = y ~ 1 + X + offset(log(w)),
  # same as this:
  y ~ . - w + offset(log(w)),
  data = dat,
  family = poisson(link = "log"),
  prior = c(
    prior(normal(-3, 2), class = "Intercept"),
    prior(normal(0, 1), class = "b")
  )
)

# consider weights as an OFFSET: convert before putting in model
dat$w <- ifelse(
  dat$y == 1,
  1,
  study_area / n_background
)

dat$log_w <- log(dat$w)

fit <- brm(
  y ~ 1 + elev + forest + precip + offset(log_w),
  data = dat,
  family = poisson(link = "log"),
  prior = c(
    prior(normal(-3, 2), class = "Intercept"),
    prior(normal(0, 1), class = "b")
  )
)

# also recommend converting lat/lon to equal area projection (3310 or similar)
# then calculate area and weights
# Transform to an appropriate projected CRS
pres_proj <- st_transform(pres_sf, crs = YOUR_PROJECTED_CRS)

bbox <- st_bbox(pres_proj)

study_area <- 
  (bbox["xmax"] - bbox["xmin"]) *
  (bbox["ymax"] - bbox["ymin"])

n_bg <- sum(pres_proj$type == 0)

pres_proj$weight <- ifelse(
  pres_proj$type == 1,
  1,
  study_area / n_bg
)

library(brms)

fit <- brm(
  type ~ 1 + x1 + x2 + x3 + offset(log(weight)),
  data = pres,
  family = poisson(link = "log"),
  prior = c(
    prior(normal(-3, 2), class = "Intercept"),
    prior(normal(0, 1), class = "b")
  ),
  chains = 4,
  cores = 4,
  iter = 4000
)