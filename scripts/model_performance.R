# model performance eval

library(performance)
library(tidyverse)
library(tidymodels)
library(see)

mpg <- mpg
str(mpg)

# Make a model ------------------------------------------------------------

# linear
model_lm_tidy <- linear_reg() %>%
  set_engine("lm") %>%
  fit(hwy ~ displ + class, data=mpg)

check_model(model_lm_tidy)
