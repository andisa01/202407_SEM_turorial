
###
# SEM Tutorial
# Sourceable script to create toy example data
###

set.seed(109)

n <- 200

weather <- rnorm(n, mean = 20, sd = 5)

ice_cream_sales <- 50 + 2 * weather + rnorm(n, mean = 0, sd = 5)

shark_attacks <- 5 + 0.3 * weather + rnorm(n, mean = 0, sd = 5)

example01_data_anon <- 
  data.frame(shark_attacks, weather, ice_cream_sales) %>%
  rename(
    Y = shark_attacks,
    X1 = weather,
    X2 = ice_cream_sales
  )