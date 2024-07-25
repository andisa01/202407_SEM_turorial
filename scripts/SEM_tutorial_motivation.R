# library(lavaan)
# library(tidyverse)

for(i in 1:1000){
  set.seed(i)
  
  n <- 200
  
  weather <- rnorm(n, mean = 20, sd = 5)
  
  ice_cream_sales <- 50 + 2 * weather + rnorm(n, mean = 0, sd = 5)
  
  shark_attacks <- 5 + 0.3 * weather + rnorm(n, mean = 0, sd = 5)
  
  data <- data.frame(shark_attacks, weather, ice_cream_sales)
  
  lm_ex_01 <- lm(shark_attacks ~ weather + ice_cream_sales, data = data)
  
  if(summary(lm_ex_01)$coef[3,4] < 0.05 & summary(lm_ex_01)$coef[2,4] > 0.05){
    print(paste0(i, " p = ", summary(lm_ex_01)$coef[3,4]))
  }
}

set.seed(109)

n <- 200

weather <- rnorm(n, mean = 20, sd = 5)

ice_cream_sales <- 50 + 2 * weather + rnorm(n, mean = 0, sd = 5)

shark_attacks <- 5 + 0.3 * weather + rnorm(n, mean = 0, sd = 5)

example01_data <- data.frame(shark_attacks, weather, ice_cream_sales)

example01_data_anon <- 
  example01_data %>%
  rename(
    Y = shark_attacks,
    X1 = weather,
    X2 = ice_cream_sales
  )

write.csv(example01_data_anon, "./data/example01_data_anon.csv", row.names = FALSE)

lm(shark_attacks ~ weather + ice_cream_sales + weather:ice_cream_sales, data = data) %>% summary()
lm(shark_attacks ~ weather + ice_cream_sales, data = data) %>% summary()
lm(shark_attacks ~ ice_cream_sales, data = data) %>% summary()

lm_ex_01 <- lm(shark_attacks ~ weather + ice_cream_sales, data = data)
summary(lm_ex_01)

data %>%
  ggplot(aes(x = ice_cream_sales, y = shark_attacks)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(x = weather, y = shark_attacks)) +
  geom_point() +
  geom_smooth(method = "lm")

sem_ex_01 <- '
# Measurement model
shark_attacks ~ b1 * weather
ice_cream_sales ~ b1 * weather
'
fit_sem_ex_01 <- sem(sem_ex_01, data = data)
summary(fit_sem_ex_01, standardized = FALSE)
