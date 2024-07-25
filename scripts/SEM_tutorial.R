
library(tidyverse)

# Create a simple structural model

# A -> B -> C -> D

DF01 <-
  data.frame(
  A = runif(100),
  B_e = rnorm(100, 0, 0.4),
  C_e = rnorm(100, 0, 0.1),
  D_e = rnorm(100, 0, 0.3)
) %>%
  mutate(
    B = 2 + A*1.5 + B_e,
    C = -1.5 + B*0.5 + C_e,
    D = 2.5 + C *-1 + D_e
  )

lm(D ~ A + B + C, data = DF01) %>% summary()

lm_AB <- lm(B ~ A, data = DF01)
summary(lm_AB)

DF01 <-
  DF01 %>%
  mutate(
    resid_AB = resid(lm_AB)
  )

lm_BC <- lm(C ~ resid_AB, data = DF01)
summary(lm_BC)

DF01 <-
  DF01 %>%
  mutate(
    resid_BC = resid(lm_BC)
  )

lm_CD <- lm(D ~ resid_BC, data = DF01)
summary(lm_CD)

DF01 <-
  DF01 %>%
  mutate(
    resid_CD = resid(lm_CD)
  )

DF01$resid_CD %>% sd()

DF01$resid_BC %>% sd()

DF01$resid_AB %>% sd()

# A -> C, B -> C, C -> D
DF02 <-
  data.frame(
    A = runif(100),
    B = runif(100),
    C_e = rnorm(100, 0, 0.1),
    D_e = rnorm(100, 0, 0.3)
  ) %>%
  mutate(
    C = 1 + A*2 + B*0.5 + C_e,
    D = 2.5 + C *-1 + D_e
  )

lm(D ~ A + B + C, data = DF02) %>% summary()
# A1 -> L, A2 -> L, A3 -> L, L -> D

lm02_AC <- lm(C ~ A, data = DF02)
lm02_AC %>% summary()

lm02_BC <- lm(C ~ B, data = DF02)
lm02_BC %>% summary()

DF02 <-
  DF02 %>%
  mutate(
    resid_AC = resid(lm02_AC),
    resid_BC = resid(lm02_BC),
    resid_C = resid_AC + resid_BC
  )

lm02_CD <- lm(D ~ resid_C, data = DF02)
lm02_CD %>% summary()
