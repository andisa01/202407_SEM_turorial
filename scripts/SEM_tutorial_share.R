
####
# SEM Tutorial
# Yale University
# 2024 07 24
####

# Set up the environment ====

# source("https://raw.githubusercontent.com/andisa01/andis_utils/main/00_HelperFunctions.R") # Andis' collection of helperfunctions

# Install and load packages ====
packages_for_sem_workshop <- 
  c(
    'tidyverse',
    'tidygraph',
    'ggraph',
    'lavaan',
    'piecewiseSEM',
    'mgcv',
    'lme4',
    'cvsem'
  )

install_and_load_packages <-
  function(x){
    for( i in x ){
      if( ! require( i , character.only = TRUE ) ){
        install.packages( i , dependencies = TRUE )
        library( i , character.only = TRUE )
      }
    }
  }

install_and_load_packages(packages_for_sem_workshop)

sessionInfo()

## Simple model =====
# Simulate a toy dataset
set.seed(999)

simple_ex <-
  data.frame(
    x = runif(n = 100, min = 0, max = 10),
    e = rnorm(n = 100, 0, 1)
  )

simple_ex <-
  simple_ex %>%
  mutate(
    y = 1 + 2.5*x + e
  )

## . Fit simple regression ====
fit_simple_ex_lm <- 
  lm(y ~ x, data = simple_ex)

summary(fit_simple_ex_lm)

## . Fit simple SEM ====
simple_ex_sem <-
  '
  y ~ x
  y ~ 1
'

fit_simple_ex_sem <-
  sem(model = simple_ex_sem,
      data = simple_ex)

summary(fit_simple_ex_sem)

fit_simple_ex_lm %>%
  resid() %>%
  var()

## Motivational example ====
# . Get data ====
source("https://raw.githubusercontent.com/andisa01/202407_SEM_turorial/main/scripts/SEM_tutorial_example_source.R")

## . Stepwise regression =====
# Fit the full or 'global model'
mod_ex01_full <- lm(Y ~ X1 + X2 + X1:X2, data = example01_data_anon)
summary(mod_ex01_full)

# Drop the interaction
lm(Y ~ X1 + X2, data = example01_data_anon) %>% 
  summary()
# Now X2 is significant, but X1 is not.

lm(Y ~ X2, data = example01_data_anon) %>% 
  summary() # Drop X1
# Now we have a significant result!!! We can explain 14% of the variation in Y with X2.
# For every 1 unit increase of X2 we expect Y to increase by 0.16 units!!!

# There's even a package for this: https://www.rdocumentation.org/packages/MuMIn/versions/1.48.4/topics/dredge
options(na.action = "na.fail")
MuMIn::dredge(mod_ex01_full)

# Job done! Let's publish a paper?!

## SEM modeling ====
# . Consider other model structures ====
# Visualize the alternative structures
nodes <- 
  tibble(
    name = c("X1", "X2", "Y"),
    type = c("input", "input", "outcome")
  )

edges <- 
  tibble(
    from = "X1",
    to = "Y"
) %>% bind_rows(
  tibble(
    from = "X2",
    to = "Y"
  )
)

nodes
edges

graph01 <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE) # Create a graph object

graph01 %>% str()
graph01

graph01 %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name)),
                 arrow = arrow(length = unit(6, 'mm')), 
                 end_cap = circle(4, 'mm'),
                 edge_width = 1.5) + 
  geom_node_point(size = 15, aes(color = type), shape = 21, fill = "white") +
  geom_node_text(aes(label = name, color = type), size = 6) +
  scale_color_manual(values = c("input" = "blue", "output" = "red")) +
  scale_size_manual(values = c(15)) +
  theme(legend.position = "none")

# This is the assumtion that our regression model is making. Both X1 and X2 are independently affecting Y.

# Chain
edges <- 
  tibble(
    from = "X1",
    to = "X2"
  ) %>% bind_rows(
    tibble(
      from = "X2",
      to = "Y"
    )
  )

tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name)),
                 arrow = arrow(length = unit(6, 'mm')), 
                 end_cap = circle(4, 'mm'),
                 edge_width = 1.5) + 
  geom_node_point(size = 15, aes(color = type), shape = 21, fill = "white") +
  geom_node_text(aes(label = name, color = type), size = 6) +
  scale_color_manual(values = c("input" = "blue", "output" = "red")) +
  scale_size_manual(values = c(15)) +
  theme(legend.position = "none")
# Now X1 causes X2 which causes Y. This is called a chain.

# Confounder mediation
edges <- 
  tibble(
    from = "X1",
    to = "Y"
  ) %>% 
  bind_rows(
    tibble(
      from = "X2",
      to = "Y"
    ) 
  ) %>% 
  bind_rows(
    tibble(
      from = "X2",
      to = "X1"
    )
  )

tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name)),
                 arrow = arrow(length = unit(6, 'mm')), 
                 end_cap = circle(4, 'mm'),
                 edge_width = 1.5) + 
  geom_node_point(size = 15, aes(color = type), shape = 21, fill = "white") +
  geom_node_text(aes(label = name, color = type), size = 6) +
  scale_color_manual(values = c("input" = "blue", "output" = "red")) +
  scale_size_manual(values = c(15)) +
  theme(legend.position = "none")
# Here, X2 is a confounder.

# Partial mediator
edges <- 
  tibble(
    from = "X1",
    to = "X2"
  ) %>% 
  bind_rows(
    tibble(
      from = "X1",
      to = "Y"
    ) 
  ) %>% 
  bind_rows(
    tibble(
      from = "X2",
      to = "Y"
    )
  )

tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name)),
                 arrow = arrow(length = unit(6, 'mm')), 
                 end_cap = circle(4, 'mm'),
                 edge_width = 1.5) + 
  geom_node_point(size = 15, aes(color = type), shape = 21, fill = "white") +
  geom_node_text(aes(label = name, color = type), size = 6) +
  scale_color_manual(values = c("input" = "blue", "output" = "red")) +
  scale_size_manual(values = c(15)) +
  theme(legend.position = "none")
# Here, X2 is a mediator

## Fit SEM hypotheses ====
# Let's use SEMs to test which structure best fits our data.

set.seed(666)

# single effect
ex01_formula_x2effect <- '
Y ~ X2
X1 ~~ X1
'

ex01_sem_x2effect <- sem(ex01_formula_x2effect, data = example01_data_anon)

summary(ex01_sem_x2effect)

# chain
ex01_formula_chain <- '
X2 ~ X1
Y ~ X2
'

ex01_sem_chain <- sem(ex01_formula_chain, data = example01_data_anon)

summary(ex01_sem_chain)

# partially mediated
ex01_formula_mediator <- '
X2 ~ X1
Y ~ X2
Y ~ X1
'

ex01_sem_mediator <- sem(ex01_formula_mediator, data = example01_data_anon)

summary(ex01_sem_mediator)

# common cause
ex01_formula_commoncause <- '
X2 ~ X1
Y ~ X1
'

ex01_sem_commoncause <- sem(ex01_formula_commoncause, data = example01_data_anon)

summary(ex01_sem_commoncause)

# common effect (uncorrelated)
ex01_formula_commoneffect <- '
Y ~ X1
Y ~ X2
'

ex01_sem_commoneffect <- sem(ex01_formula_commoneffect, data = example01_data_anon)

summary(ex01_sem_commoneffect)

# common effect (correlated)
ex01_formula_commoneffect2 <- '
Y ~ X1
Y ~ X2
X1 ~~ X2
'

ex01_sem_commoneffect2 <- sem(ex01_formula_commoneffect2, data = example01_data_anon)

summary(ex01_sem_commoneffect2)

# Compare model fits with anova
anova(ex01_sem_chain, ex01_sem_commoneffect, ex01_sem_commoncause, ex01_sem_x2effect, ex01_sem_commoneffect2, ex01_sem_mediator)

## . . Cross validating ====
models_to_cv <- 
  cvgather(
    ex01_sem_chain, 
    ex01_sem_commoneffect, 
    ex01_sem_commoncause, 
    ex01_sem_x2effect, 
    ex01_sem_commoneffect2, 
    ex01_sem_mediator
    )

cvsem(
  data = example01_data_anon,
  Models = models_to_cv,
  k = 10
)

cvsem(
  data = example01_data_anon,
  Models = models_to_cv,
  k = 5
)

cvsem(
  data = example01_data_anon,
  Models = models_to_cv,
  k = 10,
  discrepancyMetric = 'GLS'
)

# We could have expected correlation between X1 and X2
example01_data_anon %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point()

## Estimate coefficients of the model ====

## . Generate additonal data ====
set.seed(666)

n <- 200

weather <- rnorm(n, mean = 20, sd = 5)

ice_cream_sales <- 50 + 2 * weather + rnorm(n, mean = 0, sd = 5)

shark_attacks <- 5 + 0.3 * weather + rnorm(n, mean = 0, sd = 5)

example01_newdata <- data.frame(shark_attacks, weather, ice_cream_sales)

# common cause
ex01_new_commoncause <- '
ice_cream_sales ~ weather
shark_attacks ~ weather
# shark_attacks ~ 1
'

ex01_sem_new_commoncause <- sem(ex01_new_commoncause, data = example01_newdata)

summary(ex01_sem_new_commoncause)

## . Bootstrapping coefficients ====

print("Content to come.")

###
## Complicated data ====
###

# Here's the structure we want to simulate.

nodes <- 
  tibble(
    name = c("canopy", "pond_temp", "embryo_vol", "dev_rate"),
    type = c("ex", "en", "en", "outcome")
  )

edges <- 
  tibble(
    from = "canopy",
    to = "pond_temp",
    type = "linear"
  ) %>% 
  bind_rows(
    tibble(
      from = "pond_temp",
      to = "embryo_vol",
      type = "linear"
    ) 
  ) %>% 
  bind_rows(
    tibble(
      from = "embryo_vol",
      to = "dev_rate",
      type = "linear"
    )
  ) %>% 
  bind_rows(
    tibble(
      from = "canopy",
      to = "embryo_vol",
      type = "linear"
    )
  ) %>% 
  bind_rows(
    tibble(
      from = "pond_temp",
      to = "dev_rate",
      type = "discont"
    )
  )

tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name),
                     col = type),
                 arrow = arrow(length = unit(6, 'mm')), 
                 end_cap = circle(4, 'mm'),
                 edge_width = 1.5) + 
  geom_node_point(size = 15, aes(color = type), shape = 21, fill = "white") +
  geom_node_text(aes(label = name, color = type), size = 6) +
  scale_color_manual(values = c("input" = "blue", "output" = "red")) +
  scale_size_manual(values = c(15)) +
  theme(legend.position = "none")

# Simulate data

"
pond_temp ~ canopy + e1
embryo_vol ~ canopy + pond_temp + e2
dev_rate ~ embryo_vol + pond_temp + e3
"

set.seed(666)

n = 2000

frog_dat <-
  tibble(
    canopy = runif(n = n, min = 0, max = 1),
    e_1 = rnorm(n = n),
    e_2 = rnorm(n = n),
    e_3 = rnorm(n = n),
    maternal_effect = sample(rnorm(n = 10, sd = 2), size = n, replace = TRUE)
  ) %>%
  mutate(
    pond_temp = canopy + e_1,
    embryo_vol = canopy + pond_temp + maternal_effect + e_2,
    dev_rate = 
      ifelse(
        pond_temp < median(pond_temp), 
        pond_temp + embryo_vol + e_3,
        -2*pond_temp + embryo_vol + e_3
      )
  ) 

frog_dat %>%
  ggplot(aes(x = pond_temp, y = dev_rate)) +
  geom_point() +
  geom_smooth(method = 'gam') +
  geom_smooth(method = 'lm', col = "red")

frog_dat %>%
  ggplot(aes(x = embryo_vol, fill = as.factor(maternal_effect))) +
  geom_density(alpha = 0.3) +
  # facet_wrap(vars(as.factor(maternal_effect)), ncol = 1) +
  NULL

# Define alternative structure

psem_01 <-
  psem(
    lm(pond_temp ~ canopy, data = frog_dat),
    lm(embryo_vol ~ canopy + pond_temp , data = frog_dat),
    lm(dev_rate ~ embryo_vol + pond_temp, data = frog_dat),
    data = frog_dat
  )

psem_01 %>% summary(standardize = "none")
basisSet(psem_01)
dSep(psem_01)

piecewiseSEM::getDAG(psem_01)

psem_02 %>% plot()

coefs(psem_01)

psem_02 <-
  psem(
    lm(pond_temp ~ canopy, data = frog_dat),
    lmer(embryo_vol ~ canopy + pond_temp + (1|maternal_effect), data = frog_dat),
    gam(dev_rate ~ embryo_vol + s(pond_temp, bs = 'cs'), family = gaussian, data = frog_dat),
    data = frog_dat
  )


summary(psem_02, standardize = "none")

coefs(psem_02, standardize = "none")

## Extensions ====
## . Latent variables ====
## . COnstruct variables ====