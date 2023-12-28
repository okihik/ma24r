# PCA in the tidyverse Framework -------------
# https://www.rpubs.com/twyunting/PCA_in_the_tidyverse_Framework
# Compressive Strength of Concrete Mixtures Data

library(ggbiplot)
library(tidymodels)
data(concrete)
head(concrete)

dim(concrete)
length(map_dbl(concrete, mean)) == length(concrete)
map_dbl(concrete, sd)

# Principal Component Analysis ------
# scale the variables to have standard deviation one
concrete_pca <- prcomp(~ . -compressive_strength, 
                       data = concrete, scale. = TRUE) 
concrete_pca

# NO scale the variables
concrete_pca_noscale <- prcomp(~ . -compressive_strength, 
                               data = concrete, scale. = FALSE,
                               rank. = 8) # rank is redundant
concrete_pca_noscale

names(concrete_pca)
concrete_pca$center
concrete_pca$scale

# Explore the loading, eigenvalues, and final projection using the broom package.
concrete_pca %>%
  tidy(matrix = "u") %>% 
  head()

# return the rotation matrix
concrete_pca %>%
  tidy(matrix = "loadings") # v", "rotation", "loadings" or "variables":

# 
concrete_pca %>%
tidy(matrix = "pcs") # "d", "eigenvalues" or "pcs": returns information about the eigenvalues.


# Data Visualization --------
# scaled and not scaled
augment(concrete_pca, newdata = concrete) %>% 
  head()
augment(concrete_pca_noscale, newdata = concrete) %>% 
  head()

predict(concrete_pca_noscale, newdata = concrete)

# Variance explained 
concrete_pca %>%
  tidy("pcs") %>%
  ggplot(aes(x=PC, y=percent))+
  geom_col(fill="dodgerblue", alpha=0.7) +
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+
  labs(y= "Variance explained",
       title="Scree plot")

# Cumulative variance explained
concrete_pca %>%
  tidy("pcs") %>%
  ggplot(aes(x=PC, y=cumulative))+
  geom_point(size=4) +
  geom_line(color="dodgerblue", linewidth=2)+
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+
  labs(y= "Cumulative Variance explained",
       title="Scree plot")

variance_exp <- concrete_pca %>%  
  tidy("pcs") %>% 
  pull(percent)

concrete_pca %>%
  augment(concrete %>% drop_na()) %>%
  rename_with(function(x){gsub(".fitted","",x)}) %>%
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp[1]*100), "%"),
       y = paste0("PC2: ",round(variance_exp[2]*100), "%"))


# See the location of rotated observations in the first principal direction (PC1) space
concrete_pca %>%
  tidy(matrix = "loadings") %>%
  filter(PC ==1) %>%
  ggplot(aes(value, column)) +
  geom_col() +
  theme_bw()

# see the outcomes on each PCA
concrete_pca %>%
  tidy(matrix = "loadings") %>%
  ggplot(aes(value, column)) +
  geom_col() +
  facet_wrap(~PC)

# The first component explains most of the variation in the data
concrete_pca %>%
  tidy(matrix = "pcs") %>%
  ggplot(aes(PC, percent)) +
  geom_col() +
  theme_bw() +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )

map_dbl(concrete, ~length(unique(.x)))

ggbiplot::ggbiplot(concrete_pca, arrow.color = red)

## Use the {recipes} package ----
set.seed(1234)
concrete_split <- initial_split(concrete, strata = "compressive_strength")
concrete_train <- training(concrete_split)
concrete_test <- testing(concrete_split)

rec_spec <- recipe(compressive_strength ~., data = concrete_train) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), threshold = 0.75)

lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

pcr_wf <- workflow() %>%
  add_recipe(rec_spec) %>%
  add_model(lm_spec)

pcr_fit <- fit(pcr_wf, data = concrete_train)
pcr_fit

augment(pcr_fit, new_data = concrete_test) %>%
  rmse(truth = compressive_strength, estimate = .pred) # root mean squared error

augment(pcr_fit, new_data = concrete_test) %>%
  ggplot(aes(compressive_strength, .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "green") +
  coord_fixed() +
  theme_bw()

# Python and R Tips ---------
# https://cmdlinetips.com/2022/12/pca-with-tidyverse/







