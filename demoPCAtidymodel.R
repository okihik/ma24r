## Dimensionality Reduction
# 16.2 A PICTURE IS WORTH A THOUSAND… BEANS
library(tidymodels)
tidymodels_prefer()
library(beans)

set.seed(1601)
bean_split <- initial_validation_split(beans, strata = class, prop = c(0.75, 0.125))
#> Warning: Too little data to stratify.
#> • Resampling will be unstratified.
bean_split
#> <Training/Validation/Testing/Total>
#> <10206/1702/1703/13611>

# Return data frames:
bean_train <- training(bean_split)
bean_test <- testing(bean_split)
bean_validation <- validation(bean_split)


set.seed(1602)
# Return an 'rset' object to use with the tune functions:
bean_val <- validation_set(bean_split)
bean_val$splits[[1]]
#> <Training/Validation/Total>
#> <10206/1702/11908>
#> 

library(corrplot)
tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
bean_train %>% 
  select(-class) %>% 
  cor() %>% 
  corrplot(col = tmwr_cols(200), tl.col = "black", method = "ellipse")


# 16.3 A STARTER RECIPE
library(bestNormalize)
bean_rec <-
  # Use the training data from the bean_val split object
  recipe(class ~ ., data = bean_train) %>%
  step_zv(all_numeric_predictors()) %>%
  step_orderNorm(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# 16.4 RECIPES IN THE WILD
bean_rec_trained <- prep(bean_rec)
bean_rec_trained

bean_rec_trained %>% 
  step_dummy(cornbread) %>%  # <- not a real predictor
  prep(verbose = TRUE)
#> oper 1 step zv [pre-trained]
#> oper 2 step orderNorm [pre-trained]
#> oper 3 step normalize [pre-trained]
#> oper 4 step dummy [training]
#> Error in `step_dummy()`:
#> Caused by error in `prep()`:
#> ! Can't subset columns that don't exist.
#> ✖ Column `cornbread` doesn't exist.

show_variables <- 
  bean_rec %>% 
  prep(log_changes = TRUE)
#> step_zv (zv_RLYwH): same number of columns
#> 
#> step_orderNorm (orderNorm_Jx8oD): same number of columns
#> 
#> step_normalize (normalize_GU75D): same number of columns

# 16.4.2 BAKING THE RECIPE
bean_val_processed <- bake(bean_rec_trained, new_data = bean_validation)

library(patchwork)
p1 <- 
  bean_validation %>% 
  ggplot(aes(x = area)) + 
  geom_histogram(bins = 30, color = "white", fill = "blue", alpha = 1/3) + 
  ggtitle("Original validation set data")

p2 <- 
  bean_val_processed %>% 
  ggplot(aes(x = area)) + 
  geom_histogram(bins = 30, color = "white", fill = "red", alpha = 1/3) + 
  ggtitle("Processed validation set data")

p1 + p2

bake(bean_rec_trained, new_data = NULL) %>% nrow()
#> [1] 10206
bean_train %>% nrow()
#> [1] 10206

library(ggforce)
plot_validation_results <- function(recipe, dat = bean_validation) {
  recipe %>%
    # Estimate any additional steps
    prep() %>%
    # Process the data (the validation set by default)
    bake(new_data = dat) %>%
    # Create the scatterplot matrix
    ggplot(aes(x = .panel_x, y = .panel_y, color = class, fill = class)) +
    geom_point(alpha = 0.4, size = 0.5) +
    geom_autodensity(alpha = .3) +
    facet_matrix(vars(-class), layer.diag = 2) + 
    scale_color_brewer(palette = "Dark2") + 
    scale_fill_brewer(palette = "Dark2")
}

# 16.5.1 PRINCIPAL COMPONENT ANALYSIS
bean_rec_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  plot_validation_results() + 
  ggtitle("Principal Component Analysis")

