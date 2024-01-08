## food waste ----

# aML of food loss -------------------------------------------------------------
aml_fl <- food_loss_kg ~ temp_c + humi_p + prcp_mm + 
  tueE + wedE + thuE + friE + satE +
  container + liquors + sales + halfs
aml_fl <- lm(aml_fl, data = df, subset = (!df$is_closed))
summary(aml_fl)

# aML of food waste ------------------------------------------------------------
aml_fw <- food_waste_kg ~ temp_c + humi_p + prcp_mm + 
  tueE + wedE + thuE + friE + satE +
  container + liquors + sales + halfs
aml_fw <- lm(aml_fw, data = df, subset = (!df$is_closed))
summary(aml_fw)

# aML of solid food waste ------------------------------------------------------
aml_sfw <- solid_waste_kg ~ temp_c + humi_p + prcp_mm + 
  tueE + wedE + thuE + friE + satE +
  container + liquors + sales + halfs
aml_sfw <- lm(aml_sfw, data = df, subset = (!df$is_closed))
summary(aml_sfw)

# aML of liquid food waste -----------------------------------------------------
aml_lfw <- liquid_waste_kg ~ temp_c + humi_p + prcp_mm + 
  tueE + wedE + thuE + friE + satE +
  container + liquors + sales + halfs
aml_lfw <- lm(aml_lfw, data = df, subset = (!df$is_closed))
summary(aml_lfw)

## food waste per customer ----
# aML of food waste ------------------------------------------------------------
aml_p_fw <- food_waste_p_kg ~ temp_c + humi_p + prcp_mm + 
  tueE + wedE + thuE + friE + satE +
  container + liquors + sales + halfs

summary(aml_p_fw <- lm(formula =  aml_p_fw, 
                       data = df, subset = (!df$is_closed)))

# aML of solid food waste ------------------------------------------------------
aml_p_sfw <- solid_waste_p_kg ~ temp_c + humi_p + prcp_mm + 
  tueE + wedE + thuE + friE + satE +
  container + liquors + sales + halfs
summary(aml_p_sfw <- lm(aml_p_sfw, 
                        data = df, subset = (!df$is_closed)))

# aML of liquid food waste -----------------------------------------------------
aml_p_lfw <- liquid_waste_p_kg ~ temp_c + humi_p + prcp_mm + 
  tueE + wedE + thuE + friE + satE +
  container + liquors + sales + halfs

summary(aml_p_lfw <- lm(aml_p_lfw, 
                        data = df, subset = (!df$is_closed)))


#### Assumptions Check for food waste per customer
# ```{r ass ckeck food waste per customer}
# check the models with 1. assumptions -----------------------------------------


## check the models with 2. assumptions ----------------------------------------
# check autocorrelation --------------------------------------------------------
library(performance)
check_autocorrelation(aml_p_fw)
check_autocorrelation(aml_p_sfw)
check_autocorrelation(aml_p_lfw)

# Phillips-Perron Test ---------------------------------------------------------
# Model: diff(y_t) = (1-r)*y_{t-1} + e_t
# Null: r = 1 -> Stationary Process 
# Alt: otherwise -> Not Stationary Process
PP.test(df$food_waste_p_kg)
PP.test(df$solid_waste_p_kg)
PP.test(df$liquid_waste_p_kg)
# -> pval = 0.01 => likely not unit root process

# Augmented Dickey–Fuller Test -------------------------------------------------
# Model: diff(y_t) = a + bt + r*y_{t-1} + c*diff(y_{t-1}) + ... + e_t
# Null: r = 0 -> Stationary Process 
# Alt:  r < 0 -> Not Stationary Process
adf.test(df$food_waste_p_kg)
adf.test(df$solid_waste_p_kg)
adf.test(df$liquid_waste_p_kg)

# Augmented Dickey–Fuller Test -------------------------------------------------
# Model: diff(y_t) = a + bt + r*y_{t-1} + c*diff(y_{t-1}) + ... + e_t
# Null: r = 0 -> Stationary Process 
# Alt:  r > 0 -> Not Stationary Process
adf.test(df$food_waste_p_kg,   alternative = "explosive")
adf.test(df$solid_waste_p_kg,  alternative = "explosive")
adf.test(df$liquid_waste_p_kg, alternative = "explosive")

# check (3) normality assumption -----------------------------------------------
check_normality(aml_p_fw)
check_normality(aml_p_sfw)
check_normality(aml_p_lfw)

# check (3) normality assumption -----------------------------------------------

qqnorm(resid(aml_p_fw));qqline(resid(aml_p_fw))
plot(fitted(aml_p_fw), resid(aml_p_fw)); abline(0, 0)
plot(density(resid(aml_p_fw)))
# check outliers (5) -----------------------------------------------------------
check_outliers(aml_p_fw)
check_outliers(aml_p_sfw)
check_outliers(aml_p_lfw)

# check the models with 3-5. assumptions ---------------------------------------
check_model(aml_p_fw)
check_model(aml_p_sfw)
check_model(aml_p_lfw)
# ```


#### Assumptions Check
# ```{r ass. check simple multiple regression}
library(tseries)
# check the models with 1. assumptions -----------------------------------------


## check the models with 2. assumptions ----------------------------------------
# check autocorrelation --------------------------------------------------------
library(performance)
check_autocorrelation(aml)
check_autocorrelation(aml_fw)
check_autocorrelation(aml_sfw)
check_autocorrelation(aml_lfw)

# Phillips-Perron Test ---------------------------------------------------------
# Model: diff(y_t) = (1-r)*y_{t-1} + e_t
# Null: r = 1 -> Stationary Process 
# Alt: otherwise -> Not Stationary Process
PP.test(df$food_loss_kg)
PP.test(df$food_waste_kg)
PP.test(df$solid_waste_kg)
PP.test(df$liquid_waste_kg)
# -> pval = 0.01 => likely not unit root process

# Augmented Dickey–Fuller Test -------------------------------------------------
# Model: diff(y_t) = a + bt + r*y_{t-1} + c*diff(y_{t-1}) + ... + e_t
# Null: r = 0 -> Stationary Process 
# Alt:  r < 0 -> Not Stationary Process
adf.test(df$food_loss_kg)
adf.test(df$food_waste_kg)
adf.test(df$solid_waste_kg)
adf.test(df$liquid_waste_kg)

# Augmented Dickey–Fuller Test -------------------------------------------------
# Model: diff(y_t) = a + bt + r*y_{t-1} + c*diff(y_{t-1}) + ... + e_t
# Null: r = 0 -> Stationary Process 
# Alt:  r > 0 -> Not Stationary Process
adf.test(df$food_loss_kg,    alternative = "explosive")
adf.test(df$food_waste_kg,   alternative = "explosive")
adf.test(df$solid_waste_kg,  alternative = "explosive")
adf.test(df$liquid_waste_kg, alternative = "explosive")

# check (3) normality assumption -----------------------------------------------
check_normality(aml)
check_normality(aml_fw)
check_normality(aml_sfw)
check_normality(aml_lfw)

# check (3) normality assumption -----------------------------------------------

qqnorm(resid(aml_fw));qqline(resid(aml_fw))
plot(fitted(aml_fw), resid(aml_fw)); abline(0, 0)
plot(density(resid(aml_fw)))
# check outliers (5) -----------------------------------------------------------
check_outliers(aml)
check_outliers(aml_fw)
check_outliers(aml_sfw)
check_outliers(aml_lfw)

# check the models with 3-5. assumptions ---------------------------------------
check_model(aml_fl)
check_model(aml_fw)
check_model(aml_sfw)
check_model(aml_lfw)
# ```


rdt_fw

tidyr::gather(data = .)

diag_plots_fw  <- plot(check_model(rdt_fw, panel = FALSE))
diag_plots_sfw <- plot(check_model(rdt_sfw, panel = FALSE))
diag_plots_lfw <- plot(check_model(rdt_lfw, panel = FALSE))

diag_plots_fw[[2]] + ggtitle("Linearity: Food Waste")
diag_plots_sfw[[2]] + ggtitle("Linearity: Solid Food Waste")
diag_plots_lfw[[2]] + ggtitle("Linearity: Liquid Food Waste")

diag_plots_fw[[6]] + ggtitle("QQ Plot: Food Waste")
diag_plots_sfw[[6]] + ggtitle("QQ Plot: Solid Food Waste")
diag_plots_lfw[[6]] + ggtitle("QQ Plot: Liquid Food Waste")

diag_plots_fw[[3]] + ggtitle("Homoscedasticity: Food Waste")
diag_plots_sfw[[3]] + ggtitle("Homoscedasticity: Solid Food Waste")
diag_plots_lfw[[3]] + ggtitle("Homoscedasticity: Liquid Food Waste")

diag_plots_fw[[4]] + ggtitle("Outliers: Food Waste")
