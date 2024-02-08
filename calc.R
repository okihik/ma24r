library(stargazer)

df_dept <- subset(df[c("food_waste_kg", "solid_waste_kg","liquid_waste_kg")],
                  df$is_closed == FALSE)

stargazer(df_dept, type = "text", digits=2)

df_dept_p <- subset(df[c("food_waste_p_kg", "solid_waste_p_kg","liquid_waste_p_kg")],
                  df$is_closed == FALSE)

stargazer(df_dept_p, type = "text", digits=2)


library(modelsummary, warn.conflicts = FALSE)
library(correlation, warn.conflicts = FALSE)
get_p_stars = function(x) {
  out = correlation(x)
  stars = c("." = .1, "*" = .05, "**" = .01)
  p = modelsummary:::make_stars(out$p, stars)
  out$r = sprintf("%.2f%s", out$r, p)
  out = as.matrix(out)
  return(out)
}

df_des <- as.data.frame(df[,c(62:64,11,13,14,16:18)])
corr_des <- datasummary_correlation(df_des,
                                    output = "data.frame",
                                    method = get_p_stars)[,2:10]

datasummary(All(df_des) ~ Mean + SD, 
            data = df_des, add_columns = corr_des, output = 'markdown',
            title = 'Descriptive statistics and correlation matrix.',
            notes = c('N = 161.'))

library(modelsummary)
library(kableExtra)
library(gt)
# log-multiple linear model with cubic time - food waste ----
# food waste formula

# rdt_cub_fw_log <- log_food_waste_kg ~ container * time + 
#   I(time^2) + I(time^3) +
#   tueE + wedE+ thuE+ friE+ satE+
#   temp_c + humi_p + prcp_mm + 
#   liquors + sales + halfs
# # solid formula
# rdt_cub_sfw_log <- log_solid_waste_kg ~ container * time + 
#   I(time^2) +I(time^3) +
#   tueE + wedE+ thuE+ friE+ satE+
#   temp_c + humi_p + prcp_mm + 
#   liquors + sales + halfs
# # liquid formula
# rdt_cub_lfw_log <- log_liquid_waste_kg ~ container * time + 
#   I(time^2) +I(time^3) +
#   tueE + wedE+ thuE+ friE+ satE+
#   temp_c + humi_p + prcp_mm + 
#   liquors + sales + halfs
# 
# # list of log-multiple linear model with cubic time -----
# rdt_cub_fw_log <- list(
#   "log food waste"   = lm(rdt_cub_fw_log,  data = subset(df, df$is_closed == FALSE)),
#   "food waste"       = lm(rdt_cub_fw_log,  data = subset(df, df$is_closed == FALSE)),
#   "log solid waste"  = lm(rdt_cub_sfw_log, data = subset(df, df$is_closed == FALSE)),
#   "solid waste"      = lm(rdt_cub_sfw_log, data = subset(df, df$is_closed == FALSE)),
#   "log liquid waste" = lm(rdt_cub_lfw_log, data = subset(df, df$is_closed == FALSE)),
#   "liquid waste"     = lm(rdt_cub_lfw_log, data = subset(df, df$is_closed == FALSE))
# )
log_fw_rdt_mult_cubic <- 
  log_food_waste_kg ~ D_01 * t_01 + 
                      temp_c + humi_p + prcp_mm + 
                      liquors + sales + halfs +
                      tueE + wedE+ thuE+ friE+ satE+
                      I(t_01^2) + I(t_01^3)
log_sfw_rdt_mult_cubic <- 
  log_solid_waste_kg ~ D_01 * t_01 + 
                       temp_c + humi_p + prcp_mm + 
                       liquors + sales + halfs +
                       tueE + wedE+ thuE+ friE+ satE+
                       I(t_01^2) + I(t_01^3)
log_lfw_rdt_mult_cubic <- 
  log_liquid_waste_kg ~ D_01 * t_01 + 
                        temp_c + humi_p + prcp_mm + 
                        liquors + sales + halfs +
                        tueE + wedE+ thuE+ friE+ satE+
                        I(t_01^2) +  I(t_01^3)

# list of log-multiple linear model with cubic time -----
log_mult_cubic_models <- list(
  "log food waste"   = lm(log_fw_rdt_mult_cubic,  data = subset(df, df$is_closed == FALSE)),
  "food waste"       = lm(log_fw_rdt_mult_cubic,  data = subset(df, df$is_closed == FALSE)),
  "log solid waste"  = lm(log_sfw_rdt_mult_cubic, data = subset(df, df$is_closed == FALSE)),
  "solid waste"      = lm(log_sfw_rdt_mult_cubic, data = subset(df, df$is_closed == FALSE)),
  "log liquid waste" = lm(log_lfw_rdt_mult_cubic, data = subset(df, df$is_closed == FALSE)),
  "liquid waste"     = lm(log_lfw_rdt_mult_cubic, data = subset(df, df$is_closed == FALSE))
)

# comparing rd and MANOVA
rdt_fw_log <- log_food_waste_kg ~ D_01 + t_01 +
  temp_c + humi_p + prcp_mm + 
  liquors + sales + halfs +
  tueE + wedE+ thuE+ friE+ satE+
  I(t_01^2) + I(t_01^3)
rdt_sfw_log <- log_solid_waste_kg ~ D_01 + t_01  +
  temp_c + humi_p + prcp_mm + 
  liquors + sales + halfs +
  tueE + wedE+ thuE+ friE+ satE+
  I(t_01^2) + I(t_01^3)
rdt_lfw_log <- log_liquid_waste_kg ~ D_01 + t_01  +
  temp_c + humi_p + prcp_mm + 
  liquors + sales + halfs +
  tueE + wedE+ thuE+ friE+ satE+
  I(t_01^2) + I(t_01^3)

comp_rdt_of <-  list(
    "RDiT: food waste"   = lm(log_fw_rdt_mult_cubic,  
                            data = subset(df, df$is_closed == FALSE)),
    "MANOVA: food waste" = lm(rdt_fw_log, data = subset(df, df$is_closed == FALSE)),
    "RDiT: solid waste"  = lm(log_sfw_rdt_mult_cubic, 
                            data = subset(df, df$is_closed == FALSE)),
    "MANOVA: solid waste"  = lm(rdt_sfw_log, data = subset(df, df$is_closed == FALSE)),
    "RDiT: liquid waste" = lm(log_lfw_rdt_mult_cubic, 
                            data = subset(df, df$is_closed == FALSE)),
    "MANOVA: liquid waste" = lm(rdt_lfw_log, data = subset(df, df$is_closed == FALSE))
  )

# modelsummary(log_mult_cubic_models, output = 'markdown')

cm <- c('D_01' = 'D',
        'D_01:t_01' = 'D × t',
        'temp_c' = 'temp. (°C)',
        'humi_p' = 'humi. (%)',
        'prcp_mm' = 'precip. (mm)',
        'liquors' = '# liquors',
        'sales' = 'sales CAD',
        'halfs' = '# halfs',
        'tueE' = 'tue',
        'wedE' = 'wed',
        'thuE' = 'thu',
        'friE' = 'fri',
        'satE' = 'sat',
        't_01' = 't',
        'I(t_01^2)' = 't^2',
        'I(t_01^3)' = 't^3')
gm <- c("r.squared",
        "adj.r.squared",
        "F",
        "p.value",
        "rmse")
# modelsummary(rdt_cub_fw_log,
#              estimate = "{estimate}{stars}",
#              statistic = c("s.e. = {std.error}"),
#              stars = c('*' = .05),
#              exponentiate = c(FALSE,TRUE,FALSE,TRUE,FALSE,TRUE),
#              coef_omit = "Intercept",
#              coef_map =  cm,
#              gof_map = gm,
#              title = "Estimated Coefficients of RD in Time using log-transformed food waste data and Cubic Time Flexibility",
#              output = 'markdown')

modelsummary(log_mult_cubic_models,
             estimate = "{estimate}{stars}",
             statistic = c("s.e. = {std.error}"),
             stars = c('*' = .05),
             exponentiate = c(FALSE,TRUE,FALSE,TRUE,FALSE,TRUE),
             coef_omit = "Intercept",
             coef_map =  cm,
             gof_map = gm,
             title = "Estimated Coefficients of RD in Time using log-transformed food waste data and Cubic Time Flexibility",
             output = 'markdown')

modelsummary(comp_rdt_of,
             estimate = "{estimate}{stars}",
             statistic = c("s.e. = {std.error}"),
             stars = c('*' = .05),
             exponentiate = c(T,T,T,T,T,T),
             coef_omit = "Intercept",
             coef_map =  cm,
             gof_map = gm,
             title = "Comparison of RD in Time and MANCOVA estimates",
             output = 'latex')

modelplot(log_mult_cubic_models[c(1,3,5)]) # log food waste
modelplot(log_mult_cubic_models[c(2,4,6)]) # food waste

#load car package
library(car)

#perform Durbin-Watson test
durbinWatsonTest(log_mult_cubic_models$`log food waste`)
durbinWatsonTest(log_mult_cubic_models$`log solid waste`)
durbinWatsonTest(log_mult_cubic_models$`log liquid waste`)               
                 

