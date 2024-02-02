library(stargazer)

df_dept <- subset(df[c("food_waste_kg", "solid_waste_kg","liquid_waste_kg")],
                  df$is_closed == FALSE)

stargazer(df_dept, type = "latex", digits=2)

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
            data = df_des, add_columns = corr_des,output = 'markdown',
            title = 'Descriptive statistics and correlation matrix.',
            notes = c('N = 161.'))

