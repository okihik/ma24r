# number of days
n = 365


# intervention at day
interven = 200

# time index from 1 to 365
time = c(1:n)

# treatment variable: before internvation = day 1 to 200, 
# after intervention = day 201 to 365
treatment = c(rep(0, interven), rep(1, n - interven))

# time since treatment
timesincetreat = c(rep(0, interven), c(1:(n - interven)))

# outcome
outcome = 10 + 15 * time + 20 * treatment + 
  25 * timesincetreat + rnorm(n, mean = 0, sd = 1)

df_tsrdd = data.frame(outcome, time, treatment, timesincetreat)

head(df_tsrdd, 10)
#>      outcome time treatment timesincetreat
#> 1   25.27547    1         0              0
#> 2   38.20899    2         0              0
#> 3   54.99056    3         0              0
#> 4   70.24955    4         0              0
#> 5   85.03771    5         0              0
#> 6   99.96599    6         0              0
#> 7  114.12969    7         0              0
#> 8  130.88334    8         0              0
#> 9  145.49623    9         0              0
#> 10 160.08668   10         0              0


plot(df_tsrdd$time, df_tsrdd$outcome)

# intervention date
abline(v = interven, col = "blue")

# regression line
ts <- lm(outcome ~ time + treatment + timesincetreat, data = df_tsrdd)
lines(df_tsrdd$time, df_tsrdd$fitted.values, col = "red")

summary(ts)
