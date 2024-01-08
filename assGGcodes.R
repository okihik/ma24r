library(ggpubr)
library(car)
model_res <- tibble(Fitted.values = fitted(rdt_fw), 
                    Residuals = residuals(rdt_fw),
                    Obs.values = rdt_fw$model[[1]],
                    cd_pos = sqrt(0.5*length(coef(rdt_fw))*(1-hatvalues(rdt_fw))/hatvalues(rdt_fw)),
                    cd_neg = -cd_pos,
                    tr.hat.matrix = hatvalues(rdt_fw),
                    sigma = summary(rdt_fw)$sigma)
# 1. Linearity of the relationships between the dependent and independent variables
# 1.1 plot residual vs fitted values         
model_res %>% 
  ggplot(aes(x = Fitted.values, y = Residuals)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  geom_hline(yintercept=0, linetype="dashed") +
  ggtitle("Linearity: Food Waste")
# 1.2 plot fitted values vs obs values
model_res %>% 
  ggplot(aes(x = Obs.values, y = Fitted.values)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  geom_hline(yintercept = mean(model_res$Obs.values), linetype="dashed") +
  ggtitle("Linearity: Food Waste")

# 2. Normality of the residuals
# 2.1 histogram of residuals
model_res %>% 
  ggplot(aes(x = Residuals)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, colour = 1, fill = "white") +
  geom_density(linewidth = 1.5, colour = 4, fill = 4, alpha = 0.15) +
  ggtitle("Normality: Food Waste")

# 3.2 QQ plot 
model_res %>% 
  ggqqplot(x = "Residuals") +
  ggtitle("QQ Plot: Food Waste")

# 2.3 shapiro-wilk normality test
shapiro.test(model_res$Residuals) # less than 0.05 -> not normal dist.

# 3. Homoscedasticity of the residuals
# 3.1 plot residuals
library(plotrix)
model_res %>% 
  ggplot(aes(x = Fitted.values,
             y = sqrt(abs(Residuals)/(sigma^2*(1-tr.hat.matrix))))) +
  geom_point() +
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  ggtitle("Homoscedasticity: Food Waste")

# 3.2 Breusch-Pagan test
lmtest::bptest(rdt_fw)

# 4. No influential points (outliers)
model_res %>% 
  ggplot(aes(x = tr.hat.matrix,
             y = Residuals/(sigma*(1-tr.hat.matrix)))) +
  geom_point() +
  geom_line(aes(x = tr.hat.matrix,y=cd_pos), lty = 2, colour = "red") +
  geom_line(aes(x = tr.hat.matrix,y=cd_neg), lty = 2, colour = "red") +
  expand_limits(x = 0, y = 0) + 
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  ggtitle("Outliers: Food Waste")

# 5. No multicollinearity


# 6. Independence of the observations
# Autocorrelation
