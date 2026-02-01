library(ggplot2)
library(lmtest)
library(MASS)
library(car)

wages <- read.csv("wages.csv")

View(wages)

summary(wages)            
sum(is.na(wages)) 

ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.5) +
  ggtitle("Age vs Hourly Wage") +
  xlab("Age") +
  ylab("Hourly Wage ($)") +
  theme_minimal()

ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),
    se = FALSE,
    color = "pink"
  ) +
  labs(
    title = "Wage vs Age: Linear (Blue) vs Quadratic (Red)",
    x = "Age",
    y = "Hourly Wage"
  )

model_simple <- lm(Wage ~ Age + Educ, data = wages)
summary(model_simple)


par(mfrow = c(1, 2))
plot(model_linear$fitted.values, model_linear$residuals,
     main = "Residuals vs Fitted (Linear Model)",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "purple", lty = 2)

hist(model_linear$residuals, main = "Histogram of Residuals (Linear Model)",
     xlab = "Residuals", breaks = 30)

wages$Age2 <- wages$Age^2

model_quad <- lm(Wage ~ Age + Age2 + Educ, data = wages)
summary(model_quad)


par(mfrow = c(1, 2))

plot(model_simple$fitted.values, model_simple$residuals,
     main = "Simple Model: Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(model_simple$residuals,
     main = "Simple Model: Histogram of Residuals",
     xlab = "Residuals", breaks = 20)

plot(model_quad$fitted.values, model_quad$residuals,
     main = "Quadratic Model: Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(model_quad$residuals,
     main = "Quadratic Model: Histogram of Residuals",
     xlab = "Residuals", breaks = 20)

#The quadratic model provides a better fit than the simple model, as evidenced by higher R-squared values and residuals that are more randomly distributed and approximately normal.


new_workers <- data.frame(
  Age = c(30, 50, 70),
  Age2 = c(30^2, 50^2, 70^2),
  Educ = c(16, 16, 16)
)

predicted_wages <- predict(model_quad, newdata = new_workers)
summary(predicted_wages)

beta_age <- coef(model_quad)["Age"]
beta_age2 <- coef(model_quad)["Age2"]

max_wage_age <- -beta_age / (2 * beta_age2)
summary(max_wage_age)


