library(ggplot2)
library(lmtest)
library(MASS)
library(car)


rent <- read.csv("AnnArbor.csv")
View(rent)

summary(rent)
sum(is.na(rent))


ggplot(rent, aes(x = Sqft, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Rent vs Square Footage") +
  xlab("Square Footage") +
  ylab("Monthly Rent ($)") +
  theme_minimal()

ggplot(rent, aes(x = Beds, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Rent vs Bedrooms") +
  xlab("Bedrooms") +
  ylab("Monthly Rent ($)") +
  theme_minimal()

ggplot(rent, aes(x = Baths, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Rent vs Bathrooms") +
  xlab("Bathrooms") +
  ylab("Monthly Rent ($)") +
  theme_minimal()


model_rent <- lm(Rent ~ Sqft + Beds + Baths, data = rent)
summary(model_rent)

new_rental <- data.frame(
  Sqft = 1600,
  Beds = 3,
  Baths = 2
)

predicted_rent <- predict(model_rent, newdata = new_rental)
predicted_rent

