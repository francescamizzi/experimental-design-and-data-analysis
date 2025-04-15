library(MASS)




# Model summary
summary(poisson_model)

# Check for overdispersion
dispersion <- sum(residuals(poisson_model, type="pearson")^2) / poisson_model$df.residual
dispersion

library(ggplot2)
ggplot(coups, aes(x = miltcoup)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", alpha = 0.6, color = "black") +
  stat_function(fun = dpois, args = list(lambda = mean(coups$miltcoup)), 
                color = "red", size = 1.2, linetype = "dashed") +
  labs(title = "Histogram of Military Coups",
       x = "Number of Military Coups",
       y = "Density") +
  theme_minimal()

full_model <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + 
                    popn + size + numelec + numregim, 
                  family = poisson, data = coups)
summary(full_model)

reduced_model1 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + 
                        popn + size + numregim, 
                      family = poisson, data = coups)
summary(reduced_model1)

reduced_model2 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + popn + size, 
                      family = poisson, data = coups)
summary(reduced_model2)

reduced_model3 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + popn, 
                      family = poisson, data = coups)
summary(reduced_model3)

reduced_model4 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote, 
                      family = poisson, data = coups)
summary(reduced_model4)

reduced_model5 <- glm(miltcoup ~ oligarchy + pollib + parties, 
                      family = poisson, data = coups)
summary(reduced_model5)

anova(full_model, reduced_model5, test = "Chisq")

# Compute mean values of all numerical predictors except pollib
mean_oligarchy <- mean(coups$oligarchy)
mean_parties <- mean(coups$parties)

# Create a data frame with three levels of pollib and mean values for other variables
new_data <- data.frame(
  oligarchy = rep(mean_oligarchy, 3),
  pollib = c(0, 1, 2),  # Political liberalization levels
  parties = rep(mean_parties, 3)
)

# Predict number of coups using the reduced model
new_data$predicted_coups <- predict(reduced_model, newdata = new_data, type = "response")

# Display results
print(new_data)

