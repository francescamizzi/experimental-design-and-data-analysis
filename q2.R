# Load necessary libraries
library(ggplot2)

# Step 1: Load and Explore the Data
# crops_data <- read.table("crops.txt", header = TRUE)

# Check the structure and summary of the data
str(crops_data)
summary(crops_data)
head(crops_data)

# Convert County and Related to factors
crops_data$County <- as.factor(crops_data$County)
crops_data$Related <- as.factor(crops_data$Related)

# Step 2: Perform ANOVA (without Size)
anova_model <- aov(Crops ~ County * Related, data = crops_data)
summary(anova_model)

# Estimate crops for County 3 with landlord and tenant not related
new_data_anova <- data.frame(County = factor(3, levels = levels(crops_data$County)),
                             Related = factor("no", levels = levels(crops_data$Related)))
predicted_anova <- predict(anova_model, new_data_anova, se.fit = TRUE)
predicted_anova

# Step 3: Perform ANCOVA (including Size)
ancova_model <- aov(Crops ~ County * Related + Size, data = crops_data)
summary(ancova_model)

# Test interactions of Size with County and Related
ancova_model_interaction <- aov(Crops ~ County * Related + Size * County + Size * Related, data = crops_data)
summary(ancova_model_interaction)

# Choose the most appropriate model (based on significance levels)
# Assume we decide to use ancova_model (without interactions)

# Step 4: Interpret effects using visualization
ggplot(crops_data, aes(x = Size, y = Crops, color = County)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Related) +
  theme_minimal()

# Step 5: Predict Crops for County 2, Size 165, with related landlord and tenant
new_farm <- data.frame(County = factor(2, levels = levels(crops_data$County)),
                       Size = 165,
                       Related = factor("yes", levels = levels(crops_data$Related)))
predicted_crops <- predict(ancova_model, new_farm, se.fit = TRUE)
predicted_crops

# Estimate error variance
sigma_squared <- summary(ancova_model)$sigma^2
sigma_squared
