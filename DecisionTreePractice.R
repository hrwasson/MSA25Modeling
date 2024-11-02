# Objective of the analysis:
# - Develop a decision tree model to predict the likelihood of customers purchasing a variable rate annuity for a bank.
# - Compare decision tree model accuracy with a previously created logistic regression model.
# - Visualize and interpret the final decision tree and feature importance.

# Load required libraries
library(tidyverse) # For data manipulation and visualization
library(rpart) # For decision tree modeling
library(ggplot2) # For plotting
library(rpart.plot) # For visualizing decision trees

# Check the first few rows of the training data and check for missing values
head(train)
sum(is.na(train))

# Create a decision tree model using Gini impurity
BC.tree <- rpart(INS ~ ., data = train, method = 'class', parms = list(split = 'gini'))
print(BC.tree)

# Create another decision tree model using information gain (entropy)
BC.tree1 <- rpart(INS ~ ., data = train, method = 'class', parms = list(split = 'information'))
print(BC.tree1)

# Visualize feature importance for Gini decision tree model
varimp.data <- data.frame(BC.tree$variable.importance)
varimp.data$names <- as.character(rownames(varimp.data))

ggplot(data = varimp.data, aes(x = names, y = BC.tree.variable.importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable Name", y = "Variable Importance", title = "Gini Decision Tree Feature Importance")

# Visualize feature importance for information gain decision tree model
varimp.data1 <- data.frame(BC.tree1$variable.importance)
varimp.data1$names <- as.character(rownames(varimp.data1))

ggplot(data = varimp.data1, aes(x = names, y = BC.tree1.variable.importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable Name", y = "Variable Importance", title = "Information Gain Decision Tree Feature Importance")

# Calculate misclassification rates for training and validation sets with the Gini decision tree model
tscores <- predict(BC.tree, type = 'class') # Predict on training set
scores <- predict(BC.tree, validation, type = 'class') # Predict on validation set

# Print misclassification rates for Gini model
print(paste("Gini Training Misclassification Rate:", sum(tscores != train$INS) / nrow(train)))
print(paste("Gini Validation Misclassification Rate:", sum(scores != validation$INS) / nrow(validation)))

# Calculate misclassification rates for training and validation sets with the information gain decision tree model
tscores1 <- predict(BC.tree1, type = 'class')
scores1 <- predict(BC.tree1, validation, type = 'class')

# Print misclassification rates for information gain model
print(paste("Information Training Misclassification Rate:", sum(tscores1 != train$INS) / nrow(train)))
print(paste("Information Validation Misclassification Rate:", sum(scores1 != validation$INS) / nrow(validation)))

# Plot the Gini decision tree model
rpart.plot(BC.tree)

# Print accuracy for decision tree model based on misclassification rate
print(paste("Accuracy (Decision Tree):", 1 - 0.2834275))

# Logistic regression model (for comparison)

# Fit logistic regression model
f.model <- glm(INS ~ DDA + NSF + IRA + INV + MTG + CC + DDABAL_BIN + CHECKS_BIN + 
                 TELLER_BIN + SAVBAL_BIN + ATMAMT_BIN + CDBAL_BIN + ILSBAL_BIN + 
                 MMBAL_BIN + DDA:IRA, data = train, family = binomial(link = "logit"))

# Display summary of logistic regression model
summary(f.model)

# Print misclassification rate for logistic regression model
print(paste("Misclassification (Logistic Regression):", 1 - 0.7020))
