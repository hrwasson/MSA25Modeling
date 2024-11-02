# Objective of the analysis:
# - Analyze restaurant order data to explore item popularity, identify frequent item combinations, and analyze wine and entree orders.
# - Use association rule mining to uncover patterns in customer orders, with a focus on wine items.

# Load required libraries
library(tidyverse) # For data manipulation and visualization
library(datasets)
library(fpp3) # For forecasting tools (if needed)
library(ggplot2) # For plotting
library(fabletools)
library(arules) # For association rule mining

head(menu)

# List unique items in the menu dataset
unique(menu$item)

# Summarize total orders for selected wine items
wine <- menu %>%
  filter(item %in% c('Innocent Bystander Sauvignon Blanc', "Duckhorn Chardonnay", "Helben Blanc", 
                     "Adelsheim Pinot Noir", "Total Recall Chardonnay", "Single Vineyard Malbec", 
                     "Cantina Pinot Bianco", "Blackstone Merlot", "Brancott Pinot Grigio", 
                     "Echeverria Gran Syrah")) %>%
  mutate(individual_order = 1) %>%
  summarise(wine_orders = sum(individual_order))

wine

# Summarize total orders for selected meat items (excluding fish)
meat <- menu %>%
  filter(item %in% c("Pork Chop", "Filet Mignon", "Roast Chicken", "Pork Tenderloin", "Duck Breast")) %>%
  mutate(individual_order = 1) %>%
  summarise(meat_orders = sum(individual_order))

meat

# Check for missing values in the menu data
sum(is.na(menu))

# Create a unique identifier for each order and seat
menu <- menu %>%
  mutate(id = paste(orderNo, seatNo, sep = '-'))
head(menu)

# Convert menu data into transactions format for association rule mining
menu.data <- as(split(menu$item, menu$id), "transactions")
menu.data@itemInfo$labels # Display labels of items in transactions
inspect(menu.data) # Inspect transaction data

# Plot the item frequency for the top 5 items
itemFrequencyPlot(menu.data, topN = 5, type = "absolute")

# Generate association rules with minimum support and confidence thresholds
rules <- apriori(menu.data, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))

# Filter and sort rules by confidence
filtered <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(filtered)

# Define a set of wine orders and create association rules targeting these items as RHS
wine_orders <- c('Innocent Bystander Sauvignon Blanc', "Duckhorn Chardonnay", "Helben Blanc", 
                 "Adelsheim Pinot Noir", "Total Recall Chardonnay", "Single Vineyard Malbec", 
                 "Cantina Pinot Bianco", "Blackstone Merlot", "Brancott Pinot Grigio", 
                 "Echeverria Gran Syrah")

# Generate association rules with wine items on the RHS, applying higher confidence threshold
wine_rules <- apriori(menu.data, parameter = list(supp = 0.01, conf = 0.15), 
                      appearance = list(default = "lhs", rhs = wine_orders))

# Filter and sort wine-specific rules by confidence
filtered_wine <- sort(wine_rules, by = "confidence", decreasing = TRUE)
inspect(filtered_wine)
