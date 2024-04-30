#Reading the dataset and storing in a data frame
sustainability <- read.csv('/users/rajatchelani/Downloads/sustainable_development_report_2023.csv')
str(sustainability)

#Loading the required libraries
library(reshape)
library(ggplot2)
library(dplyr)
library(tidyverse)

#Histogram of overall scores
# Calculate the counts within each bin
counts <- as.data.frame(table(cut(sustainability$overall_score, breaks = seq(0, 100, by = 5))))
# Rename the columns for clarity
colnames(counts) <- c("Bin", "Count")
# Filter out bins with a count of 0
counts <- counts[counts$Count > 0, ]
# Create the histogram with frequency numbers
ggplot(counts, aes(x = Bin, y = Count)) +
  geom_bar(stat = "identity", fill = "#1984c5", color = "#c23728") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3, color = "#c23728") +
  labs(title = "Histogram of Overall Sustainability Scores",
       x = "Overall SDG Index Score",
       y = "Frequency") +
  theme_minimal()


#Boxplot of overall_scores
ggplot(sustainability, aes(y = overall_score)) +
  geom_boxplot(fill = "#c23728", color = "#1984c5") +
  labs(title = "Boxplot of Overall Sustainability Scores",
       y = "Overall SDG Index Score") +
  theme_minimal()


#Density plot to visualize the distribution of overall sustainability scores by region
# Define custom colors for each region
custom_colors <- c("#1984c5", "#22a7f0", "#63bff0", "#a7d5ed", "#e2e2e2", "#e14b31", "#c23728")
ggplot(sustainability, aes(x = overall_score, fill = region)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Distribution of Overall Sustainability Scores by Region",
    x = "Overall Score"
  ) +
  scale_fill_manual(name = "Region", values = setNames(custom_colors, levels(sustainability$region))) +
  theme_minimal()


#box plot for each goal's scores
# Calculate the average goal scores for each goal
avg_goal_scores <- sapply(sustainability[, 5:21], function(x) mean(x, na.rm = TRUE))
# Sort the average scores in decreasing order and get the corresponding order
sorted_order <- order(avg_goal_scores, decreasing = TRUE)
sorted_scores <- avg_goal_scores[sorted_order]
# Define a color gradient from darkest to lightest (you can adjust these colors)
color_gradient <- colorRampPalette(c("#1984c5", "#c23728"))
# Create a vector of colors using the defined gradient
colors <- color_gradient(length(sorted_scores))
# Create a box plot for the sorted scores
boxplot(sustainability[, sorted_order + 4],  # +4 to account for the non-goal_score columns
        names = paste("Goal", 1:17)[sorted_order],
        main = "Average Goal Scores (Sorted)",
        xlab = "SDG Goals",
        ylab = "Score",
        col = colors,
        border = "black")


# Identify the top 5 and bottom 5 countries based on overall score
top_5_countries <- sustainability %>%
  arrange(desc(overall_score)) %>%
  head(5)

bottom_5_countries <- sustainability %>%
  arrange(overall_score) %>%
  head(5)

# Create a circular bar chart for the top 5 countries
ggplot(top_5_countries, aes(x = country, y = overall_score, fill = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", overall_score)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "x", start = 0) +
  labs(
    title = "Top 5 Countries - Overall Sustainability Score",
    x = NULL,  # Remove x-axis label
    y = "Overall Score",
    fill = "Country"
  ) +
  scale_fill_manual(values = c("#1984c5", "#22a7f0", "#63bff0", "#a9d1ed", "#a1d9ed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a circular bar chart for the bottom 5 countries
ggplot(bottom_5_countries, aes(x = country, y = overall_score, fill = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", overall_score)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "x", start = 0) +
  labs(
    title = "Bottom 5 Countries - Overall Sustainability Score",
    x = NULL,  # Remove x-axis label
    y = "Overall Score",
    fill = "Country"
  ) +
  scale_fill_manual(values = c("#de4e56", "#e1a692", "#de6e56", "#e14b31", "#c23728")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create new variables for goal categories
sustainability$Social_Wellbeing <- rowSums(sustainability[, c("goal_1_score", "goal_2_score", "goal_3_score", "goal_4_score", "goal_5_score")], na.rm = TRUE)
sustainability$Environmental_Sustainability <- rowSums(sustainability[, c("goal_6_score", "goal_7_score", "goal_12_score", "goal_13_score", "goal_14_score", "goal_15_score")], na.rm = TRUE)
sustainability$Economic_Growth_Equality <- rowSums(sustainability[, c("goal_8_score", "goal_9_score", "goal_10_score")], na.rm = TRUE)
sustainability$Sustainable_Communities <- sustainability$goal_11_score
sustainability$Peace_Justice_Partnerships <- rowSums(sustainability[, c("goal_16_score", "goal_17_score")], na.rm = TRUE)

# Grouped barchart for Social Well-being category (top 5)
social_wellbeing_sorted <- sustainability[order(-sustainability$Social_Wellbeing), ]
top_5_social_wellbeing <- head(social_wellbeing_sorted, 5)
# Select specified goal columns for the top 5 countries
selected_columns_social_wellbeing <- c("goal_1_score", "goal_2_score", "goal_3_score", "goal_4_score", "goal_5_score")
selected_data_social_wellbeing <- top_5_social_wellbeing[, c("country", selected_columns_social_wellbeing)]
selected_data_social_wellbeing_melted <- gather(selected_data_social_wellbeing, key = "Goal", value = "Score", -country)
ggplot(selected_data_social_wellbeing_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Top 5 Countries - Social Well-being",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#1984c5", "#22a7f0", "#63bff0", "#a9d9ed", "#a1d1ed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped barchart for Social Well-being category (bottom 5)
bottom_5_social_wellbeing <- tail(social_wellbeing_sorted, 5)
# Select specified goal columns for the bottom 5 countries
selected_data_social_wellbeing_bottom <- bottom_5_social_wellbeing[, c("country", selected_columns_social_wellbeing)]
# Plotting the grouped barchart for the bottom 5 Social Well-being countries
selected_data_social_wellbeing_bottom_melted <- gather(selected_data_social_wellbeing_bottom, key = "Goal", value = "Score", -country)
ggplot(selected_data_social_wellbeing_bottom_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Bottom 5 Countries - Social Well-being",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#de4e56", "#e1a692", "#de6e56", "#e14b31", "#c23728")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Grouped barchart for Environmental Sustainability category (top 5)
environmental_sustainability_sorted <- sustainability[order(-sustainability$Environmental_Sustainability), ]
top_5_environmental_sustainability <- head(environmental_sustainability_sorted, 5)
# Select specified goal columns for the top 5 countries
selected_columns_environmental_sustainability <- c("goal_6_score", "goal_7_score", "goal_12_score", "goal_13_score", "goal_14_score", "goal_15_score")
selected_data_environmental_sustainability <- top_5_environmental_sustainability[, c("country", selected_columns_environmental_sustainability)]
selected_data_environmental_sustainability_melted <- gather(selected_data_environmental_sustainability, key = "Goal", value = "Score", -country)
ggplot(selected_data_environmental_sustainability_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Top 5 Countries - Environmental Sustainability",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#1954c5", "#1984c5", "#22a7f0", "#63bff0", "#a7d8ed", "#a3d1ed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped barchart for Environmental Sustainability category (bottom 5)
bottom_5_environmental_sustainability <- tail(environmental_sustainability_sorted, 5)
# Select specified goal columns for the bottom 5 countries
selected_data_environmental_sustainability_bottom <- bottom_5_environmental_sustainability[, c("country", selected_columns_environmental_sustainability)]
# Plotting the grouped barchart for the bottom 5 Environmental Sustainability countries
selected_data_environmental_sustainability_bottom_melted <- gather(selected_data_environmental_sustainability_bottom, key = "Goal", value = "Score", -country)
ggplot(selected_data_environmental_sustainability_bottom_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Bottom 5 Countries - Environmental Sustainability",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#e1a692", "#de4e56", "#de5e56", "#de6e56", "#e14b31", "#c23728")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Grouped barchart for Economic Growth and Equality category (top 5)
economic_growth_equality_sorted <- sustainability[order(-sustainability$Economic_Growth_Equality), ]
top_5_economic_growth_equality <- head(economic_growth_equality_sorted, 5)
# Select specified goal columns for the top 5 countries
selected_columns_economic_growth_equality <- c("goal_8_score", "goal_9_score", "goal_10_score")
selected_data_economic_growth_equality <- top_5_economic_growth_equality[, c("country", selected_columns_economic_growth_equality)]
selected_data_economic_growth_equality_melted <- gather(selected_data_economic_growth_equality, key = "Goal", value = "Score", -country)
ggplot(selected_data_economic_growth_equality_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Top 5 Countries - Economic Growth and Equality",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#1984c5", "#22a7f0", "#63bff0")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped barchart for Economic Growth and Equality category (bottom 5)
bottom_5_economic_growth_equality <- tail(economic_growth_equality_sorted, 5)
# Select specified goal columns for the bottom 5 countries
selected_data_economic_growth_equality_bottom <- bottom_5_economic_growth_equality[, c("country", selected_columns_economic_growth_equality)]
# Plotting the grouped barchart for the bottom 5 Economic Growth and Equality countries
selected_data_economic_growth_equality_bottom_melted <- gather(selected_data_economic_growth_equality_bottom, key = "Goal", value = "Score", -country)
ggplot(selected_data_economic_growth_equality_bottom_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Bottom 5 Countries - Economic Growth and Equality",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#de6e56", "#e14b31", "#c23728")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Grouped barchart for Sustainable Communities category (top 5)
sustainable_communities_sorted <- sustainability[order(-sustainability$Sustainable_Communities), ]
top_5_sustainable_communities <- head(sustainable_communities_sorted, 5)
# Select specified goal columns for the top 5 countries
selected_columns_sustainable_communities <- c("goal_11_score")
selected_data_sustainable_communities <- top_5_sustainable_communities[, c("country", selected_columns_sustainable_communities)]
selected_data_sustainable_communities_melted <- gather(selected_data_sustainable_communities, key = "Goal", value = "Score", -country)
ggplot(selected_data_sustainable_communities_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Top 5 Countries - Sustainable Communities",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#1984c5")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped barchart for Sustainable Communities category (bottom 5)
bottom_5_sustainable_communities <- tail(sustainable_communities_sorted, 5)
# Select specified goal columns for the bottom 5 countries
selected_data_sustainable_communities_bottom <- bottom_5_sustainable_communities[, c("country", selected_columns_sustainable_communities)]
# Plotting the grouped barchart for the bottom 5 Sustainable Communities countries
selected_data_sustainable_communities_bottom_melted <- gather(selected_data_sustainable_communities_bottom, key = "Goal", value = "Score", -country)
ggplot(selected_data_sustainable_communities_bottom_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Bottom 5 Countries - Sustainable Communities",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#c23728")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Grouped barchart for Peace, Justice, and Partnerships category (top 5)
peace_justice_partnerships_sorted <- sustainability[order(-sustainability$Peace_Justice_Partnerships), ]
top_5_peace_justice_partnerships <- head(peace_justice_partnerships_sorted, 5)
# Select specified goal columns for the top 5 countries
selected_columns_peace_justice_partnerships <- c("goal_16_score", "goal_17_score")
selected_data_peace_justice_partnerships <- top_5_peace_justice_partnerships[, c("country", selected_columns_peace_justice_partnerships)]
selected_data_peace_justice_partnerships_melted <- gather(selected_data_peace_justice_partnerships, key = "Goal", value = "Score", -country)
ggplot(selected_data_peace_justice_partnerships_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Top 5 Countries - Peace, Justice, and Partnerships",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#1984c5", "#22a7f0")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped barchart for Peace, Justice, and Partnerships category (bottom 5)
bottom_5_peace_justice_partnerships <- tail(peace_justice_partnerships_sorted, 5)
# Select specified goal columns for the bottom 5 countries
selected_data_peace_justice_partnerships_bottom <- bottom_5_peace_justice_partnerships[, c("country", selected_columns_peace_justice_partnerships)]
# Plotting the grouped barchart for the bottom 5 Peace, Justice, and Partnerships countries
selected_data_peace_justice_partnerships_bottom_melted <- gather(selected_data_peace_justice_partnerships_bottom, key = "Goal", value = "Score", -country)
ggplot(selected_data_peace_justice_partnerships_bottom_melted, aes(x = country, y = Score, fill = Goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar(theta = "x", start = 0) +  # Specify polar coordinates
  labs(
    title = "Bottom 5 Countries - Peace, Justice, and Partnerships",
    x = NULL,  # Remove x-axis label
    y = "Score",
    fill = "Goal"
  ) +
  scale_fill_manual(values = c("#e14b31", "#c23728")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))