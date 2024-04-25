df <- read.csv("~/Desktop/Advanced Regression Analysis/Final Project CDA/Dataset/heart.csv")

# Display basic information about the dataset
summary(df)
str(df)



# Exploratory Data Analysis (EDA)

# Load necessary library for data manipulation and visualization
library(ggplot2)
library(tidyverse) 

# Ensure categorical variables are factors
categorical_vars <- c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)


# Plotting bar charts for categorical variables
df %>%
  select(all_of(categorical_vars)) %>%
  gather(key = "Variables", value = "Categories") %>%
  ggplot(aes(x = Categories, fill = Variables)) +
  geom_bar() +
  facet_wrap(~Variables, scales = "free") +
  theme_minimal() +
  labs(title = "Bar Charts for Categorical Variables", x = "Categories", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plotting histograms for continuous variables
df %>%
  select(age, trtbps, chol, thalachh, oldpeak) %>%
  gather(key = "Variables", value = "Values") %>%
  ggplot(aes(x = Values, fill = Variables)) +
  geom_histogram(bins = 30, alpha = 0.6) +
  facet_wrap(~Variables, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms for Continuous Variables", x = "Values", y = "Frequency") +
  scale_fill_brewer(palette = "Pastel1")  # Coloring for clarity






# Create bar charts for each variable with the response variable
categorical_vars <- c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall")
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)
df$output <- as.factor(df$output)
levels(df$output) <- c("Low Risk", "High Risk")
# Melt data to long format for easier plotting with ggplot2
df_long <- pivot_longer(df, cols = categorical_vars)

# Create bar charts
ggplot(df_long, aes(x = value, fill = output)) +
  geom_bar(position = "fill") +
  facet_wrap(~name, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution of Categorical Variables by Heart Attack Risk",
       x = "Category",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")




