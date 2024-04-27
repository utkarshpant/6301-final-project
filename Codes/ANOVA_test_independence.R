library(ggplot2)
library(dplyr)
library(ggpubr)
library(psych)
library(ggcorrplot)


setwd("C:/Users/soham/Documents/Soham/Assignments/6301/6301-final-project/dataset/")
data_heart <- read.csv('../dataset/heart.csv')

#Null Hypothesis: There isn't any significant difference between age & output

model <- aov(data_heart$output ~ data_heart$age, data= data_heart)

summary(model)


#Null Hypothesis: There isn't any significant difference between trtbps & output

model <- aov(data_heart$output ~ data_heart$trtbps, data= data_heart)

summary(model)


#Null Hypothesis: There isn't any significant difference between chol & output

model <- aov(data_heart$output ~ data_heart$chol, data= data_heart)

summary(model)


#Null Hypothesis: There isn't any significant difference between thalachh & output

model <- aov(data_heart$output ~ data_heart$thalachh, data= data_heart)

summary(model)

#Null Hypothesis: There isn't any significant difference between oldpeak & output

model <- aov(data_heart$output ~ data_heart$oldpeak, data= data_heart)

summary(model)


str(data_heart)


summary(data_heart)

describe(data_heart)

# Correlation matrix
correlation_matrix <- cor(data_heart)

# Visualization of correlation matrix
ggcorrplot(correlation_matrix, 
           type ="lower",
           lab = "TRUE",
           lab_size = 3.25,
           method = "square",
           colors =c("blue","white", "red"),
           title = "correlation matrix")

#Variables to exclude
exclude_vars <- c("sex", "cp", "restecg", "exng", "slp", "caa", 
                  "thall", "fbs", "output")

#new_data <- data_heart[, !names(data_heart) %in% exclude_vars]

print(new_data)

#Outlier Detection
# Create a list to store outliers
outliers_list <- list()

# Loop through each column in the dataframe
for (col in names(data_heart)) {
 
   # Exclude non-numeric columns and categorical variables
  if (!(col %in% exclude_vars) && is.numeric(data_heart[[col]])) {
   
     # Calculate quartiles
    Q1 <- quantile(data_heart[[col]], 0.25)
    Q3 <- quantile(data_heart[[col]], 0.75)
    
    # Calculate IQR
    IQR <- Q3 - Q1
    
    # Find upper and lower bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Identify outliers
    outliers <- data_heart[[col]][data_heart[[col]] < lower_bound | data_heart[[col]] > upper_bound]
    
    # Store outliers in the list
    outliers_list[[col]] <- outliers
   
     # Create boxplot
    boxplot(data_heart[[col]], main = paste("Boxplot of", col), 
            xlab = col, outline = TRUE, horizontal = TRUE)
  }
}

# Print outliers
for (i in 1:length(outliers_list)) {
  if (length(outliers_list[[i]]) > 0) {
    print(paste("Outliers in", names(outliers_list)[i], ":"))
    print(outliers_list[[i]])
  } else {
    print(paste("No outliers detected in", names(outliers_list)[i]))
  }
}

# Install and load the cowplot package
install.packages("cowplot")
library(cowplot)

# Create a list to store boxplots
boxplot_list <- list()

# Loop through each column in the dataframe
for (col in names(data_heart)) {
  # Exclude non-numeric columns
  if (is.numeric(data_heart[[col]])) {
    # Create boxplot
    p <- ggplot(data_heart, aes_string(x = col, y = col)) +
      geom_boxplot() +
      labs(title = paste(col), x = col, y = col) +
      theme_minimal()
    # Store boxplot in the list
    boxplot_list[[col]] <- p
  }
}

# Arrange boxplots in a grid
plot_grid(plotlist = boxplot_list, ncol = 7)

#Outlier Removal
# Loop through each column in the dataframe
for (col in names(data_heart)) {
  # Exclude non-numeric columns
  if (is.numeric(new_data[[col]])) {
    # Calculate quartiles
    Q1 <- quantile(data_heart[[col]], 0.25)
    Q3 <- quantile(data_heart[[col]], 0.75)
    # Calculate IQR
    IQR <- Q3 - Q1
    # Find lower bound for outliers
    lower_bound <- Q1 - 1.5 * IQR
    #Find the upper bound for outliers
    upper_bound <- Q3 + 1.5 * IQR
    # Impute outliers
    data_heart[[col]][data_heart[[col]] < lower_bound] <- lower_bound
    data_heart[[col]][data_heart[[col]] > upper_bound] <- upper_bound
  }
}

#Checking for removed outliers
# Loop through each column in the dataframe
for (col in names(data_heart)) {
  
  # Exclude non-numeric columns
  if (is.numeric(data_heart[[col]])) {
    
    # Calculate quartiles
    Q1 <- quantile(data_heart[[col]], 0.25)
    Q3 <- quantile(data_heart[[col]], 0.75)
    
    # Calculate IQR
    IQR <- Q3 - Q1
    
    # Find upper and lower bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Identify outliers
    outliers <- data_heart[[col]][data_heart[[col]] < lower_bound | data_heart[[col]] > upper_bound]
    
    # Store outliers in the list
    outliers_list[[col]] <- outliers
    
    # Create boxplot
    boxplot(data_heart[[col]], main = paste("Boxplot of", col), 
            xlab = col, outline = TRUE, horizontal = TRUE)
  }
}

# Print outliers
for (i in 1:length(outliers_list)) {
  if (length(outliers_list[[i]]) > 0) {
    print(paste("Outliers in", names(outliers_list)[i], ":"))
    print(outliers_list[[i]])
  } else {
    print(paste("No outliers detected in", names(outliers_list)[i]))
  }
}


#Code for fixed data boxplots
# Create a list to store boxplots
boxplot_list <- list()

# Loop through each column in the dataframe
for (col in names(data_heart)) {
  # Exclude non-numeric columns
  if (is.numeric(data_heart[[col]])) {
    # Create boxplot
    p <- ggplot(data_heart, aes_string(x = col, y = col)) +
      geom_boxplot() +
      labs(title = paste(col), x = col, y = col) +
      theme_minimal()
    # Store boxplot in the list
    boxplot_list[[col]] <- p
  }
}

# Arrange boxplots in a grid
plot_grid(plotlist = boxplot_list, ncol = 7)

print(data_heart)
