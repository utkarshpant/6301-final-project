data_heart <- read.csv("heart.csv")
df <- read.csv("heart.csv")

#Variables to exclude
exclude_vars <- c("sex", "cp", "restecg", "exng", "slp", "caa", 
                  "thall", "fbs", "output")

new_data <- data_heart[, !names(data_heart) %in% exclude_vars]

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

identical(df, data_heart)
df <- data_heart



#Set train and test data
set.seed(1)
bound <- floor((nrow(df)/5)*4)         #define % of training and test set(80-20)

df <- df[sample(nrow(df)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

#11 Features Logistic Regression
result11 <- glm(output ~ age+sex+cp+trtbps+restecg+thalachh+exng+oldpeak+slp+caa+thall,
              family = binomial, data = df.train)
summary(result11)
predprob <- predict.glm(result11, newdata = df.test, type = 'response')
predclass <- ifelse(predprob >= 0.5, 1, 0)

# Create a confusion matrix
conf_matrix11 <- table(df.test$output, predclass)
print(conf_matrix11)
# Accuracy
accuracy11 <- sum(diag(conf_matrix11)) / sum(conf_matrix11)
# Sensitivity (True Positive Rate)
sensitivity11 <- conf_matrix11[2, 2] / sum(conf_matrix11[2, ])
# Specificity (True Negative Rate)
specificity11 <- conf_matrix11[1, 1] / sum(conf_matrix11[1, ])
cat("Accuracy:", accuracy11, "Sensitivity:", 
    sensitivity11, "Specificity:", 
    specificity11, "\n")




#Remove age (10 features)
result10 <- glm(output ~ sex+cp+trtbps+restecg+thalachh+exng+oldpeak+slp+caa+thall,
              family = binomial, data = df.train)
summary(result10)
predprob <- predict.glm(result10, newdata = df.test, type = 'response')
predclass <- ifelse(predprob >= 0.5, 1, 0)

# Create a confusion matrix
conf_matrix10 <- table(df.test$output, predclass)
print(conf_matrix10)
# Accuracy
accuracy10 <- sum(diag(conf_matrix10)) / sum(conf_matrix10)
# Sensitivity (True Positive Rate)
sensitivity10 <- conf_matrix10[2, 2] / sum(conf_matrix10[2, ])
# Specificity (True Negative Rate)
specificity10 <- conf_matrix10[1, 1] / sum(conf_matrix10[1, ])
cat("Accuracy:", accuracy10, "Sensitivity:", 
    sensitivity10, "Specificity:", 
    specificity10, "\n")

#Remove restecg (9 features)
result9 <- glm(output ~ sex+cp+trtbps+thalachh+exng+oldpeak+slp+caa+thall,
              family = binomial, data = df.train)
summary(result9)
predprob <- predict.glm(result9, newdata = df.test, type = 'response')
predclass <- ifelse(predprob >= 0.5, 1, 0)

# Create a confusion matrix
conf_matrix9 <- table(df.test$output, predclass)
print(conf_matrix9)
# Accuracy
accuracy9 <- sum(diag(conf_matrix9)) / sum(conf_matrix9)
# Sensitivity (True Positive Rate)
sensitivity9 <- conf_matrix9[2, 2] / sum(conf_matrix9[2, ])
# Specificity (True Negative Rate)
specificity9 <- conf_matrix9[1, 1] / sum(conf_matrix9[1, ])
cat("Accuracy:", accuracy9, "Sensitivity:", 
    sensitivity9, "Specificity:", 
    specificity9, "\n")

#Remove exng (8 features)
result8 <- glm(output ~ sex+cp+trtbps+thalachh+oldpeak+slp+caa+thall,
              family = binomial, data = df.train)
summary(result8)
predprob <- predict.glm(result8, newdata = df.test, type = 'response')
predclass <- ifelse(predprob >= 0.5, 1, 0)

# Create a confusion matrix
conf_matrix8 <- table(df.test$output, predclass)
print(conf_matrix8)
# Accuracy
accuracy8 <- sum(diag(conf_matrix8)) / sum(conf_matrix8)
# Sensitivity (True Positive Rate)
sensitivity8 <- conf_matrix8[2, 2] / sum(conf_matrix8[2, ])
# Specificity (True Negative Rate)
specificity8 <- conf_matrix8[1, 1] / sum(conf_matrix8[1, ])
cat("Accuracy:", accuracy8, "Sensitivity:", 
    sensitivity8, "Specificity:", 
    specificity8, "\n")


#Remove oldpeak (7 features)
result7 <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall,
              family = binomial, data = df.train)
summary(result7)
predprob <- predict.glm(result7, newdata = df.test, type = 'response')
predclass <- ifelse(predprob >= 0.5, 1, 0)

# Create a confusion matrix
conf_matrix7 <- table(df.test$output, predclass)
print(conf_matrix7)
# Accuracy
accuracy7 <- sum(diag(conf_matrix7)) / sum(conf_matrix7)
# Sensitivity (True Positive Rate)
sensitivity7 <- conf_matrix7[2, 2] / sum(conf_matrix7[2, ])
# Specificity (True Negative Rate)
specificity7 <- conf_matrix7[1, 1] / sum(conf_matrix7[1, ])
cat("Accuracy:", accuracy7, "Sensitivity:", 
    sensitivity7, "Specificity:", 
    specificity7, "\n")


#LRT GOF Tests for Models with 7,8, and 9 features

#9 vs 8
lr_test9v8 <- anova(result9, result8, test = "Chisq")
print(lr_test9v8)

#8 vs 7
lr_test8v7 <- anova(result8, result7, test = "Chisq")
print(lr_test8v7)

#7 vs most basic model
result0 <- glm(output ~ 1,
                family = binomial, data = df.train)
lr_test7v0 <- anova(result7, result0, test = "Chisq")
print(lr_test7v0)

#adding interaction terms to 7
#thalachh*slp
result7a <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+thalachh*slp,
                family = binomial, data = df.train)
lr_test7av7 <- anova(result7a, result7, test = "Chisq")
print(lr_test7av7)

#thalachh*cp
result7b <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+thalachh*cp,
                family = binomial, data = df.train)
lr_test7bv7 <- anova(result7b, result7, test = "Chisq")
print(lr_test7bv7)

#thalachh*caa
result7c <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+thalachh*caa,
                family = binomial, data = df.train)
lr_test7cv7 <- anova(result7c, result7, test = "Chisq")
print(lr_test7cv7)

#thall*sex
result7d <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+thall*sex,
                family = binomial, data = df.train)
lr_test7dv7 <- anova(result7d, result7, test = "Chisq")
print(lr_test7dv7)

#reintroducing removed variables with interaction
#slp*oldpeak
result7e <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+slp*oldpeak,
                family = binomial, data = df.train)
lr_test7ev7 <- anova(result7e, result7, test = "Chisq")
print(lr_test7ev7)

#age*thalachh
result7f <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+age*thalachh,
                family = binomial, data = df.train)
lr_test7fv7 <- anova(result7f, result7, test = "Chisq")
print(lr_test7fv7)

#exng*thalachh
result7g <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+exng*thalachh,
                family = binomial, data = df.train)
lr_test7gv7 <- anova(result7g, result7, test = "Chisq")
print(lr_test7gv7)

#oldpeak*thalachh
result7h <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+oldpeak*thalachh,
                family = binomial, data = df.train)
lr_test7hv7 <- anova(result7h, result7, test = "Chisq")
print(lr_test7hv7)

#exng*cp
result7i <- glm(output ~ sex+cp+trtbps+thalachh+slp+caa+thall+exng*cp,
                family = binomial, data = df.train)
lr_test7iv7 <- anova(result7i, result7, test = "Chisq")
print(lr_test7iv7)