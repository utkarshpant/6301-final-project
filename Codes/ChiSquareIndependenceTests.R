df <- read.csv("heart.csv")

# Create a contingency table
contable <- table(df$output, df$sex)

# Display the contingency table
print(contable)

# Perform the chi-square test for independence
result <- chisq.test(contable)

# Print the result
print(result)

#Repeat
contable <- table(df$output, df$cp)
print(contable)
result <- chisq.test(contable)
print(result)

#Repeat
contable <- table(df$output, df$fbs)
print(contable)
result <- chisq.test(contable)
print(result)

#Repeat
contable <- table(df$output, df$restecg)
print(contable)
result <- chisq.test(contable)
print(result)

#Repeat
contable <- table(df$output, df$exng)
print(contable)
result <- chisq.test(contable)
print(result)

#Repeat
contable <- table(df$output, df$slp)
print(contable)
result <- chisq.test(contable)
print(result)

#Repeat
contable <- table(df$output, df$caa)
print(contable)
result <- chisq.test(contable)
print(result)

#Repeat
contable <- table(df$output, df$thall)
print(contable)
result <- chisq.test(contable)
print(result)